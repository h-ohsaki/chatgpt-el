;; -*- Emacs-Lisp -*-
;;
;; Interactively access AIs from Emacs with/without APIs.
;; Copyright (C) 2023-2025 Hiroyuki Ohsaki.
;; All rights reserved.
;;

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Add the following lines to ~/.emacs:
;; (autoload 'chatgpt-send "chatgpt" nil t)
;; (autoload 'chatgpt-send-api "chatgpt" nil t)
;; (autoload 'chatgpt-insert-response "chatgpt" nil t)
;; (autoload 'chatgpt-fill "chatgpt" nil t)
;; (autoload 'chatgpt-select-engine "chatgpt" nil t)
;; (global-set-key "\C-cb" 'chatgpt-send)
;; (global-set-key "\C-cq" 'chatgpt-send-api)
;; (global-set-key "\C-cQ" 'chatgpt-insert-response)
;; (global-set-key "\C-cf" 'chatgpt-fill)
;; (global-set-key "\C-cE" 'chatgpt-select-engine)
;; (setq chatgpt-engine "ChatGPT")
;; (setq chatgpt-prog "../path/to/chatgpt-el/chatgpt-cdp")

;; Usage:
;; C-c b          Send a prompt near the point.
;; C-u C-c b      Send a prompt near the point after revising the prompt in the minibuffer.
;; C-u C-u C-c b  Send a prompt near the point after selecting a prompt prefix.
;; C-c Q          Insert the latest response at the point.  
;; C-u C-c Q      Insert the pair of the latest prompt and response at the point.  
;; C-c f          Generate a context that fits at the point.
;; C-c E          Select AI engine.

(defvar chatgpt-prog "~/src/chatgpt-el/chatgpt-cdp")
;; (defvar chatgpt-prog-api "~/src/chatgpt-el/chatgpt-api")
(defvar chatgpt-prog-api "~/src/chatgpt-el/gemini-api")
(defvar chatgpt-engine "Gemini")
;; (defvar chatgpt-browser-prog "chromium")
;; (defvar chatgpt-browser-args '("--remote-debugging-port=9000"
;; 			       "--remote-allow-origins=http://127.0.0.1:9000"))
(defvar chatgpt-browser-prog "qutebrowser")
(defvar chatgpt-browser-args '("--qt-flag" "remote-debugging-port=9001"))
(defvar chatgpt-use-api nil)
(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues:")
    (?s . "Summarize the following in Japanese in a plain academic writing style:")
    (?j . "Translate the following in Japanese in a plain academic writing style:")
    (?e . "Translate the following in English in a plain academic writing style:")
    (?p . "Proofread the following and provide a list of changes made in Markdown table:")
    (?r . "Rewrite the following in a plain academic writing style:")
    (?R . "Refactor the code starting from ----.  Do not delete any comment.  Add a short docstring for functions if missing. ----")
    )
  "Alist of prompt prefixes.")

(defvar chatgpt-font-lock-keywords
  '(;; comment
    ("^[;%].+"  . font-lock-comment-face)
    ;; header
    ("^#+.+"  . font-lock-function-name-face)
    ("^Q\\. .+" . font-lock-function-name-face)
    ("^\\S.+?[:：]$"  . font-lock-function-name-face)
    ;; emphasis
    ("\\*\\*[^*]+\\*\\*"  . font-lock-constant-face)
    ;; item
    ("^ *[0-9.]+ " . font-lock-type-face)
    ("^ *[*-] .*$" . font-lock-string-face)
    ;; constants
    ("[A-Z_]\\{3,\\}" . font-lock-constant-face)	
    ;; strings
    ("【.+?】" . font-lock-constant-face)
    ("「.+?」" . font-lock-constant-face)
    ("\".+?\"" . font-lock-string-face)
    ("'.+?'" . font-lock-string-face)))

(defvar chatgpt--buffer-name "*ChatGPT response*")
(defvar chatgpt--raw-buffer-name "*ChatGPT raw*")
(defvar chatgpt--last-prompt nil)
(defvar chatgpt--last-raw-response nil)
(defvar chatgpt--process nil)
(defvar chatgpt--monitor-process nil)
(defvar chatgpt--monitor-timer nil)
(defvar chatgpt--monitor-ntries nil)

;; ---------------- Utils
(defun chatgpt--update-mode-name (status)
  "Update the mode name to reflect the current status."
  (setq mode-name (format "%s%s:%s"
			  chatgpt-engine
			  (if chatgpt-use-api "_API" "")
			  status)))

(defun chatgpt-mode ()
  "Set up the ChatGPT mode."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'chatgpt-mode)
  (setq font-lock-defaults '(chatgpt-font-lock-keywords 'keywords-only nil))
  (font-lock-mode 1)
  (visual-line-mode 1))

(defun chatgpt--replace-regexp (regexp newtext)
  "Replace REGEXP with NEWTEXT in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match newtext))))

(defun chatgpt--expand-macros ()
  (save-excursion
    ;; Replace all [[FILE]] directives with the content of FILE.
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
      (let ((filename (match-string 1)))
	(when (file-exists-p filename)
	  (replace-match "")
          (insert-file-contents filename))))))

;; ---------------- Low level interfaces
(defun chatgpt--read-prefix ()
  "Prompt the user to select a prompt prefix."
  (let* ((ch (read-char
	      "Select [w]hat/[s]ummary/[j]a/[e]n/[p]roof/[r]ewrite/[R]efactor: "))
	 (elem (assoc ch chatgpt-prefix-alist))
	 (prefix (cdr elem)))
    prefix))

(defun chatgpt--find-prompt ()
  "Find the prompt based on the current point or selected region."
  (let (beg end prompt)
    (cond
     ;; Region is selected.
     (mark-active
      (setq beg (region-beginning))
      (setq end (region-end))
      (setq prompt (buffer-substring-no-properties beg end)))
     ;; A character follows the point.
     ((looking-at "\\w")
      (setq prompt (thing-at-point 'word)))
     ;; Possibly, at the end line.
     (t
      (setq prompt (string-trim
		   (or (thing-at-point 'paragraph)
		       "")))
      (setq beg (save-excursion
		  (search-backward prompt nil t)
		  (point)))
      (setq end (point))))
    ;; Remove the preceding Q.
    (setq prompt (replace-regexp-in-string "^Q\\. *" "" prompt))
    prompt))

;; Start web browser if not running.
(defun chatgpt--start-browser ()
  (unless chatgpt-use-api
    (let ((pid (string-trim (shell-command-to-string
			     (format "pgrep %s" chatgpt-browser-prog)))))
      (when (string-empty-p pid)
	(apply 'start-process
	       chatgpt-browser-prog nil
	       chatgpt-browser-prog
	       chatgpt-browser-args)
	(let ((connected nil))
	  (while (not connected)
	    (condition-case nil
		(let ((proc (open-network-stream "chatgpt" nil "localhost" 9001)))
		  (setq connected t)
		  (delete-process proc))
	      (error
	       (sleep-for .5)))))))))

(defun chatgpt--send-prompt (prompt)
  "Send prompt PROMPT to the AI."
  (chatgpt--init)
  (chatgpt--stop-monitor)
  (chatgpt--start-monitor)
  ;; Stop the process if already running.
  (when (memq chatgpt--process (process-list))
    (kill-process chatgpt--process))
  (chatgpt--start-browser)
  (let ((prog (if chatgpt-use-api chatgpt-prog-api chatgpt-prog)))
    (setq chatgpt--process
	  (start-process "ChatGPT" chatgpt--buffer-name
			 prog "-e" chatgpt-engine "-s" prompt)))
  (set-process-filter chatgpt--process 'chatgpt--process-filter)
  (set-process-sentinel chatgpt--process 'chatgpt--process-sentinel)
  (setq chatgpt--last-prompt prompt))

(defun chatgpt--process-filter (proc string)
  "Process the output STRING from the process PROC."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert string)
      ;; Hide all emphasis tags.
      (goto-char (point-min))
      (while (re-search-forward "\\(\\*\\*\\).+?\\(\\*\\*\\)" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
        (put-text-property (match-beginning 2) (match-end 2) 'invisible t)))))

(defun chatgpt--process-sentinel (proc event)
  "Handle the completion EVENT of the process PROC."
  (when (string-match "finished" event)
    (if chatgpt-use-api
	(chatgpt--response-finished))))

(defun chatgpt--init ()
  "Initialize the environment."
  (interactive)
  (when (and chatgpt--process
	     (process-live-p chatgpt--process))
    (kill-process chatgpt--process))
  (when (and chatgpt--monitor-process
	     (process-live-p chatgpt--monitor-process))
    (kill-process chatgpt--monitor-process))
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    ;; Initialize the response buffer.
    (with-current-buffer buf
      (erase-buffer)
      (chatgpt-mode)
      (chatgpt--update-mode-name "streaming"))
    ;; Display in the other window.
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf)))

(defun chatgpt--extract-raw-response ()
  "Extract the raw response from the AI engine."
  (with-current-buffer chatgpt--raw-buffer-name
    (buffer-string)))

(defun chatgpt--extract-response ()
  "Extract the response from the AI engine."
  (with-current-buffer chatgpt--buffer-name
    (buffer-string)))

(defun chatgpt--save-response ()
  "Save the last prompt and response to a log file."
  (interactive)
  (when chatgpt--last-prompt 
    (let* ((save-silently t)
           (base (format-time-string "%Y%m%d"))
	   (file (format "~/var/log/chatgpt/%s-%s.org" chatgpt-engine base)))
      (with-temp-buffer
	(insert "\n** ")
	;; Insert prompt and response at the point.
	(chatgpt-insert-response '(4))
	(write-region (point-min) (point-max) file 'append)))))

;; ---------------- Response monitor.
(defun chatgpt--start-monitor ()
  "Start monitoring replies."
  (when (not chatgpt-use-api)
    ;; Schedule the next timer event.
    (setq chatgpt--monitor-ntries 0)
    (chatgpt--sched-monitor-event)))

(defun chatgpt--stop-monitor ()
  "Stop the response monitoring process."
  (when (and chatgpt--monitor-timer
	     (timerp chatgpt--monitor-timer))
    (cancel-timer chatgpt--monitor-timer)
    (setq chatgpt--monitor-timer nil)))

(defun chatgpt--sched-monitor-event ()
  "Schedule a monitoring event."
  (setq chatgpt--monitor-timer
	(run-with-timer .2 nil 'chatgpt--monitor-event)))

(defun chatgpt--monitor-event ()
  "Monitor the replies from the AI."
  (let ((buf (get-buffer-create chatgpt--raw-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (setq chatgpt--monitor-process
	    (start-process "chatgpt" buf chatgpt-prog "-e" chatgpt-engine "-r"))
      (set-process-sentinel chatgpt--monitor-process
			    'chatgpt--monitor-process-sentinel))))

(defun chatgpt--monitor-format-buffer ()
  "Format the contents of the response buffer."
  ;; Reformat list items.
  (chatgpt--replace-regexp "\n\n\\( *[0-9-] .+?\\)$" "\n\\1")
  ;; Remove emphasis.
  (chatgpt--replace-regexp "\\*\\*\\(.+?\\)\\*\\*" "\\1")
  (chatgpt--replace-regexp "’" "'")
  (chatgpt--replace-regexp "—" "---")
  (chatgpt--replace-regexp "–" "-")
  (chatgpt--replace-regexp "^Edit in a page$" ""))
  
(defun chatgpt--monitor-process-sentinel (proc event)
  "Handle the completion EVENT of the monitor process PROC."
  (when (string-match-p "finished" event)
    (let ((response (chatgpt--extract-raw-response)))
      (if (string= response chatgpt--last-raw-response)
	  ;; No update.
	  (setq chatgpt--monitor-ntries (1+ chatgpt--monitor-ntries))
	;; Updated.
	(let* ((win (get-buffer-window chatgpt--buffer-name))
               (last-pnt (window-point win))
               (last-start (window-start win)))
	  (with-current-buffer chatgpt--buffer-name
	    (chatgpt--update-mode-name "streaming")
	    (erase-buffer)
	    (insert response)
	    (shr-render-region (point-min) (point-max))
	    (chatgpt--monitor-format-buffer))
	  ;; Restore the window start and position.
	  (when win
            (set-window-point win last-pnt)
            (set-window-start win last-start)))
	(setq chatgpt--last-raw-response response)
	(setq chatgpt--monitor-ntries 0))
      (if (or (string-suffix-p "\nEOF\n" response)
	      (>= chatgpt--monitor-ntries 50)) ;; .2 seconds x 50 = 10 seconds.
	  ;; Finished
	  (chatgpt--response-finished)
	;; Schedule the next event
	(chatgpt--sched-monitor-event)))))

(defun chatgpt--response-finished ()
  "Handle the completion of a prompt."
  (with-current-buffer chatgpt--buffer-name
    (chatgpt--update-mode-name "finished"))
  (chatgpt--save-response))

;; ---------------- User-level interfaces.
(defun chatgpt-send (arg)
  "Send a prompt to the AI."
  (interactive "P")
  (let ((prefix "")
	(prompt (chatgpt--find-prompt)))
    (when (equal arg '(16))
      (setq prefix (chatgpt--read-prefix)))
    (when arg
      (setq prompt (read-string (format "%s prompt: " chatgpt-engine) prompt)))
    (with-temp-buffer
      (insert prompt)
      (chatgpt--expand-macros)
      (setq prompt (buffer-string)))
    (chatgpt--send-prompt (concat prefix prompt))))

(defun chatgpt-send-api (arg)
  (interactive "P")
  (let ((chatgpt-use-api t))
    (chatgpt-send arg)))

(defun chatgpt-insert-response (arg)
  "Insert the latest response from the AI. If ARG is non-nil, include the prompt."
  (interactive "P")
  (let ((response (string-trim (chatgpt--extract-response))))
    (if arg
	(progn
	  (insert "Q. " chatgpt--last-prompt "\n\n")
	  (insert "A. " response))
      (insert response))))

(defun chatgpt-fill ()
  "Guess a content that fits at the the point."
  (interactive)
  (let* ((pnt (point))
	 (buf (buffer-string))
	 (prefix "Write sentences/program that fits in __FILL_THIS_PART__ in the text after ----.
If the text is in RFC 822 format email, write five sentences corresponding to the previous message.
Every line of the previous message is marked with '> '.
Do not output anything except the sentences/program that fits in __FILL_THIS_PART__.
Do not add '> ' at the beginning of lines.

----
")
	 (prompt (concat (substring buf 0 (1- pnt))
			"__FILL_THIS_PART__"
			(substring buf (1- pnt)))))
    (let ((chatgpt-use-api t))
      (chatgpt--send-prompt (concat prefix prompt)))))

;; (chatgpt-select-engine)
(defun chatgpt-select-engine ()
  "Change the AI engine."
  (interactive)
  (let ((ch (read-char-from-minibuffer
	     "Select engine (c:ChatGPT, g:Gemini, l:Claude, p:Copilot, e:Copilot-Enterprise): ")))
    (setq chatgpt-engine
          (pcase ch
            (?c "ChatGPT")
            (?g "Gemini")
            (?l "Claude")
            (?p "Copilot")
            (?e "Copilot-Enterprise")
            (_ chatgpt-engine)))))

(provide 'chatgpt)
