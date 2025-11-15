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
;; (autoload 'chatgpt-query "chatgpt" nil t)
;; (autoload 'chatgpt-insert-reply "chatgpt" nil t)
;; (autoload 'chatgpt-fill-at-point "chatgpt" nil t)
;; (global-set-key "\C-cq" 'chatgpt-query-api)
;; (global-set-key "\C-cb" 'chatgpt-query)
;; (global-set-key "\C-cQ" 'chatgpt-insert-reply)
;; (global-set-key "\C-cf" 'chatgpt-fill-at-point-api)
;;
;; Usage:
;; C-c q          Send a query near the point.
;; C-u C-c q      Send a query near the point after revising the query in the minibuffer.
;; C-u C-u C-c q  Send a query near the point after selecting a query prefix.
;; C-c Q          Insert the latest reply at the point.  
;; C-u C-c Q      Insert the pair of the latest query and reply at the point.  
;; C-c f          Generate a context that fits at the point.

(defvar chatgpt-prog "~/src/chatgpt-el/chatgpt-cdp")
(defvar chatgpt-prog-api "~/src/chatgpt-el/chatgpt-api")
(defvar chatgpt-engine "ChatGPT")
;; (defvar chatgpt-engine "Gemini")
(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues:")
    (?s . "Summarize the following in Japanese in a plain academic writing style:")
    (?j . "Translate the following in Japanese in a plain academic writing style:")
    (?e . "Translate the following in English in a plain academic writing style:")
    (?p . "Proofread the following and provide a list of changes made in Markdown table:")
    (?r . "Rewrite the following in a plain academic writing style:")
    (?R . "Refactor the code starting from ----.  Do not delete any comment.  Add a short docstring for functions if missing. ----")
    )
  "Alist of query prefixes.")

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

(defvar chatgpt--buffer-name "*ChatGPT reply*")
(defvar chatgpt--raw-buffer-name "*ChatGPT raw*")
(defvar chatgpt--last-query nil)
(defvar chatgpt--last-engine nil)
(defvar chatgpt--last-raw-reply nil)
(defvar chatgpt--process nil)
(defvar chatgpt--monitor-process nil)
(defvar chatgpt--monitor-timer nil)
(defvar chatgpt--monitor-ntries nil)

;; ---------------- Utils
(defun chatgpt--update-mode-name (engine use-api status)
  "Update the mode name to reflect the current engine, API usage, and status."
  (setq mode-name (format "%s%s:%s" engine
			  (if use-api "_API" "")
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

;; ---------------- Low level interfaces
(defun chatgpt--read-prefix ()
  "Prompt the user to select a query prefix."
  (let* ((ch (read-char
	      "Select [w]hat/[s]ummary/[j]a/[e]n/[p]roof/[r]ewrite/[R]efactor: "))
	 (elem (assoc ch chatgpt-prefix-alist))
	 (prefix (cdr elem)))
    prefix))

(defun chatgpt--find-query ()
  "Find the query based on the current point or selected region."
  (let (beg end query)
    (cond
     ;; Region is selected.
     (mark-active
      (setq beg (region-beginning))
      (setq end (region-end))
      (setq query (buffer-substring-no-properties beg end)))
     ;; A character follows the point.
     ((looking-at "\\w")
      (setq query (thing-at-point 'word)))
     ;; Possibly, at the end line.
     (t
      (setq query (string-trim
		   (or (thing-at-point 'paragraph)
		       "")))
      (setq beg (save-excursion
		  (search-backward query nil t)
		  (point)))
      (setq end (point))))
    ;; Remove the preceding Q.
    (setq query (replace-regexp-in-string "^Q\\. *" "" query))
    query))

(defun chatgpt--send-query (query &optional engine use-api)
  "Send QUERY to the engine specified by ENGINE, using API if USE-API is non-nil."
  (chatgpt--init engine use-api)
  (chatgpt--stop-monitor)
  (chatgpt--start-monitor engine use-api)
  ;; Stop the process if already running.
  (when (memq chatgpt--process (process-list))
    (kill-process chatgpt--process))
  ;; Start Chromium if not running.
  (unless use-api
    (let ((pid (string-trim (shell-command-to-string "pidof chromium"))))
      (when (string-empty-p pid)
        (start-process "chromium" nil "chromium"
		       "--remote-debugging-port=9000"
		       "--remote-allow-origins=http://127.0.0.1:9000")
	(sleep-for 3.))))
  (let ((prog (if use-api chatgpt-prog-api chatgpt-prog)))
    (setq chatgpt--process
	  (start-process "ChatGPT" chatgpt--buffer-name
			 prog "-e" engine "-s" query)))
  (set-process-filter chatgpt--process 'chatgpt--process-filter)
  ;; Catch the end of reply with the sentinel in API mode.
  (if use-api
      (set-process-sentinel chatgpt--process 'chatgpt--process-sentinel))
  (setq chatgpt--last-query query)
  (setq chatgpt--last-engine engine))

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

;; This sentinel is used only in API mode.
(defun chatgpt--process-sentinel (proc event)
  "Handle the completion EVENT of the process PROC."
  (when (string-match "finished" event)
    (chatgpt--query-finished)))

(defun chatgpt--init (engine use-api)
  "Initialize the environment for ENGINE and API usage if USE-API is non-nil."
  (interactive)
  (when (and chatgpt--process
	     (process-live-p chatgpt--process))
    (kill-process chatgpt--process))
  (when (and chatgpt--monitor-process
	     (process-live-p chatgpt--monitor-process))
    (kill-process chatgpt--monitor-process))
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    ;; Initialize the reply buffer.
    (with-current-buffer buf
      (erase-buffer)
      (chatgpt-mode)
      (chatgpt--update-mode-name engine use-api "streaming"))
    ;; Display in the other window.
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf)))

(defun chatgpt--extract-raw-reply ()
  "Extract the raw reply from the AI engine."
  (with-current-buffer chatgpt--raw-buffer-name
    (buffer-string)))

(defun chatgpt--extract-reply ()
  "Extract the reply from the AI engine."
  (with-current-buffer chatgpt--buffer-name
    (buffer-string)))

(defun chatgpt--save-reply ()
  "Save the last query and reply to a log file."
  (interactive)
  (when chatgpt--last-query 
    (let* ((save-silently t)
           (base (format-time-string "%Y%m%d"))
	   (file (format "~/var/log/chatgpt/%s-%s.org" chatgpt--last-engine base)))
      (with-temp-buffer
	(insert "\n** ")
	;; Insert query and reply at the point.
	(chatgpt-insert-reply '(4))
	(write-region (point-min) (point-max) file 'append)))))

;; ---------------- Reply monitor.
(defun chatgpt--start-monitor (engine use-api)
  "Start monitoring replies for the specified ENGINE, using API if USE-API is non-nil."
  (when (not use-api)
    ;; Schedule the next timer event.
    (setq chatgpt--monitor-ntries 0)
    (chatgpt--sched-monitor-event engine)))

(defun chatgpt--stop-monitor ()
  "Stop the reply monitoring process."
  (when (and chatgpt--monitor-timer
	     (timerp chatgpt--monitor-timer))
    (cancel-timer chatgpt--monitor-timer)
    (setq chatgpt--monitor-timer nil)))

(defun chatgpt--sched-monitor-event (engine)
  "Schedule a monitoring event for the specified ENGINE."
  (setq chatgpt--monitor-timer
	(run-with-timer .5 nil 'chatgpt--monitor-event engine)))

(defun chatgpt--monitor-event (engine)
  "Monitor the replies for the specified ENGINE."
  (let ((buf (get-buffer-create chatgpt--raw-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (setq chatgpt--monitor-process
	    (start-process "chatgpt" buf chatgpt-prog "-e" engine "-r"))
      (set-process-sentinel chatgpt--monitor-process
			    'chatgpt--monitor-process-sentinel))))

(defun chatgpt--monitor-format-buffer ()
  "Format the contents of the reply buffer."
  ;; Reformat list items.
  (chatgpt--replace-regexp "\n\n\\( *[0-9-] .+?\\)$" "\n\\1")
  ;; Remove emphasis.
  (chatgpt--replace-regexp "\\*\\*\\(.+?\\)\\*\\*" "\\1")
  (chatgpt--replace-regexp "’" "'")
  (chatgpt--replace-regexp "—" "---")
  (chatgpt--replace-regexp "–" "-")
  (chatgpt--replace-regexp "SVG Image\n" ""))

(defun chatgpt--monitor-process-sentinel (proc event)
  "Handle the completion EVENT of the monitor process PROC."
  (when (string-match-p "finished" event)
    (let ((reply (chatgpt--extract-raw-reply)))
      (if (string= reply chatgpt--last-raw-reply)
	  ;; No update.
	  (setq chatgpt--monitor-ntries (1+ chatgpt--monitor-ntries))
	;; Updated.
	(let* ((win (get-buffer-window chatgpt--buffer-name))
               (last-pnt (window-point win))
               (last-start (window-start win)))
	  (with-current-buffer chatgpt--buffer-name
	    (chatgpt--update-mode-name chatgpt--last-engine nil "streaming")
	    (erase-buffer)
	    (insert reply)
	    (shr-render-region (point-min) (point-max))
	    (chatgpt--monitor-format-buffer))
	  ;; Restore the window start and position.
          (set-window-point win last-pnt)
          (set-window-start win last-start))
	(setq chatgpt--last-raw-reply reply)
	(setq chatgpt--monitor-ntries 0))
      (if (or (string-suffix-p "\nEOF\n" reply)
	      (>= chatgpt--monitor-ntries 20)) ;; .5 seconds x 20 = 10 seconds.
	  ;; Finished
	  (chatgpt--query-finished)
	;; Schedule the next event
	(chatgpt--sched-monitor-event chatgpt--last-engine)))))

(defun chatgpt--query-finished ()
  "Handle the completion of a query."
  (with-current-buffer chatgpt--buffer-name
    (chatgpt--update-mode-name chatgpt--last-engine nil "finished"))
  (chatgpt--save-reply))

;; ---------------- User-level interfaces.
(defun chatgpt-query (arg &optional engine use-api)
  "Send a query to the AI, optionally using ENGINE and API based on ARG."
  (interactive "P")
  (setq engine (or engine chatgpt-engine))
  (let ((prefix "")
	(query (chatgpt--find-query)))
    (when (equal arg '(16))
      (setq prefix (chatgpt--read-prefix)))
    (when arg
      (setq query (read-string (format "%s query: " engine) query)))
    (chatgpt--send-query (concat prefix query) engine use-api)))

(defun chatgpt-query-api (arg &optional engine)
  (interactive "P")
  (chatgpt-query arg engine t))

(defun chatgpt-insert-reply (arg)
  "Insert the latest reply from the AI. If ARG is non-nil, include the query."
  (interactive "P")
  (let ((reply (string-trim (chatgpt--extract-reply))))
    (if arg
	(progn
	  (insert "Q. " chatgpt--last-query "\n\n")
	  (insert "A. " reply))
      (insert reply))))

(defun chatgpt-fill (&optional engine use-api)
  "Guess a content that fits at the the point, using ENGINE and API if specified."
  (interactive)
  (setq engine (or engine "ChatGPT"))
  ;; FIXME: Do not force to use API.
  (setq use-api t)
  (let* ((pnt (point))
	 (buf (buffer-string))
	 (prefix "Write sentences/program that fits in __FILL_THIS_PART__ in the text after ----.
If the text is in RFC 822 format email, write five sentences corresponding to the previous message.
Every line of the previous message is marked with '> '.
Do not output anything except the sentences/program that fits in __FILL_THIS_PART__.
Do not add '> ' at the beginning of lines.

----
")
	 (query (concat (substring buf 0 (1- pnt))
			"__FILL_THIS_PART__"
			(substring buf (1- pnt)))))
    (chatgpt--send-query (concat prefix query) engine use-api)))

(defun chatgpt-fill-api (&optional engine)
  (interactive)
  (chatgpt-fill engine t))

(provide 'chatgpt)
