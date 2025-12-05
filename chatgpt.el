;; -*- Emacs-Lisp -*-
;;
;; Interactively access generative AIs from Emacs with/without APIs.
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

;; Usage:
;; C-c b          Send a prompt near the point.
;; C-u C-c b      Send a prompt near the point after revising the prompt.
;; C-u C-u C-c b  Send a prompt near the point after selecting a prompt prefix.
;; C-c Q          Insert the latest response at the point.
;; C-u C-c Q      Insert the pair of the latest prompt and response at the point.
;; C-c f          Generate a context that fits at the point.
;; C-c E          Select AI engine.

(require 'shr)

;;; User Configuration

(defvar chatgpt-prog "~/src/chatgpt-el/chatgpt-cdp")
(defvar chatgpt-engine "gemini")

(defvar chatgpt-api-prog "~/src/chatgpt-el/chatgpt-api")
(defvar chatgpt-api-engine "ollama")

(defvar chatgpt-browser-prog "qutebrowser")
(defvar chatgpt-browser-args '("--qt-flag" "remote-debugging-port=9000"))
;; (defvar chatgpt-browser-prog "chromium")
;; (defvar chatgpt-browser-args '("--remote-debugging-port=9000"
;;                                "--remote-allow-origins=http://127.0.0.1:9000"))

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues:")
    (?s . "Summarize the following in Japanese in a plain academic writing style:")
    (?j . "Translate the following in Japanese in a plain academic writing style:")
    (?e . "Translate the following in English in a plain academic writing style:")
    (?p . "Proofread the following and provide a list of changes made in Markdown table:")
    (?r . "Rewrite the following in a plain academic writing style:")
    (?R . "Refactor the code starting from ----.  Do not delete any comment.  Add a short docstring for functions if missing. ----"))
  "Alist of prompt prefixes.")

(defvar chatgpt-engine-alist
  '((?c . "chatgpt")
    (?g . "gemini")
    (?o . "ollama")
    (?l . "claude")
    (?p . "copilot")
    (?e . "copilot-enterprise"))
  "Alist mapping characters to engine names.")

;;; Internal Variables (Buffer Local)

;; Make variables buffer-local to support parallel execution across different buffers.
(defvar-local chatgpt--use-api nil)
(defvar-local chatgpt--process nil)
(defvar-local chatgpt--last-prompt nil)
(defvar-local chatgpt--monitor-process nil)
(defvar-local chatgpt--monitor-timer nil)
(defvar-local chatgpt--monitor-ntries 0)
(defvar-local chatgpt--last-raw-response nil)

(defvar chatgpt-font-lock-keywords
  '(("^[;%].+" . font-lock-comment-face)
    ("^#+.+" . font-lock-function-name-face)
    ("^Q\\. .+" . font-lock-function-name-face)
    ("^\\S.+?[:：]$" . font-lock-function-name-face)
    ("\\*\\*[^*]+\\*\\*" . font-lock-constant-face)
    ("^ *[0-9.]+ " . font-lock-type-face)
    ("^ *[*-] .*$" . font-lock-string-face)
    ("[A-Z_]\\{3,\\}" . font-lock-constant-face)
    ("【.+?】" . font-lock-constant-face)
    ("「.+?」" . font-lock-constant-face)
    ("\".+?\"" . font-lock-string-face)
    ("'.+?'" . font-lock-string-face)))

;;; Mode Definition

(define-derived-mode chatgpt-mode text-mode "ChatGPT"
  "Major mode for ChatGPT response."
  (setq font-lock-defaults '(chatgpt-font-lock-keywords 'keywords-only nil))
  (font-lock-mode 1))

(defun chatgpt--update-mode-name (status)
  "Update the mode name to reflect the current status."
  (setq mode-name (format "%s:%s"
                          (if chatgpt--use-api
                              (concat chatgpt-api-engine "-api")
                            chatgpt-engine)
                          status))
  (force-mode-line-update))

;;; Buffer Management

(defun chatgpt--buffer-name (engine use-api &optional raw)
  "Generate buffer name based on ENGINE and USE-API."
  (format "*%s%s %s*"
          engine
          (if use-api "-api" "")
          (if raw "raw" "response")))

(defun chatgpt--get-buffer-create (engine use-api &optional raw)
  "Get or create a buffer for the specific ENGINE and API usage."
  (let ((buf-name (chatgpt--buffer-name engine use-api raw)))
    (get-buffer-create buf-name)))

(defun chatgpt--init-buffer (engine use-api)
  "Initialize the response buffer with local variables."
  (let ((buf (chatgpt--get-buffer-create engine use-api)))
    (with-current-buffer buf
      (chatgpt-mode)
      ;; Set local variables specific to this execution context.
      (setq chatgpt-engine engine)
      (setq chatgpt--use-api use-api)
      (chatgpt--update-mode-name "streaming")
      (erase-buffer)
      ;; Display in the other window
      (delete-other-windows)
      (split-window)
      (set-window-buffer (next-window) buf))
    buf))

;;; Utilities

(defun chatgpt--replace-regexp (regexp newtext)
  "Replace REGEXP with NEWTEXT in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match newtext))))

(defun chatgpt--expand-macros ()
  "Expand macros in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
      (let ((filename (match-string 1)))
        (when (file-exists-p filename)
          (replace-match "")
          (insert-file-contents filename))))))

(defun chatgpt--find-prompt ()
  "Find the prompt based on the current point or selected region."
  (let (beg end prompt)
    (cond
     (mark-active
      (setq beg (region-beginning) end (region-end)
            prompt (buffer-substring-no-properties beg end)))
     ((looking-at "\\w")
      (setq prompt (thing-at-point 'word)))
     (t
      (setq prompt (string-trim (or (thing-at-point 'paragraph) "")))
      (save-excursion
        (setq beg (search-backward prompt nil t)
              end (point)))))
    (replace-regexp-in-string "^Q\\. *" "" prompt)))

(defun chatgpt--start-browser ()
  "Start web browser if not running."
  (unless chatgpt--use-api
    (let ((pid (string-trim (shell-command-to-string
                             (format "pgrep %s" chatgpt-browser-prog)))))
      (when (string-empty-p pid)
        (apply 'start-process chatgpt-browser-prog nil chatgpt-browser-prog chatgpt-browser-args)
        (let ((connected nil))
          (while (not connected)
            (condition-case nil
                (let ((proc (open-network-stream "chatgpt" nil "localhost" 9000)))
                  (setq connected t)
                  (delete-process proc))
              (error (sleep-for .5)))))))))

;;; Process Handling (Send & Receive)

(defun chatgpt--send-prompt (prompt use-api)
  "Send PROMPT to the AI using specified configuration."
  (let* ((engine (if use-api chatgpt-api-engine chatgpt-engine))
         (buf (chatgpt--init-buffer engine use-api)))
    
    (with-current-buffer buf
      (chatgpt--stop-monitor) ;; Stop existing monitor in THIS buffer
      (chatgpt--start-monitor)
      
      (when (and chatgpt--process (process-live-p chatgpt--process))
        (kill-process chatgpt--process))
      
      (chatgpt--start-browser)
      
      (let* ((prog (if use-api chatgpt-api-prog chatgpt-prog))
             (args (list "-e" engine "-s" prompt)))
        (setq chatgpt--process
              (apply 'start-process engine buf prog args)))
      
      (set-process-filter chatgpt--process 'chatgpt--process-filter)
      (set-process-sentinel chatgpt--process 'chatgpt--process-sentinel)
      (setq chatgpt--last-prompt prompt))))

(defun chatgpt--process-filter (proc string)
  "Process the output STRING from the process PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char (point-max))
        (insert string)
        ;; Hide emphasis tags.
        (goto-char (point-min))
        (while (re-search-forward "\\(\\*\\*\\).+?\\(\\*\\*\\)" nil t)
          (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
          (put-text-property (match-beginning 2) (match-end 2) 'invisible t))))))

(defun chatgpt--process-sentinel (proc event)
  "Handle the completion EVENT of the process PROC."
  (when (and (buffer-live-p (process-buffer proc))
             (string-match "finished" event))
    (with-current-buffer (process-buffer proc)
      (if chatgpt--use-api (chatgpt--response-finished)))))

(defun chatgpt--response-finished ()
  "Handle the completion of a prompt."
  (chatgpt--update-mode-name "finished")
  (chatgpt--save))

(defun chatgpt--save ()
  "Save the last prompt and response."
  (when chatgpt--last-prompt
    (let* ((save-silently t)
           (base (format-time-string "~/var/log/chatgpt/%y%m%d-%H%M%S"))
           (response (buffer-string)))
      (with-temp-buffer
        (insert chatgpt--last-prompt)
        (write-region (point-min) (point-max) (concat base ".pt")))
      (with-temp-buffer
        (insert response)
        (write-region (point-min) (point-max) (concat base ".rs"))))))

;;; Monitor (Polling) Implementation

(defun chatgpt--start-monitor ()
  "Start monitoring response."
  (unless chatgpt--use-api
    (setq chatgpt--monitor-ntries 0)
    (chatgpt--sched-monitor-event)))

(defun chatgpt--stop-monitor ()
  "Stop the response monitoring process."
  (when (and chatgpt--monitor-timer (timerp chatgpt--monitor-timer))
    (cancel-timer chatgpt--monitor-timer)
    (setq chatgpt--monitor-timer nil))
  (when (and chatgpt--monitor-process (process-live-p chatgpt--monitor-process))
    (kill-process chatgpt--monitor-process)))

(defun chatgpt--sched-monitor-event ()
  "Schedule a monitoring event."
  ;; Pass the current buffer to the timer so it runs in correct context.
  (setq chatgpt--monitor-timer
        (run-with-timer .2 nil 'chatgpt--monitor-event (current-buffer))))

(defun chatgpt--monitor-event (buf)
  "Monitor the response from the AI."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((raw-buf (chatgpt--get-buffer-create chatgpt-engine nil t)))
        (with-current-buffer raw-buf
          (erase-buffer)
          (let ((proc (start-process "chatgpt-monitor" raw-buf
                                     chatgpt-prog "-e" chatgpt-engine "-r")))
            ;; Save the target response buffer in the process object for the sentinel
            (process-put proc 'target-buffer buf)
            (set-process-sentinel proc 'chatgpt--monitor-process-sentinel)))))))

(defun chatgpt--monitor-format-buffer ()
  "Format the contents of the response buffer."
  (chatgpt--replace-regexp "\n\n\\( *[0-9-] .+?\\)$" "\n\\1")
  (chatgpt--replace-regexp "\\*\\*\\(.+?\\)\\*\\*" "\\1")
  (chatgpt--replace-regexp "’" "'")
  (chatgpt--replace-regexp "—" "---")
  (chatgpt--replace-regexp "–" "-")
  (chatgpt--replace-regexp "^Edit in a page$" ""))

(defun chatgpt--monitor-process-sentinel (proc event)
  "Handle the completion EVENT of the monitor process PROC."
  (when (string-match-p "finished" event)
    (let ((buf (process-get proc 'target-buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((response (with-current-buffer (process-buffer proc) (buffer-string))))
            (if (string= response chatgpt--last-raw-response)
                (setq chatgpt--monitor-ntries (1+ chatgpt--monitor-ntries))
              ;; Content updated.
              (let* ((win (get-buffer-window buf))
                     (last-pnt (if win (window-point win)))
                     (last-start (if win (window-start win))))
                (erase-buffer)
                (insert response)
                (shr-render-region (point-min) (point-max))
                (chatgpt--monitor-format-buffer)
                (when win
                  (set-window-point win last-pnt)
                  (set-window-start win last-start))
                (setq chatgpt--last-raw-response response)
                (setq chatgpt--monitor-ntries 0)))
            
            (if (or (string-suffix-p "\nEOF\n" response)
                    (>= chatgpt--monitor-ntries 50))
                (chatgpt--response-finished)
              (chatgpt--sched-monitor-event))))))))

;;; Interactive Commands

(defun chatgpt-send (arg)
  "Send a prompt to the AI. With C-u, edit prompt. With C-u C-u, select
prefix."
  (interactive "P")
  (let ((prefix "")
        (prompt (chatgpt--find-prompt)))
    (when (equal arg '(16))
      (let* ((ch (read-char "Select [w]hat/[s]ummary/[j]a/[e]n/[p]roof/[r]ewrite/[R]efactor: "))
             (entry (assoc ch chatgpt-prefix-alist)))
        (setq prefix (cdr entry))))
    (when arg
      (setq prompt (read-string (format "%s prompt: " chatgpt-engine) prompt)))
    
    (with-temp-buffer
      (insert prompt)
      (chatgpt--expand-macros)
      (setq prompt (buffer-string)))
    
    (chatgpt--send-prompt (concat prefix prompt) chatgpt--use-api)))

(defun chatgpt-send-api (arg)
  (interactive "P")
  (let ((chatgpt--use-api t))
    (chatgpt-send arg)))

(defun chatgpt-insert-response (&optional arg)
  "Insert the latest response.  If the current buffer is a ChatGPT response
buffer, use it.  Otherwise, try to find the most recent response buffer
for the current engine."
  (interactive "P")
  (let* ((buf (if (derived-mode-p 'chatgpt-mode)
                         (current-buffer)
                       (get-buffer (chatgpt--buffer-name 
                                    (if chatgpt--use-api chatgpt-api-engine chatgpt-engine)
                                    chatgpt--use-api)))))
    (if (not (and buf (buffer-live-p buf)))
        (message "No active ChatGPT response buffer found.")
      (with-current-buffer buf
        (let ((prompt chatgpt--last-prompt)
              (response (string-trim (buffer-string))))
          (with-current-buffer (window-buffer (selected-window)) ;; Insert into original buffer.
            (if arg
                (insert "Q. " (or prompt "") "\n\nA. " response)
              (insert response))))))))

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
    (let ((chatgpt--use-api t))
      (chatgpt--send-prompt (concat prefix prompt) t))))

(defun chatgpt-select-engine ()
  "Change the AI engine."
  (interactive)
  (let* ((ch (read-char-from-minibuffer
	      "Select engine (c:chatgpt, g:gemini, o:ollama, l:claude, p:copilot, e:copilot-enterprise): "))
         (selected (cdr (assoc ch chatgpt-engine-alist))))
    (if selected
        (setq chatgpt-engine selected)
      (message "No engine selected, keeping %s" chatgpt-engine))))

(provide 'chatgpt)
