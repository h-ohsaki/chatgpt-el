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
(defvar chatgpt-default-engine "gemini")

(defvar chatgpt-api-prog "~/src/chatgpt-el/chatgpt-api")
(defvar chatgpt-default-api-engine "chatgpt")

(defvar chatgpt-browser-prog "qutebrowser")
(defvar chatgpt-browser-args '("--qt-flag" "remote-debugging-port=9000"))
;; (defvar chatgpt-browser-prog "chromium")
;; (defvar chatgpt-browser-args '("--remote-debugging-port=9000"
;;                                "--remote-allow-origins=http://127.0.0.1:9000"))

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues:")
    (?s . "Summarize the following in Japanese in a plain academic writing style:")
    (?S . "Select interesting or noteworthy information from the following and present five items in a ranked list in Japanese:")
    (?j . "Translate the following in Japanese in a plain academic writing style:")
    (?e . "Translate the following in English in a plain academic writing style:")
    (?p . "Proofread the following and provide a list of changes made in Markdown table:")
    (?r . "Rewrite the following in a plain academic writing style:")
    (?E . "Review and identify errors in the following document/program:")
    (?R . "Refactor the code starting from ----.  Do not delete any comment.  Add a short docstring for functions if missing. ----"))
  "Alist of prompt prefixes.")

(defvar chatgpt-available-engines
  '("chatgpt" "gemini" "ollama" "claude" "copilot" "copilot-enterprise"))

(defvar chatgpt-model-alist
  '(("chatgpt" . "ChatGPT-5.2")
    ("gemini" . "Gemini-3")
    ("claude" . "ClaudeSonnet-4.5")
    ("copilot" . "Copilot-Auto")
    ("copilot-enterprise" . "Copilot-Auto")))

(defvar chatgpt-api-available-models-alist
  '(("chatgpt" . ("gpt-5.2" "gpt-5.1" "gpt-5-mini" "gpt-4.1" "gpt-4.1-mini"))
    ("gemini" . ("gemini-3-pro-preview" "gemini-3-flash-preview"
		 "gemini-2.5-pro" "gemini-2.5-flash" "gemini-2.5-flash-lite"
		 "gemini-2.0-flash" "gemini-2.0-flash-lite"))
    ("ollama" .  ("gemma3:12b-it-qat-jp" "gemma3:4b-it-qat" "phi4:15b-q3_K_M"
		  "qwen3:14b" "qwen3:8b" "qwen3:4b" "T3Q-qwen2.5:14b"
		  "deepseek-r1:14b-jpn"
		  "ministral-3:14b" "ministral-3:8b-instruct-2512-q4_K_M"
		  "deepseek-coder-v2:16b" "qwen2.5-coder:14b"))))

(defvar chatgpt-api-model-alist
  '(("chatgpt" . "gpt-4.1-mini")
    ("gemini" . "gemini-3-flash-preview")
    ("ollama" . "ministral-3:14b")))

;;; Internal Variables (Buffer Local)

(defvar chatgpt--last-buf nil)
;; Make variables buffer-local to support parallel execution across different buffers.
(defvar-local chatgpt--engine nil)
(defvar-local chatgpt--model nil)
(defvar-local chatgpt--use-api nil)
(defvar-local chatgpt--process nil)
(defvar-local chatgpt--prompt nil)
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
  (font-lock-mode 1)
  (visual-line-mode 1))

(defun chatgpt--update-mode-name (status)
  "Update the mode name to reflect the current status."
  (setq mode-name (format "%s%s %s %s"
                          chatgpt--engine (if chatgpt--use-api "-api" "") chatgpt--model
                          status))
  (force-mode-line-update))

;;; Buffer Management

(defun chatgpt--buffer-name (engine model use-api &optional raw)
  "Generate buffer name based on ENGINE, MODEL and USE-API."
  (format "*%s%s %s %s*"
          engine
	  model
          (if use-api "-api" "")
          (if raw "raw" "response")))

(defun chatgpt--get-buffer-create (engine model use-api &optional raw)
  "Get or create a buffer for the specific ENGINE, MODEL and API usage."
  (let ((buf-name (chatgpt--buffer-name engine model use-api raw)))
    (get-buffer-create buf-name)))

(defun chatgpt--init-buffer (engine model use-api)
  "Initialize the response buffer with local variables."
  (let ((buf (chatgpt--get-buffer-create engine model use-api)))
    (with-current-buffer buf
      (let ((proc (get-buffer-process buf)))
        (when (processp proc)
          (set-process-sentinel proc nil)
          (delete-process proc)))
      (chatgpt-mode)
      ;; Set local variables specific to this execution context.
      (setq chatgpt--engine engine)
      (setq chatgpt--model model)
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
      (setq prompt (string-trim (or (thing-at-point 'paragraph) "")))))
    (replace-regexp-in-string "^Q\\. *" "" prompt)))

(defun chatgpt--port-listening-p (host port)
  (let ((connected nil))
    (condition-case nil
	(let ((proc (open-network-stream "chatgpt" nil host port)))
          (setq connected t)
          (delete-process proc))
      (error nil))
    connected))

(defun chatgpt--start-browser ()
  "Start web browser if not running."
  (unless chatgpt--use-api
      (unless (chatgpt--port-listening-p "localhost" 9000)
        (apply 'start-process chatgpt-browser-prog nil
	       chatgpt-browser-prog chatgpt-browser-args)
          (while (not (chatgpt--port-listening-p "localhost" 9000))
            (sleep-for .5)))))

;;; Process Handling (Send & Receive)

(defun chatgpt--send-prompt (prompt engine model use-api)
  "Send PROMPT to the AI using specified configuration."
  (let ((buf (chatgpt--init-buffer engine model use-api)))

    (with-current-buffer buf
      (chatgpt--stop-monitor) ;; Stop existing monitor in THIS buffer
      (chatgpt--start-monitor)

      (when (and chatgpt--process (process-live-p chatgpt--process))
        (kill-process chatgpt--process))

      (chatgpt--start-browser)

      (let* ((prog (if use-api chatgpt-api-prog chatgpt-prog))
             (args (list "-e" engine "-m" model))
             (proc (apply 'start-process engine buf prog args)))
	(setq chatgpt--process proc)
        (process-send-string proc (concat prompt "\n"))
	(process-send-eof proc))
	
      (set-process-filter chatgpt--process 'chatgpt--process-filter)
      (set-process-sentinel chatgpt--process 'chatgpt--process-sentinel)
      (setq chatgpt--prompt prompt)
      (setq chatgpt--last-buf buf))))

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
  (let* ((save-silently t)
	 (tstamp (format-time-string "%y%m%d-%H%M%S"))
         (base (format "~/var/log/chatgpt/%s-%s" chatgpt--engine tstamp))
	 (prompt chatgpt--prompt)
	 (response (buffer-string)))
      (with-temp-buffer
        (insert prompt)
        (write-region (point-min) (point-max) (concat base ".pt")))
      (with-temp-buffer
        (insert response)
        (write-region (point-min) (point-max) (concat base ".rs")))))

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
      (let ((raw-buf (chatgpt--get-buffer-create chatgpt--engine chatgpt--model nil t))
	    (engine chatgpt--engine))
        (with-current-buffer raw-buf
          (erase-buffer)
          (let ((proc (start-process "chatgpt-monitor" raw-buf
                                     chatgpt-prog "-e" engine "-r")))
	    (setq chatgpt--monitor-process proc)
            ;; Save the target response buffer in the process object for the sentinel
            (process-put proc 'target-buffer buf)
            (set-process-sentinel proc 'chatgpt--monitor-process-sentinel)))))))

(defun chatgpt--monitor-format-buffer ()
  "Format the contents of the response buffer."
  (goto-char (point-min))
  (insert (format "[%s]\n" chatgpt--model))
  (chatgpt--replace-regexp "\n\n\\( *[0-9-] .+?\\)$" "\n\\1")
  (chatgpt--replace-regexp "\\*\\*\\(.+?\\)\\*\\*" "\\1")
  (chatgpt--replace-regexp "’" "'")
  (chatgpt--replace-regexp "—" "---")
  (chatgpt--replace-regexp "–" "-")
  (chatgpt--replace-regexp "^Edit in a page$" "")
  (chatgpt--replace-regexp "^SVG Image\n" ""))

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

(defun chatgpt-send (arg &optional use-api)
  "Send a prompt to the AI. With C-u, edit prompt. With C-u C-u, select
prefix."
  (interactive "P")
  (let* ((prefix "")
	 (engine (if use-api chatgpt-default-api-engine chatgpt-default-engine))
	 (model (cdr (assoc engine (if use-api chatgpt-api-model-alist chatgpt-model-alist))))
         (prompt (chatgpt--find-prompt)))
    (when (equal arg '(16))
      (let* ((ch (read-char "Prefix [w]hat/[s]ummary/[S]ummary/[j]a/[e]n/[p]roof/[r]ewrite/[E]rror/[R]efactor: "))
             (entry (assoc ch chatgpt-prefix-alist)))
        (setq prefix (cdr entry))))
    (when arg
      (setq prompt (read-string (format "%s prompt: " engine) prompt)))

    (with-temp-buffer
      (insert prompt)
      (chatgpt--expand-macros)
      (setq prompt (buffer-string)))
    
    (chatgpt--send-prompt (concat prefix prompt) engine model use-api)))

(defun chatgpt-send-api (arg)
  (interactive "P")
  (chatgpt-send arg t))

(defun chatgpt-insert-response (&optional arg)
  "Insert the latest response."
  (interactive "P")
  (let ((buf chatgpt--last-buf))
    (if (not (and buf (buffer-live-p buf)))
        (message "No active response buffer found.")
      (with-current-buffer buf
        (let ((prompt chatgpt--prompt)
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
         (prefix "Appropriately fill in the __FILL_THIS_PART__ placeholder in the following document with the corresponding text or program.
If the document is a program, complete the code.
If it is a general document, complete the text.
If it is an email, compose a reply;
note that the sender's message is quoted with a leading '> '.
Write in the same language as the source document.
Output only the content to be inserted into __FILL_THIS_PART__.
Strictly exclude any other output.

---
")
	 (engine chatgpt-default-api-engine)
	 (model (cdr (assoc engine chatgpt-api-model-alist)))
         (prompt (concat (substring buf 0 (1- pnt))
			 "__FILL_THIS_PART__"
			 (substring buf (1- pnt)))))
    (chatgpt--send-prompt (concat prefix prompt) engine model t)))

;; (chatgpt-select-engine nil)
;; (chatgpt-select-engine t)
(defun chatgpt-select-engine (use-api)
  "Change the AI engine.  With the prefix argument USE-API, change the default
API engine; without ARG, change the default engine for Web."
  (interactive "P")
  (let* ((engine (if use-api chatgpt-default-api-engine chatgpt-default-engine))
	 (selected (completing-read (format "Select %sengine [%s]: "
					    (if use-api "API " "")
					    engine)
                                    chatgpt-available-engines
				    nil t)))
    (when (not (string= selected ""))
      (if use-api
          (setq chatgpt-default-api-engine selected)
	(setq chatgpt-default-engine selected)))))

;; (chatgpt-select-api-model)
(defun chatgpt-select-api-model ()
  (interactive)
  (let* ((engine chatgpt-default-api-engine)
	 (model (cdr (assoc engine chatgpt-api-model-alist)))
	 (all-models (cdr (assoc engine chatgpt-api-available-models-alist)))
	 (selected (completing-read (format "Select API model for %s [%s]: "
					    engine
					    model)
                                    all-models
				    nil t)))
    (when (not (string= selected ""))
      (setf (alist-get engine chatgpt-api-model-alist ni nil 'equal) selected))))

(provide 'chatgpt)
