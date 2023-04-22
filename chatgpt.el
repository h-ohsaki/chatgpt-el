;; -*- Emacs-Lisp -*-
;;
;; Access ChatGTP from Emacs without OpenAI API.
;; Copyright (C) 2023 Hiroyuki Ohsaki.
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

(defvar chatgpt-prog "~/src/chatgpt-el/chatgpt"
  "The name of the program communicating with the Chromium using
  the CDP protocol.")

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues.")
    (?s . "Summarize the following in a plain Japanese.")
    (?j . "Translate the following in Japanese in an academic writing style.")
    (?e . "Translate the following in English in an academic writing style.")
    (?p . "Proofread following text and summarize all suggested changes.")
    (?P . "以下の文章の誤りを直して、変更点の一覧を出力して。")))

(defvar chatgpt-font-lock-keywords
  '(;; comment
    ("^[#;%].+"  . font-lock-comment-face)
    ;; header
    ("^Q\\. .+" . font-lock-function-name-face)
    ("^\\S.+?[:：]$"  . font-lock-function-name-face)
    ;; item
    ("^ *[0-9.]+ " . font-lock-type-face)
    ("^ *[*-] .*$" . font-lock-string-face)
    ;; constants
    ("[A-Z_]\\{3,\\}" . font-lock-constant-face)	
    ;; strings
    ("「.+?」" . font-lock-constant-face)
    ("\".+?\"" . font-lock-string-face)))

(defvar chatgpt--buffer-name "*ChatGPT reply*"
  "The name of the buffer to display the reply from ChatGPT.")

(defvar chatgpt--tmpbuffer-name "*ChatGPT temp*"
  "The name of the temporary buffer to receive the reply from ChatGPT.")

(defvar chatgpt--last-query nil
  "The last query sent to the server.")

(defvar chatgpt--last-reply nil
  "The last reply returned by the server.")

(defvar chatgpt--process nil
  "An instance of `chatgpt` script to retrieve the reply.")

(defvar chatgpt--timer nil
  "A timer event to retriee the reply from the server.")

(defvar chatgpt--timer-count nil)

;; (chatgpt-send-string "which of Emacs or vi is better?")
;; (chatgpt-send-string "what is Emacs's interesting history?")
(defun chatgpt-send-string (query)
  "Send query QUERY to the ChatGPT server via chromium."
  (interactive)
  ;; Compose a query in a temporary buffer.
  (with-temp-buffer
    (insert query)
    ;; Provide chromium with the query string.
    (call-process-region (point-min) (point-max) chatgpt-prog
			 nil nil nil "-s"))
  (setq chatgpt--last-query query))

;; (chatgpt--current-paragraph)
(defun chatgpt--current-paragraph ()
  "Return a query string at around the current point."
  (buffer-substring-no-properties
   (save-excursion
     (forward-paragraph -1)
     (if (looking-at "$")
	 (forward-line 1))
     (point))
   (save-excursion
     (forward-paragraph 1)
     (if (not (eobp))
	 (forward-char -1))
     (point))))

;; (chatgpt-send 1)
(defun chatgpt-send (arg)
  "Send the query around the point to the ChatGPT server.  If the
mark is active, send the hlghlighted region as a query."
  (interactive "P")
  (let* ((query
	  (if mark-active
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    ;; When mark is inactive.
	    (chatgpt--current-paragraph)))
	 (buf (get-buffer-create chatgpt--buffer-name)))
    ;; Change the behavior when prefix is provided.
    (cond ((equal arg '(16))
	   (setq query "続き"))
	  (arg
	   (let* ((ch (read-char "Prefix [w]what/[s]ummary/[j]apanese/[e]nglish/[p/P]roofread: "))
		  (val (assoc ch chatgpt-prefix-alist)))
	     (setq query (concat (cdr val) " " query)))))
    (chatgpt-send-string query)
    (chatgpt--start-monitor)))

;; (chatgpt-lookup "Emacs")
(defun chatgpt-lookup (query)
  (interactive (list (read-string "ChatGPT lookup: " 
				  (thing-at-point 'word))))
    (chatgpt-send-string query)
    (chatgpt--start-monitor))

;; (chatgpt--start-monitor)
(defun chatgpt--start-monitor ()
  "Start monitoring the reply from the ChatGPT server."
  (interactive)
  ;; Only for debugging.
  (kill-buffer chatgpt--buffer-name)
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    ;; Initialize the reply buffer.
    (with-current-buffer buf
      (erase-buffer)
      (setq font-lock-defaults
	    '(chatgpt-font-lock-keywords 'keywords-only nil))
      (font-lock-mode 1))
    ;; Display in the other window.
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf)
    ;; Schedule the next timer event.
    (setq chatgpt--timer-count 0)
    (chatgpt--sched-timer-event)))

;; (chatgpt-insert-reply)
(defun chatgpt-insert-reply ()
  "At the current point, insert the reply for the last query from
the ChatGPT server."
  (interactive)
  (insert chatgpt--last-reply))

(defun chatgpt--process-sentinel (proc event)
  ;; Is process completed?
  (when (string-match-p "finished" event)
    (let* ((tmpbuf (get-buffer-create chatgpt--tmpbuffer-name))
	   (reply (with-current-buffer tmpbuf
		    (buffer-string))))
      (if (string= reply chatgpt--last-reply)
	  ;; No update.
	  (setq chatgpt--timer-count (1+ chatgpt--timer-count))
	;; Updated.
	(let ((buf (get-buffer-create chatgpt--buffer-name)))
	  (with-current-buffer buf
	    (erase-buffer)
	    (insert reply)
	    (setq chatgpt--last-reply reply)
	    (setq chatgpt--timer-count 0))))
      ;; Schedule next event if it seems reply is in progress.
      (if (< chatgpt--timer-count 10)
	  (chatgpt--sched-timer-event)))))

;; (chatgpt--timer-event)
(defun chatgpt--timer-event ()
  ;; Stop the process if already running.
  (if (memq chatgpt--process (process-list))
      (kill-process chatgpt--process))
  (let ((tmpbuf (get-buffer-create chatgpt--tmpbuffer-name)))
    (with-current-buffer tmpbuf
      (erase-buffer)
      (insert "Q. " chatgpt--last-query "\n\n")
      (setq chatgpt--process
	    (start-process "chatgpt" tmpbuf chatgpt-prog "-r"))
      (set-process-sentinel chatgpt--process
			    'chatgpt--process-sentinel))))

;; (chatgpt--sched-timer-event)
(defun chatgpt--sched-timer-event ()
  (run-with-timer .2 nil 'chatgpt--timer-event))
