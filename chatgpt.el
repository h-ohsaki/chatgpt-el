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

(defvar chatgpt-buffer-name "*ChatGPT reply*"
  "The name of the buffer to display the response from ChatGPT.")

(defvar chatgpt--last-reply nil
  "The last reply returned by the server.")

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues.")
    (?s . "Summarize the following in a plain Japanese.")
    (?j . "Translate the following in Japanese in an academic writing style.")
    (?e . "Translate the following in English in an academic writing style.")
    (?p . "Proofread following text and summarize all suggested changes.")
    (?P . "以下の文章の誤りを直して、変更点の一覧を出力して。")))

(defvar chatgpt--process nil
  "The process for receiving the stream of response from the server.")

;; (chatgpt-send-string "which of Emacs or vi is better?")
;; (chatgpt-send-string "what is Emacs's interesting history?")
(defun chatgpt-send-string (query)
  "Send a query string QUERY to ChatGPT via chromium."
  (interactive)
  ;; Compose a query string in a temporary buffer.
  (with-temp-buffer
    (insert query)
    ;; FIXME: Should preserve all newlines.
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match " "))
    ;; Provide chromium with the query string.
    (call-process-region (point-min) (point-max) chatgpt-prog
			 nil nil nil "-s")))

;; (chatgpt--current-paragraph)
(defun chatgpt--current-paragraph ()
  (buffer-substring-no-properties
   (save-excursion
     (forward-paragraph -1)
     (if (looking-at "$")
	 (forward-line 1))
     (point))
   (save-excursion
     (forward-paragraph 1)
     (if (looking-at "$")
	 (forward-char -1))
     (point))))

(defun chatgpt-send (arg)
  "Send the query around the point to ChatGPT via Chromium.  If
the mark is active, send the hlghlighted region as a query."
  (interactive "P")
  (let* ((query (if mark-active
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  ;; When mark is inactive.
		  (chatgpt--current-paragraph)))
	 (buf (get-buffer-create chatgpt-buffer-name)))
    ;; Change behavior when prefix is provided.
    (cond ((equal arg '(16))
	   (setq query "続き"))
	  (arg
	   (let* ((ch (read-char "Prefix [w]what/[s]ummary/[j]apanese/[e]nglish/[p/P]roofread: "))
		  (val (assoc ch chatgpt-prefix-alist)))
	     (setq query (concat (cdr val) query)))))
    (chatgpt-send-string query)
    (chatgpt-start-monitor query)))

;; (chatgpt-start-monitor)
(defun chatgpt-start-monitor (query)
  "Start a monitor to watch the output (i.e., reply) from
ChatGPT."
  (interactive)
  (let ((buf (get-buffer-create chatgpt-buffer-name)))
    ;; Prepare and display the reply buffer.
    (with-current-buffer buf
      (erase-buffer)
      (insert "Q. " query "\n\n")
      (setq chatgpt--process (start-process "chatgpt" buf chatgpt-prog "-r"))
      (set-process-sentinel chatgpt--process #'ignore)
      ;;
      (delete-other-windows)
      (split-window)
      (set-window-buffer (next-window) buf))))

;; (chatgpt-insert-reply)
(defun chatgpt-insert-reply ()
  "At the current point, insert the response for the last query
from ChatGPT."
  (interactive)
  (let ((buf (get-buffer-create chatgpt-buffer-name))
        reply)
    (with-current-buffer buf
      (setq reply (buffer-string)))
    (insert reply)))
