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

(defvar chatgpt--process nil
  "The process of the external program that continuously
  retrieves the reply from ChatGTP.")

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues.")
    (?s . "Summarize the following in a plain Japanese.")
    (?j . "Translate the following in Japanese in an academic writing style.")
    (?e . "Translate the following in English in an academic writing style.")
    (?p . "Proofread following text and summarize all suggested changes.")
    (?P . "以下の文章の誤りを直して、変更点の一覧を出力して。")))

;; (chatgpt-send-string "which of Emacs or vi is better?")
;; (chatgpt-send-string "what is Emacs's interesting history?")
(defun chatgpt-send-string (str &optional prefix)
  "Send a query string STR to ChatGPT via chromium."
  (interactive)
  ;; Compose a query string in a temporary buffer.
  (with-temp-buffer
    (when prefix
      (insert prefix "\n"))
    (insert str)
    (goto-char (point-min))
    ;; FIXME: Should preserve all newlines.
    (while (search-forward "\n" nil t)
      (replace-match " "))
    ;; Provide chromium with the query string.
    (call-process-region (point-min) (point-max) chatgpt-prog
			 nil nil nil "-s")))
  
(defun chatgpt-send-region (start end &optional prefix)
  "Send the region between START and END to ChatGPT via Chromium."
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (chatgpt-send-string str prefix)))

;; (chatgpt--current-paragraph)
(defun chatgpt--current-paragraph ()
  (buffer-substring-no-properties
   (save-excursion
     (forward-paragraph -1)
     (point))
   (save-excursion
     (forward-paragraph 1)
     (point))))

(defun chatgpt-send (arg)
  "Send the query around the point to ChatGPT via Chromium.  If
the mark is active, send the hlghlighted region as a query."
  (interactive "P")
  (let (prefix str)
    (when arg
      (let* ((ch (read-char "Prefix [w]what/[s]ummary/[j]apanese/[e]nglish/[p/P]roofread: "))
	     (val (assoc ch chatgpt-prefix-alist)))
	(setq prefix (cdr val))))
    (if mark-active
	(chatgpt-send-region (region-beginning) (region-end) prefix)
      ;; When mark is inactive.
      (setq str (chatgpt--current-paragraph))
      (chatgpt-send-string str prefix))
    (chatgpt-start-monitor)))

;; (chatgpt-start-monitor)
(defun chatgpt-start-monitor ()
  "Start a monitor to watch the output (i.e., reply) from
ChatGPT."
  (interactive)
  (let ((buf (get-buffer-create chatgpt-buffer-name)))
    ;; Prepare and display the reply buffer.
    (with-current-buffer buf
      (visual-line-mode 1)
      (erase-buffer)
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
	str)
    (with-current-buffer buf
      (setq str (buffer-string)))
    (insert "A. " str)))
