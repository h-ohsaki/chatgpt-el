;; -*- Emacs-Lisp -*-
;;
;; Interactively access generative AIs from Emacs without APIs.
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

(defvar chatgpt-prog "~/src/chatgpt-el/chatgpt")
(defvar chatgpt-engine "ChatGPT")
(defvar chatgpt-engines-alist '((?1 . "ChatGPT")
				(?2 . "Gemini")
				(?3 . "Claude")
				(?4 . "DeepSeek")))

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues:")
    (?s . "Summarize the following in a plain Japanese:")
    (?j . "Translate the following in Japanese in a plain academic writing style:")
    (?e . "Translate the following in English in a plain academic writing style:")
    (?p . "Proofread following text:")
    (?P . "以下の文章の誤りを直して、変更点の一覧を出力して:")
    (?r . "Rewrite the following in a plain academic writing style:")
    (?R . "###以下の文章を、1章の内容に合うように修正して。用語を1章のものに統一して。論理がわかりづらい箇所は明快な論理に書き替えて。平易で学術的な表現の英語にして。LaTeXのコマンドはそのままにして。\citeの前の空白はすべて~に変更して。###")
    (?d . "Write docstring for the following code:")))

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
    ("【.+?】" . font-lock-constant-face)
    ("「.+?」" . font-lock-constant-face)
    ("\".+?\"" . font-lock-string-face)
    ("'.+?'" . font-lock-string-face)))

(defvar chatgpt-query-complete-hooks 'chatgpt--save-reply)

(defvar chatgpt--buffer-name "*ChatGPT reply*")
(defvar chatgpt--raw-buffer-name "*ChatGPT raw*")

(defvar chatgpt--last-query nil)
(defvar chatgpt--last-query-beg nil)
(defvar chatgpt--last-query-end nil)
(defvar chatgpt--last-raw-reply nil)
(defvar chatgpt--process nil)
(defvar chatgpt--timer nil)
(defvar chatgpt--timer-count nil)

(defun chatgpt-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'chatgpt-mode)
  (setq mode-name "ChatGPT")
  ;; Local variables.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(chatgpt-font-lock-keywords 'keywords-only nil))
  (font-lock-mode 1))
  

;; ---------------- Low level interfaces
;; (chatgpt--read-prefix)
(defun chatgpt--read-prefix ()
  (let* ((ch (read-char
	      "Prefix ([w]hat/[s]ummary/[j]a/[e]n/[p]roof/[P]roof/[r]ewrite/{R]ewrite/[d]oc): "))
	 (elem (assoc ch chatgpt-prefix-alist))
	 (prefix (cdr elem)))
    (if prefix
	(concat prefix " ")
      nil)))

;; (chatgpt--read-query "")
(defun chatgpt--read-query (prefix)
  (let (beg-pos end-pos query)
    (cond
     ;; Region is selected.
     (mark-active
      (setq beg-pos (region-beginning))
      (setq end-pos (region-end))
      (setq query (buffer-substring-no-properties beg-pos end-pos)))
     ;; A character follows the point.
     ((looking-at "\\w")
      (setq query (thing-at-point 'word)))
     ;; Possibly, at the end line.
     (t
      (setq query (string-trim 
		   (or (thing-at-point 'paragraph) "")))
      (setq beg-pos (save-excursion
		      (search-backward query nil t)
		      (point)))
      (setq end-pos (point))))
    ;; Remove the preceeding Q.
    (setq query (replace-regexp-in-string "^Q\\. *" "" query))
    (setq query (read-string (format "%s query: " chatgpt-engine)
			     (concat prefix query)))
    ;; Record the region used as the query.
    (setq chatgpt--last-query-beg beg-pos)
    (setq chatgpt--last-query-end end-pos)
    query))

;; (chatgpt--send-query "which of Emacs or vi is better?")
;; (chatgpt--send-query "what is Emacs's interesting history?")
(defun chatgpt--send-query (query)
  (chatgpt--start-monitor)
  (call-process chatgpt-prog nil chatgpt--buffer-name nil "-e" chatgpt-engine "-s" query)
  (setq chatgpt--last-query query))

;; (chatgpt--extract-reply)
(defun chatgpt--extract-raw-reply ()
    (with-current-buffer chatgpt--raw-buffer-name
      (buffer-string)))

;; (chatgpt--extract-reply)
(defun chatgpt--extract-reply ()
    (with-current-buffer chatgpt--buffer-name
      (buffer-string)))

;; (chatgpt--save-reply)
(defun chatgpt--save-reply ()
  (interactive)
  (let* ((save-silently t)
	 (file (format "~/.chatgpt-%s.org" chatgpt-engine)))
    (with-temp-buffer
      (insert "\n** ")
      (chatgpt-insert-reply '(4))
      (write-region (point-min) (point-max) file 'append))))

;; ---------------- Reply monitor.
;; (chatgpt--start-monitor)
(defun chatgpt--start-monitor ()
  (interactive)
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    ;; Initialize the reply buffer.
    (with-current-buffer buf
      (chatgpt-mode)
      (setq mode-name (format "%s waiting" chatgpt-engine)))
    ;; Display in the other window.
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf))
  ;; Schedule the next timer event.
  (setq chatgpt--timer-count 0)
  (chatgpt--sched-timer-event))

;; (chatgpt--sched-timer-event)
(defun chatgpt--sched-timer-event ()
  (run-with-timer 1. nil 'chatgpt--timer-event))

;; (chatgpt--timer-event)
(defun chatgpt--timer-event ()
  ;; Stop the process if already running.
  (if (memq chatgpt--process (process-list))
      (kill-process chatgpt--process))
  (let ((buf (get-buffer-create chatgpt--raw-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (setq chatgpt--process (start-process "chatgpt" buf chatgpt-prog "-r"))
      (set-process-sentinel chatgpt--process 'chatgpt--process-sentinel))))

(defun chatgpt--process-sentinel (proc event)
  ;; Is process completed?
  (when (string-match-p "finished" event)
    (let* ((reply (chatgpt--extract-raw-reply)))
      (if (string= reply chatgpt--last-raw-reply)
	  ;; No update.
	  (setq chatgpt--timer-count (1+ chatgpt--timer-count))
	;; Updated.
	;; Save the window start and position.
	(let* ((win (get-buffer-window chatgpt--buffer-name))
               (last-pnt (window-point win))
               (last-start (window-start win)))
	  (with-current-buffer chatgpt--buffer-name
	    (setq mode-name (format "%s replying" chatgpt-engine))
	    (erase-buffer)
	    (insert reply)
	    (shr-render-region (point-min) (point-max)))
	  ;; Restore the window start and position.
          (set-window-point win last-pnt)
          (set-window-start win last-start))
	(setq chatgpt--last-raw-reply reply)
	(setq chatgpt--timer-count 0)))
    ;; Schedule next event if it seems reply is in progress.
    (if (< chatgpt--timer-count 10)
	;; Continue
	(chatgpt--sched-timer-event)
      ;; Finished
      (with-current-buffer chatgpt--buffer-name
	(setq mode-name (format "%s finished" chatgpt-engine)))
      (run-hooks 'chatgpt-query-complete-hooks))))

;; ---------------- User-level interfaces.
;; (chatgpt-select-engine)
(defun chatgpt-select-engine ()
  (interactive)
  (let ((key (read-char-choice
	      "Select engine (1: ChatGPT, 2: Gemini, 3: Claude, 4: DeepSeek): "
	      (mapcar #'car chatgpt-engines-alist))))
    (setq chatgpt-engine (cdr (assoc key chatgpt-engines-alist)))))

;; (chatgpt-query "Emacs")
(defun chatgpt-query (arg)
  (interactive "P")
  (let ((prefix "")
	query)
    (cond ((equal arg '(16))
	   (chatgpt-select-engine))
	  (arg
	   (setq prefix (chatgpt--read-prefix))))
    (unless query
      (setq query (chatgpt--read-query prefix)))
    (chatgpt--send-query (concat prefix query))))

;; (chatgpt-lookup "Emacs")
(defun chatgpt-lookup (query)
  (interactive
   (list (read-string (format "%s lookup: " chatgpt-engine)
		      (thing-at-point 'word))))
  (chatgpt--send-query query))

;; (chatgpt-insert-reply nil)
;; (chatgpt-insert-reply t)
(defun chatgpt-insert-reply (&optional arg)
  (interactive "P")
  ;; With C-u C-u prefix, delete the region used as the query.
  (when (and (equal arg '(16))
	     (< chatgpt--last-query-end (point-max)))
    (delete-region chatgpt--last-query-beg chatgpt--last-query-end))
  (save-restriction
    (narrow-to-region (point) (point))
    ;; With C-u, tweak the reply for better readability.
    (when (equal arg '(4))
      (insert "Q. " chatgpt--last-query "\n\n")
      (insert "A. "))
    (insert (string-trim (chatgpt--extract-reply)))))

(provide 'chatgpt)
