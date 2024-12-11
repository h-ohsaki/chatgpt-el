;; -*- Emacs-Lisp -*-
;;
;; Access ChatGTP from Emacs without OpenAI API.
;; Copyright (C) 2023-2024 Hiroyuki Ohsaki.
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

;; (setq chatgpt-prog "~/src/chatgpt-el/chatgpt2")
;; (setq chatgpt-prog "~/src/chatgpt-el/gemini")
(defvar chatgpt-prog "~/src/chatgpt-el/chatgpt2")

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

(defvar chatgpt--buffer-name "*ChatGPT reply*")
(defvar chatgpt--last-query nil)
(defvar chatgpt-query-complete-hooks 'chatgpt-save-reply)
(defvar chatgpt--last-query-beg nil)
(defvar chatgpt--last-query-end nil)
(defvar chatgpt--process nil)

;; ----------------------------------------------------------------
;; (chatgpt-send-query "which of Emacs or vi is better?")
;; (chatgpt-send-query "what is Emacs's interesting history?")
(defun chatgpt-send-query (query)
  (interactive)
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    ;; Initialize the reply buffer.
    (with-current-buffer buf
      (erase-buffer)
      (setq word-wrap t)
      (setq font-lock-defaults
	    '(chatgpt-font-lock-keywords 'keywords-only nil))
      (font-lock-mode 1)
      ;;
      (setq chatgpt--process (start-process "foo" buf chatgpt-prog "-q" query))
      (set-process-sentinel chatgpt--process
			    'chatgpt--process-sentinel))
    ;; Display in the other window.
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf))
  (setq chatgpt--last-query query))

(defun chatgpt--process-sentinel (proc event)
  ;; Is process completed?
  (when (string-match-p "finished" event)
    (run-hooks 'chatgpt-query-complete-hooks)))

(defun chatgpt-extract-reply ()
  (with-current-buffer chatgpt--buffer-name
    (buffer-string)))

;; ----------------------------------------------------------------
;; (chatgpt-lookup "Emacs")
(defun chatgpt-lookup (query)
  (interactive (list (read-string "ChatGPT lookup: " 
				  (thing-at-point 'word))))
  (chatgpt-send-query query))

(defun chatgpt--read-prefix ()
  (let* ((ch (read-char
	      "Prefix ([w]hat/[s]ummary/[j]a/[e]n/[p]roof/[P]roof/[r]ewrite/{R]ewrite/[d]oc): "))
	 (elem (assoc ch chatgpt-prefix-alist))
	 (prefix (cdr elem)))
    (if prefix
	(concat prefix " ")
      nil)))

;; (chatgpt-query "Emacs")
(defun chatgpt-query (arg)
  (interactive "P")
  (let ((prefix "")
	query beg-pos end-pos)
    (cond ((equal arg '(16))
	   (setq query "続き"))
	  (arg
	   (setq prefix (chatgpt--read-prefix))))
    (unless query
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
      (setq query (read-string "ChatGPT query: " (concat prefix query))))
    ;; Record the region used as the query.
    (setq chatgpt--last-query-beg beg-pos)
    (setq chatgpt--last-query-end end-pos)
    (chatgpt-send-query (concat prefix query))))

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
    (insert (string-trim (chatgpt-extract-reply)))))

;; (chatgpt-save-reply)
(defun chatgpt-save-reply ()
  (interactive)
  (let ((save-silently t)
	(logfile "~/.chatgpt-log.org"))
    (with-temp-buffer
      (insert "\n\n")
      (chatgpt-insert-reply '(4))
      (write-region (point-min) (point-max) logfile 'append))))

(provide 'chatgpt)