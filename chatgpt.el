;; -*- Emacs-Lisp -*-
;;
;; Interactively access AIs from Emacs without using APIs.
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

;; Add the follwoing lines to ~/.emacs:
;; (autoload 'chatgpt-query "chatgpt" nil t)
;; (autoload 'chatgpt-insert-reply "qutechat" nil t)
;; (autoload 'chatgpt-fill-at-point "chatgpt" nil t)
;; (global-set-key "\C-cq" 'chatgpt-query)
;; (global-set-key "\C-cQ" 'chatgpt-insert-reply)
;; (global-set-key "\C-cf" 'chatgpt-fill-at-point)
;;
;; Usage:
;; C-c q          Send a query near the point.
;; C-u C-c q      Send a query near the point after revising the query in the minibuffer.
;; C-u C-u C-c q  Send a query near the point after selecting a query prefix.
;; C-c Q          Insert the latest reply at the point.  
;; C-u C-c Q      Insert the pair of the latest query and reply at the point.  
;; C-u C-u C-c Q  Replace the query in the buffer with the latest reply.  

(defvar chatgpt-prog "~/src/chatgpt-el/chatgpt")
(defvar chatgpt-prog-api "~/src/chatgpt-el/chatgpt-api")
(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues:")
    (?s . "Summarize the following in Japanese in a plain academic writing style:")
    (?j . "Translate the following in Japanese in a plain academic writing style:")
    (?e . "Translate the following in English in a plain academic writing style:")
    (?p . "Proofread the following and provide a list of changes made in Markdown table:")
    (?r . "Rewrite the following in a plain academic writing style:")))

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
(defvar chatgpt--last-query-pos nil)
(defvar chatgpt--last-query-saved nil)
(defvar chatgpt--last-raw-reply nil)
(defvar chatgpt--last-engine nil)

(defvar chatgpt--process nil)
(defvar chatgpt--monitor-process nil)
(defvar chatgpt--monitor-timer nil)
(defvar chatgpt--monitor-count nil)

;; (chatgpt--update-mode-name "ChatGPT" t "finished")
(defun chatgpt--update-mode-name (engine use-api status)
  (setq mode-name (format "%s%s:%s" engine
			  (if use-api "_API" "")
			  status)))

(defun chatgpt-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'chatgpt-mode)
  ;; Local variables.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(chatgpt-font-lock-keywords 'keywords-only nil))
  (font-lock-mode 1)
  (auto-fill-mode 1))

(defun chatgpt--replace-regexp (regexp newtext)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match newtext))))


;; ---------------- Low level interfaces
;; (chatgpt--read-prefix)
(defun chatgpt--read-prefix ()
  (let* ((ch (read-char
	      "Select [w]hat/[s]ummary/[j]a/[e]n/[p]roof/[r]ewrite: "))
	 (elem (assoc ch chatgpt-prefix-alist))
	 (prefix (cdr elem)))
    prefix))

;; (chatgpt--read-query "")
(defun chatgpt--read-query ()
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
    ;; Remove the preceeding Q.
    (setq query (replace-regexp-in-string "^Q\\. *" "" query))
    ;; Record the region used as the query.
    (setq chatgpt--last-query-pos (list beg end))
    query))

;; (chatgpt--send-query "which of Emacs or vi is better?")
;; (chatgpt--send-query "what is Emacs's interesting history?")
(defun chatgpt--send-query (query &optional engine use-api)
  (chatgpt--save-reply)
  (chatgpt--init-reply engine use-api)
  (chatgpt--start-monitor engine use-api)
  (let ((prog (if use-api chatgpt-prog-api chatgpt-prog))
	(buf (get-buffer-create chatgpt--buffer-name)))
    (setq chatgpt--process (start-process "ChatGPT" buf
					  prog "-e" engine "-s" query)))
  (set-process-filter chatgpt--process 'chatgpt--process-filter)
  (set-process-sentinel chatgpt--process 'chatgpt--process-sentinel)
  (setq chatgpt--last-query query)
  (setq chatgpt--last-engine engine))

(defun chatgpt--process-filter (proc string)
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
  (when (string-match "finished" event)
    (chatgpt--query-finished)))

;; (chatgpt--init-reply)
(defun chatgpt--init-reply (engine use-api)
  (interactive)
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    ;; Initialize the reply buffer.
    (with-current-buffer buf
      (erase-buffer)
      (setq chatgpt--last-query-saved nil)
      (chatgpt-mode)
      (chatgpt--update-mode-name engine use-api "streaming"))
    ;; Display in the other window.
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf)))

;; (chatgpt--extract-raw-reply)
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
  (when (and chatgpt--last-query 
	     (not chatgpt--last-query-saved))
    (let* ((save-silently t)
           (base (format-time-string "%Y%m%d"))
	   (file (format "~/var/log/chatgpt/%s-%s.org" chatgpt--last-engine base)))
      (with-temp-buffer
	(insert "\n** ")
	;; Insert query and reply at the point.
	(chatgpt-insert-reply '(4))
	(write-region (point-min) (point-max) file 'append)))
    (setq chatgpt--last-query-saved t)))


;; ---------------- Reply monitor.
;; (chatgpt--start-monitor)
(defun chatgpt--start-monitor (engine use-api)
  (interactive)
  (when (not use-api)
    ;; Schedule the next timer event.
    (setq chatgpt--monitor-count 0)
    (chatgpt--sched-monitor-event engine)))

;; (chatgpt--sched-monitor-event)
(defun chatgpt--sched-monitor-event (engine)
  (run-with-timer .2 nil 'chatgpt--monitor-event engine))

;; (chatgpt--monitor-event)
(defun chatgpt--monitor-event (engine)
  ;; Stop the process if already running.
  (if (memq chatgpt--monitor-process (process-list))
      (kill-process chatgpt--monitor-process))
  (let ((buf (get-buffer-create chatgpt--raw-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (setq chatgpt--monitor-process
	    (start-process "chatgpt" buf chatgpt-prog "-e" engine "-r"))
      (set-process-sentinel chatgpt--monitor-process
			    'chatgpt--monitor-process-sentinel))))

(defun chatgpt--monitor-format-buffer ()
  ;; Reformat list items.
  (chatgpt--replace-regexp "\n\n\\( *[0-9-] .+?\\)$" "\n\\1")
  ;; Remove emphasis.
  (chatgpt--replace-regexp "\\*\\*\\(.+?\\)\\*\\*" "\\1")
  (chatgpt--replace-regexp "’" "'")
  (chatgpt--replace-regexp "—" "---")
  (chatgpt--replace-regexp "–" "-")
  (chatgpt--replace-regexp "SVG Image\n" ""))

(defun chatgpt--monitor-process-sentinel (proc event)
  ;; Is process completed?
  (when (string-match-p "finished" event)
    (let* ((reply (chatgpt--extract-raw-reply)))
      (if (string= reply chatgpt--last-raw-reply)
	  ;; No update.
	  (setq chatgpt--monitor-count (1+ chatgpt--monitor-count))
	;; Updated.
	;; Save the window start and position.
	(let* ((win (get-buffer-window chatgpt--buffer-name))
               (last-pnt (window-point win))
               (last-start (window-start win)))
	  (with-current-buffer chatgpt--buffer-name
	    (chatgpt--update-mode-name chatgpt--last-engine nil "synching")
	    (erase-buffer)
	    (insert reply)
	    (shr-render-region (point-min) (point-max))
	    (chatgpt--monitor-format-buffer))
	  ;; Restore the window start and position.
          (set-window-point win last-pnt)
          (set-window-start win last-start))
	(setq chatgpt--last-raw-reply reply)
	(setq chatgpt--monitor-count 0)))
    ;; Schedule next event if it seems reply is in progress.
    (if (< chatgpt--monitor-count 50) ;; .2 seconds x 50 = 10 seconds.
	;; Continue
	(chatgpt--sched-monitor-event chatgpt--last-engine)
      ;; Finished
      (chatgpt--query-finished))))

(defun chatgpt--query-finished ()
  (with-current-buffer chatgpt--buffer-name
    (chatgpt--update-mode-name chatgpt--last-engine nil "finished"))
  (chatgpt--save-reply))


;; ---------------- User-level interfaces.
;; (chatgpt-query)
(defun chatgpt-query (arg &optional engine use-api)
  (interactive "P")
  (setq engine (or engine "ChatGPT"))
  (let ((prefix "")
	(query (chatgpt--read-query)))
    (cond ((equal arg '(16))
	   (setq prefix (chatgpt--read-prefix)))
	  (arg
	   (setq query (read-string (format "%s query: " engine) query))))
    (chatgpt--send-query (concat prefix query) engine use-api)))

;; (chatgpt-insert-reply)
(defun chatgpt-insert-reply (arg)
  (interactive "P")
  (let ((reply (string-trim (chatgpt--extract-reply)))
	(beg (nth 0 chatgpt--last-query-pos))
	(end (nth 1 chatgpt--last-query-pos)))
    (cond 
     ;; With C-u C-u prefix, replace the query with the reply.
     ((equal arg '(16))
      (delete-region beg end)
      (insert reply))
     ;; With C-u, insert the query and the reply.
     (arg
      (insert "Q. " chatgpt--last-query "\n\n")
      (insert "A. " reply))
     (t
      (insert reply)))))

;; (chatgpt-fill-at-point)
(defun chatgpt-fill-at-point (&optional engine use-api)
  (interactive)
  (setq engine (or engine "ChatGPT"))
  (let* ((pnt (point))
	 (buf (buffer-string))
	 (prefix "Write sentence(s) or program at __FILL_THIS_PART__ in the following text or program.  Only write sentence(s) or program that fits in __FILL_THIS_PART__ without any unnecessary elements before or after: ")
	 (query (concat (substring buf 0 (1- pnt))
			"__FILL_THIS_PART__"
			(substring buf (1- pnt)))))
    (chatgpt--send-query (concat prefix query) engine use-api)))

;; ;; (chatgpt-restart-monitor)
;; (defun chatgpt-restart-monitor ()
;;   (interactive)
;;   (chatgpt--start-monitor))

(provide 'chatgpt)
