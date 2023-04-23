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
  "This variable is used to specify the file path to the ChatGPT
program executable. The ChatGPT program is responsible for
sending queries to the ChatGPT server and receiving replies. ")

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues.")
    (?s . "Summarize the following in a plain Japanese.")
    (?j . "Translate the following in Japanese in an academic writing style.")
    (?e . "Translate the following in English in an academic writing style.")
    (?p . "Proofread following text and summarize all suggested changes.")
    (?P . "以下の文章の誤りを直して、変更点の一覧を出力して。")
    (?d . "Write docstring for the following code."))
  "A list of prefix codes for ChatGPT queries.

This variable is a list of prefix codes that can be used to
specify the type of query to send to the ChatGPT server. Each
prefix code is paired with a string that describes the
corresponding query.")

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
    ("'.+?'" . font-lock-string-face))
  "A list of font lock keywords for ChatGPT query buffers.

This variable is a list of regular expressions and corresponding
font lock faces for highlighting keywords in ChatGPT query
buffers. The regular expressions in this list match different
types of text in ChatGPT query buffers, such as comments,
headers, items, constants, and strings.  Each regular expression
is paired with a font lock face, which determines the color and
style of the highlighted text.")

(defvar chatgpt-send-query-function 'chatgpt--send-query
  "A low level function to send a query to the ChatGPT server.")

(defvar chatgpt-start-recv-process-function 'chatgpt--start-recv-process
  "A low level function to start a process that receives a reply
  from the ChatGPT server.")

(defvar chatgpt-extract-reply-function 'chatgpt--extract-reply
  "A low level function to retrieve the reply received by the
  process started by `chatgpt-start-recv-process-function`.")

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

;; ----------------------------------------------------------------
;; Low level interfaces.
(defun chatgpt--send-query (query)
  ;; Compose a query in a temporary buffer.
  (with-temp-buffer
    (insert query)
    ;; Provide chromium with the query string.
    (call-process-region (point-min) (point-max) chatgpt-prog
			 nil nil nil "-s")))

(defun chatgpt--start-recv-process (tmpbuf)
  (start-process "chatgpt" tmpbuf chatgpt-prog "-r"))

(defun chatgpt--extract-reply ()
  (let* ((tmpbuf (get-buffer-create chatgpt--tmpbuffer-name)))
    (with-current-buffer tmpbuf
      (buffer-string))))

;; ----------------------------------------------------------------
;; (chatgpt-send-query "which of Emacs or vi is better?")
;; (chatgpt-send-query "what is Emacs's interesting history?")
(defun chatgpt-send-query (query)
  "Send a query to the ChatGPT server via chromium.

This function takes a string argument `query`, which represents
the text of the query to be sent to the ChatGPT server. The query
is passed to the ChatGPT program executable."
  (interactive)
  (funcall chatgpt-send-query-function query)
  (setq chatgpt--last-query query))

;; (chatgpt-query "Emacs")
(defun chatgpt-query (arg)
  "Send a query to the ChatGPT server and monitor the reply.

When called interactively, prompts the user to enter a query. If
the region is active in the buffer, the selected text is used as
the query. Otherwise, the current paragraph at point is extracted
and used as the query.

Optionally, a prefix can be specified by invoking with a prefix
argument. If invoked with C-u C-u, the function sets the query to
'next', indicating that the user wants to continue a previous
query.

Once the query is determined, it is sent to the ChatGPT server,
and the reply is displayed in a separate buffer."
  (interactive "P")
  (let (prefix query)
    (cond ((equal arg '(16))
	   (setq query "続き"))
	  (arg
	   (setq prefix (chatgpt--read-prefix))))
    (unless query
      (setq query
	    (cond (mark-active
		   (buffer-substring-no-properties
		    (region-beginning) (region-end)))
		  ((looking-at "\\w")
		   (thing-at-point 'word))
		  (t
		   (string-trim 
		    (or (thing-at-point 'paragraph) "")))))
      (setq query (read-string "ChatGPT lookup: " query)))
    (chatgpt-send-query (concat prefix query))
    (chatgpt--start-monitor)))

;; (chatgpt-lookup "Emacs")
(defun chatgpt-lookup (query)
  "Prompt the user for a query and send it to the ChatGPT server.

When called interactively, prompts the user to enter a query. If
the cursor is positioned over a word in the buffer, the word is
used as the default query."
  (interactive (list (read-string "ChatGPT lookup: " 
				  (thing-at-point 'word))))
  (chatgpt-send-query query)
  (chatgpt--start-monitor))

(defun chatgpt--read-prefix ()
  "Prompt the user to select a prefix code for a ChatGPT query.

This function prompts the user to enter a prefix code for a
ChatGPT query. The prefix code is used to specify the type of
query to send to the ChatGPT server, and it is selected from a
list of options provided by the `chatgpt-prefix-alist` variable."
  (let* ((ch (read-char
	      "Prefix ([w]hat/[s]ummary/[j]a/[e]n/[p]roof/[P]roof/[d]oc): "))
	 (elem (assoc ch chatgpt-prefix-alist))
	 (prefix (cdr elem)))
    (if prefix
	(concat prefix " ")
      nil)))

;; (chatgpt-insert-reply)
(defun chatgpt-insert-reply ()
  "Insert the most recent ChatGPT reply at the current point.

This function inserts the most recent reply from the ChatGPT
server at the current point in the buffer. "
  (interactive)
  (insert chatgpt--last-reply))

;; ----------------------------------------------------------------
;; (chatgpt--start-monitor)
(defun chatgpt--start-monitor ()
  "Create a buffer for monitoring ChatGPT server replies.

This function creates a new buffer for displaying replies from
the ChatGPT server.  The function then initializes the reply
buffer by enabling word-wrap, and enabling font-lock mode with
the `chatgpt-font-lock-keywords` variable.

The reply buffer is then displayed in a new window. The window is
split to show the reply buffer and the buffer of the current
window."
  (interactive)
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    ;; Initialize the reply buffer.
    (with-current-buffer buf
      (erase-buffer)
      (toggle-word-wrap 1)
      (setq font-lock-defaults
	    '(chatgpt-font-lock-keywords 'keywords-only nil))
      (font-lock-mode 1))
    ;; Display in the other window.
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf))
  ;; Schedule the next timer event.
  (setq chatgpt--timer-count 0)
  (chatgpt--sched-timer-event))

(defun chatgpt--process-sentinel (proc event)
  "Handle the completion of a process retrieving ChatGPT server query.

If the event indicates that the process finished successfully,
the reply from the ChatGPT server is retrieved from the temporary
buffer. If the reply is different from the previous reply, it is
inserted into the reply buffer and displayed to the user.

If it seems that the reply is still being generated, the function
schedules the next timer event using the
`chatgpt--sched-timer-event` function."
  ;; Is process completed?
  (when (string-match-p "finished" event)
    (let* ((reply (funcall chatgpt-extract-reply-function)))
      (if (string= reply chatgpt--last-reply)
	  ;; No update.
	  (setq chatgpt--timer-count (1+ chatgpt--timer-count))
	;; Updated.
	(let ((buf (get-buffer-create chatgpt--buffer-name))
	      last-pos)
	  (with-current-buffer buf
	    (setq last-pos (point))
	    (erase-buffer)
	    (insert reply)
	    (goto-char last-pos)))
	(setq chatgpt--last-reply reply)
	(setq chatgpt--timer-count 0)))
    ;; Schedule next event if it seems reply is in progress.
    (if (< chatgpt--timer-count 20)
	(chatgpt--sched-timer-event))))
  
;; (chatgpt--timer-event)
(defun chatgpt--timer-event ()
  "Run an event that retrieves the reply from the ChatGPT server.

This function starts a new ChatGPT process with the command
`chatgpt-prog -r` and redirects its output to the temporary
buffer.  The function sets a process sentinel that calls
`chatgpt--process-sentinel` when the process ends, to handle the
display of the response.

This function is intended to be called periodically by a timer,
so that new responses are fetched from the ChatGPT program at
regular intervals."
  ;; Stop the process if already running.
  (if (memq chatgpt--process (process-list))
      (kill-process chatgpt--process))
  (let ((tmpbuf (get-buffer-create chatgpt--tmpbuffer-name)))
    (with-current-buffer tmpbuf
      (erase-buffer)
      (insert "Q. " chatgpt--last-query "\n\n")
      (setq chatgpt--process (funcall chatgpt-start-recv-process-function tmpbuf))
      (set-process-sentinel chatgpt--process
			    'chatgpt--process-sentinel))))

;; (chatgpt--sched-timer-event)
(defun chatgpt--sched-timer-event ()
  "Schedule the next timer event to receive the reply from the
ChatGPT server."
  (run-with-timer .2 nil 'chatgpt--timer-event))

(provide 'chatgpt)
