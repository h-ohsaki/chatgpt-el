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
program executable. The default value
is "~/src/chatgpt-el/chatgpt", but it can be customized by
setting the variable to a different file path.

The ChatGPT program is responsible for sending queries to the
ChatGPT server and receiving replies. This variable is used in
conjunction with the `chatgpt-send-query` function to execute the
ChatGPT program and retrieve the server's response.

Note that the ChatGPT program is not included with this package
and must be installed separately.")

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
corresponding query.

The prefix codes are single characters, such as ?w, ?s, ?j, ?e,
?p, ?P, and ?d.  When sending a query to the ChatGPT server, a
prefix code can be specified using the `chatgpt--read-prefix`
function, which prompts the user to select a prefix code from the
list. The corresponding string is then added to the query before
sending it to the server.

The prefix codes and strings in this list are customizable and
can be modified to suit the user's needs.")

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
buffers. Font lock is a mode for highlighting text in Emacs
buffers, and it is used in this variable to provide syntax
highlighting for the different parts of ChatGPT queries.

The regular expressions in this list match different types of
text in ChatGPT query buffers, such as comments, headers, items,
constants, and strings.  Each regular expression is paired with a
font lock face, which determines the color and style of the
highlighted text.

The font lock keywords in this list are customizable and can be
modified to suit the user's preferences.")

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
;; (chatgpt-send-query "which of Emacs or vi is better?")
;; (chatgpt-send-query "what is Emacs's interesting history?")
(defun chatgpt-send-query (query)
  "Send a query to the ChatGPT server via chromium.

This function takes a string argument `query`, which represents
the text of the query to be sent to the ChatGPT server. The query
is passed to the ChatGPT program executable, which is responsible
for sending the query to the server and receiving a response.

The ChatGPT program is executed using the `call-process-region`
function, which provides chromium with the query string. The `-s`
option is used to specify that the query should be sent to the
ChatGPT server.

Once the query has been sent, the function sets the
`chatgpt--last-query` variable to the query string, which is used
to display the previous query when continuing a previous
conversation."
  (interactive)
  ;; Compose a query in a temporary buffer.
  (with-temp-buffer
    (insert query)
    ;; Provide chromium with the query string.
    (call-process-region (point-min) (point-max) chatgpt-prog
			 nil nil nil "-s"))
  (setq chatgpt--last-query query))

;; (chatgpt-query "Emacs")
(defun chatgpt-query (arg)
  "Send a query to the ChatGPT server and monitor the reply.

When called interactively, prompts the user to enter a query. If
the region is active in the buffer, the selected text is used as
the query. Otherwise, the current paragraph at point is extracted
and used as the query.

Optionally, a prefix can be specified by calling the
`chatgpt--read-prefix` function. If the argument is equal to the
symbol `(16)`, the function sets the query to 'next', indicating
that the user wants to continue a previous query.

Once the query is determined, it is sent to the ChatGPT server
using the `chatgpt-send-query` function, and the reply is
displayed in a separate buffer using the `chatgpt--start-monitor`
function."
  (interactive "P")
  (let (prefix query)
    (cond ((equal arg '(16))
	   (setq query "続き"))
	  (arg
	   (setq prefix (chatgpt--read-prefix))))
    (unless query
      (if mark-active
	  (setq query (buffer-substring-no-properties
		       (region-beginning) (region-end)))
	;; When mark is inactive.
	(let ((paragraph (string-trim
			  (or (thing-at-point 'paragraph) ""))))
	  (setq query (read-string "ChatGPT lookup: " paragraph)))))
    (chatgpt-send-query (concat prefix query))
    (chatgpt--start-monitor)))

;; (chatgpt-lookup "Emacs")
(defun chatgpt-lookup (query)
  "Prompt the user for a query and send it to the ChatGPT server.

When called interactively, prompts the user to enter a query. If
the cursor is positioned over a word in the buffer, the word is
used as the default query.

Once the query is determined, it is sent to the ChatGPT server
using the `chatgpt-send-query` function, and the reply is
displayed in a separate buffer using the `chatgpt--start-monitor`
function."
  (interactive (list (read-string "ChatGPT lookup: " 
				  (thing-at-point 'word))))
  (chatgpt-send-query query)
  (chatgpt--start-monitor))

(defun chatgpt--read-prefix ()
  "Prompt the user to select a prefix code for a ChatGPT query.

This function prompts the user to enter a prefix code for a
ChatGPT query. The prefix code is used to specify the type of
query to send to the ChatGPT server, and it is selected from a
list of options provided by the `chatgpt-prefix-alist` variable.

If the user enters an invalid prefix code, the function returns
`nil`."
  (let* ((ch (read-char "Prefix ([w]hat/[s]ummary/[j]a/[e]n/[p]roof/[P]roof/[d]oc): "))
	 (elem (assoc ch chatgpt-prefix-alist))
	 (prefix (cdr elem)))
    (if prefix
	(concat prefix " ")
      nil)))

;; (chatgpt-insert-reply)
(defun chatgpt-insert-reply ()
  "Insert the most recent ChatGPT reply at the current point.

This function inserts the most recent reply from the ChatGPT
server at the current point in the buffer. The reply is stored in
the `chatgpt--last-reply` variable, which is set by the
`chatgpt-receive-reply` function when a reply is received from
the server.

If there is no previous reply, the function does nothing."
  (interactive)
  (insert chatgpt--last-reply))

;; ----------------------------------------------------------------
;; (chatgpt--start-monitor)
(defun chatgpt--start-monitor ()
  "Create a buffer for monitoring ChatGPT server replies.

This function creates a new buffer for displaying replies from
the ChatGPT server. The buffer is created using the
`get-buffer-create` function, and its name is specified by the
`chatgpt--buffer-name` variable.

The function then initializes the reply buffer by erasing its
contents, enabling word-wrap, and enabling font-lock mode with
the `chatgpt-font-lock-keywords` variable. The font-lock keywords
specify how different parts of the reply text should be
highlighted for improved readability.

The reply buffer is then displayed in a new window using the
`split-window` and `set-window-buffer` functions. The window is
split to show the reply buffer and the buffer of the current
window.

Finally, the function schedules the next timer event by setting
the `chatgpt--timer-count` variable to 0 and calling the
`chatgpt--sched-timer-event` function."
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
  "Process the completion of a ChatGPT server query.

This function is called by the `start-process` function to
monitor the completion of a ChatGPT server query. When the
process completes, the `event` argument contains a string
indicating the completion status.

If the event indicates that the process finished successfully,
the reply from the ChatGPT server is retrieved from the temporary
buffer and stored in the `chatgpt--last-reply` variable. If the
reply is different from the previous reply, it is inserted into
the reply buffer and displayed to the user. If the reply is the
same as the previous reply, the function increments the
`chatgpt--timer-count` variable.

If the `chatgpt--timer-count` variable is less than 10,
indicating that the reply is still being generated, the function
schedules the next timer event using the
`chatgpt--sched-timer-event` function."
  ;; Is process completed?
  (when (string-match-p "finished" event)
    (let* ((tmpbuf (get-buffer-create chatgpt--tmpbuffer-name))
	   (reply (with-current-buffer tmpbuf
		    (buffer-string))))
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
    (if (< chatgpt--timer-count 10)
	(chatgpt--sched-timer-event))))

;; (chatgpt--timer-event)
(defun chatgpt--timer-event ()
  "Send a query to the ChatGPT server and schedule the next timer event.

This function sends a query to the ChatGPT server by starting a
new process with the `start-process` function. The query is taken
from the `chatgpt--last-query` variable, which should contain the
most recent query sent by the user.

The process output is captured in a temporary buffer created with
the `get-buffer-create` function. The buffer is initialized with
a header that includes the query and two newline characters.

The `set-process-sentinel` function is used to register a
sentinel function, which is called when the process
completes. The sentinel function is specified as
`chatgpt--process-sentinel`.

The function then schedules the next timer event by setting the
`chatgpt--timer-count` variable to 0 and calling the
`chatgpt--sched-timer-event` function.

If a ChatGPT query is already in progress, the function stops the
previous process with the `kill-process` function before starting
a new one."
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
  "Schedule the next timer event to send a query to the ChatGPT server.

This function uses the `run-with-timer` function to schedule the
`chatgpt--timer-event` function to be called after 0.2
seconds. The `chatgpt--timer-event` function sends the query to
the ChatGPT server and schedules the next timer event if
necessary."
  (run-with-timer .2 nil 'chatgpt--timer-event))
