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

(require 'chatgpt)

(defvar llama-prog "/src/llama.cpp/main"
  "The name of the qutebrowser userscript.")

;; ----------------------------------------------------------------
;; Low level interfaces.
;; (llama--send-query "what is Emacs?")
(defun llama--send-query (query)
  (let ((buf (get-buffer-create chatgpt--buffer-name)))
    (make-process :name "chatgpt"
		  :buffer buf
		  :command
		  (list llama-prog
			"--model" "/src/alpaca.cpp/ggml-alpaca-7b-q4.bin"
			"-p" (concat query "\n")
			"-t" "8"
			"-n" "128")
		  :connection-type 'pipe
		  ;; :stderr (get-buffer-create "*tmp*")
		  )))

;; (llama--start-recv-process nil)
(defun llama--start-recv-process (tmpbuf)
  nil)

;; (llama--extract-reply)
(defun llama--extract-reply ()
  nil)

;; ----------------------------------------------------------------
;; FIXME: Should  restore original values if necessary.
(setq chatgpt-send-query-function 'llama--send-query)
(setq chatgpt-start-recv-process-function 'llama--start-recv-process)
(setq chatgpt-extract-reply-function 'llama--extract-reply)
