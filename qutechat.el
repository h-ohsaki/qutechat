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

(defvar qutechat-prog "~/src/qutechat/chat-proxy"
  "The name of the qutebrowser userscript.")

;; FIXME: Avoid hard-coding.
;; FIXME: These files must be readable only by the owner.
(defvar qutechat--query-file "/tmp/.qutechat-query")
(defvar qutechat--reply-file "/tmp/.qutechat-reply")

;; ----------------------------------------------------------------
;; Low level interfaces.
(defun qutechat--send-query (query)
  ;; Compose a query in a temporary buffer.
  (with-temp-buffer
    (insert query)
    ;; Remove all newlines.
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match " "))
    ;; Provide chromium with the query string.
    (let ((inhibit-message t))
      (write-region (point-min) (point-max) qutechat--query-file))
    ;; Ask the qutebrowser to fill and send the query string.
    (call-process "qutebrowser" nil nil nil 
		  (format ":spawn -m -u %s -s %s"
			  qutechat-prog qutechat--query-file))))

;; (qutechat--start-recv-process nil)
(defun qutechat--start-recv-process (tmpbuf)
  (start-process "qutechat" tmpbuf "qutebrowser"
		 (format ":spawn -m -u %s -r %s"
			 qutechat-prog qutechat--reply-file)))

;; (qutechat--extract-reply)
(defun qutechat--extract-reply ()
  (with-temp-buffer
    (insert "Q. " chatgpt--last-query "\n\n")
    (insert-file-contents qutechat--reply-file)
    (goto-char (point-min))
    (while (search-forward "·" nil t) ;; 0xb7
      (replace-match "."))
    (buffer-string)))

;; ----------------------------------------------------------------
;; FIXME: Should  restore original values if necessary.
(setq chatgpt-send-query-function 'qutechat--send-query)
(setq chatgpt-start-recv-process-function 'qutechat--start-recv-process)
(setq chatgpt-extract-reply-function 'qutechat--extract-reply)
