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

(defvar qutechat-proxy-prog "~/src/qutechat/chat-proxy"
  "The name of the qutebrowser userscript.")

;; FIXME: Avoid hard-coding.
(defvar qutechat-tmpfile "/tmp/qutechat.tmp")

;; (qutechat-send-string "which of Emacs or vi is better?")
;; (qutechat-send-string "what is Emacs's interesting history?")
(defun qutechat-send-string (str)
  "Send a query string STR to a Web-based chat via qutebrowser."
  (interactive)
  ;; Save the query string to a temporary file.
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    ;; FIXME: Should preserve all newlines.
    (while (search-forward "\n" nil t)
      (replace-match " "))
    (write-region (point-min) (point-max) qutechat-tmpfile))
  ;; Ask the qutebrowser to fill and send the query string.
  (shell-command (format "qutebrowser ':spawn -m -u %s -s %s'"
			 qutechat-proxy-prog qutechat-tmpfile)))
  
(defun qutechat-send-region (start end)
  "Send the region between START and END to a Web-based chat."
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (qutechat-send-string str)))

;; (qutechat--get-block-string)
(defun qutechat--get-block-string ()
  (buffer-substring-no-properties
   (save-excursion
     (beginning-of-line)
     (while (and (not (bobp))
		 (not (looking-at "\s*$")))
       (forward-line -1))
     (point))
   (save-excursion
     (beginning-of-line)
     (while (and (not (eobp))
		 (not (looking-at "\s*$")))
       (forward-line 1))
     (point))))

(defun qutechat-send ()
  "Send the query sentence(s) around the point to a Web-based
chat.  If the mark is active, send the marked region as the query
sentence(s)."
  (interactive)
  (let ((str))
    (if mark-active
	(qutechat-send-region (region-beginning) (region-end))
      ;; When mark is inactive.
      (setq str (qutechat--get-block-string))
      (qutechat-send-string str))))

;; (qutechat-parse-reply)
(defun qutechat-parse-reply ()
  "Check the response for the last query from a Web-based chat
and return it as a string."
  (interactive)
  ;; Retrieve the response for the last query.
  (shell-command (format "qutebrowser ':spawn -m -u %s -r %s'"
			 qutechat-proxy-prog qutechat-tmpfile))
  (sit-for .1)
  (with-temp-buffer
    ;; FIXME: This code assumes the first line is the query.
    (insert "Q. ")
    (insert-file-contents qutechat-tmpfile)
    (buffer-string)))

;; (qutechat-insert-reply)
(defun qutechat-insert-reply ()
  "Insert the response for the last query from a Web-based
chat at the point."
  (interactive)
  (let ((reply (qutechat-parse-reply)))
    (insert reply)))
