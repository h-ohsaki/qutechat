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

(defvar qutechat-prog "~/src/qutechat/chat-proxy"
  "The name of the qutebrowser userscript.")

(defvar qutechat-buffer-name "*Qutechat reply*"
  "The name of the buffer to display the response from Web chat.")

;; FIXME: Avoid hard-coding.
(defvar qutechat-tmpfile "/tmp/qutechat.tmp")

(defvar qutechat--last-reply nil
  "The last reply returned by the server.")

(defvar chatgpt-prefix-alist
  '((?w . "Explain the following in Japanese with definition, pros, cons, examples, and issues.")
    (?s . "Summarize the following in a plain Japanese.")
    (?j . "Translate the following in Japanese in an academic writing style.")
    (?e . "Translate the following in English in an academic writing style.")
    (?p . "Proofread following text and summarize all suggested changes.")
    (?P . "以下の文章の誤りを直して、変更点の一覧を出力して。")))

(defvar qutechat--timer nil
  "A timer event to retriee the response from the server.")

(defvar qutechat--timer-count nil)

;; (qutechat-send-string "which of Emacs or vi is better?")
;; (qutechat-send-string "what is Emacs's interesting history?")
(defun qutechat-send-string (str &optional prefix)
  "Send a query string STR to a Web-based chat via qutebrowser."
  (interactive)
  ;; Save the query string to a temporary file.
  (with-temp-buffer
    (when prefix
      (insert prefix "\n"))
    (insert str)
    (goto-char (point-min))
    ;; FIXME: Should preserve all newlines.
    (while (search-forward "\n" nil t)
      (replace-match " "))
    (let ((inhibit-message t))
      (write-region (point-min) (point-max) qutechat-tmpfile)))
  ;; Ask the qutebrowser to fill and send the query string.
  (call-process "qutebrowser" nil nil nil 
		(format ":spawn -m -u %s -s %s" qutechat-prog qutechat-tmpfile)))
  
(defun qutechat-send-region (start end &optional prefix)
  "Send the region between START and END to a Web-based chat."
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (qutechat-send-string str prefix)))

;; (qutechat--current-paragraph)
(defun qutechat--current-paragraph ()
  (buffer-substring-no-properties
   (save-excursion
     (forward-paragraph -1)
     (point))
   (save-excursion
     (forward-paragraph 1)
     (point))))

(defun qutechat-send (arg)
  "Send the query sentence(s) around the point to a Web-based
chat.  If the mark is active, send the marked region as the query
sentence(s)."
  (interactive "P")
  (let (prefix str buf)
    (when arg
      (let* ((ch (read-char "Prefix [w]what/[s]ummary/[j]apanese/[e]nglish/[p/P]roofread: "))
	     (val (assoc ch chatgpt-prefix-alist)))
	(setq prefix (cdr val))))
    (if mark-active
	(qutechat-send-region (region-beginning) (region-end) prefix)
      ;; When mark is inactive.
      (setq str (qutechat--current-paragraph))
      (qutechat-send-string str prefix))
    ;; Display reply buffer and start reply monitor.
    (setq buf (get-buffer-create qutechat-buffer-name))
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf)
    (setq qutechat--timer-count 0)
    (qutechat--sched-timer-event)))

;; (qutechat--parse-reply)
(defun qutechat--parse-reply ()
  "Check the response for the last query from a Web-based chat
and return it as a string."
  (interactive)
  ;; Retrieve the reply for the last query.
  (call-process "qutebrowser" nil nil nil
		(format ":spawn -m -u %s -r %s"
			qutechat-prog qutechat-tmpfile))
  (sit-for .1)
  (with-temp-buffer
    ;; FIXME: This code assumes the first line is the query.
    (insert-file-contents qutechat-tmpfile)
    (goto-char (point-min))
    (while (search-forward "·" nil t) ;; \xb7
      (replace-match "."))
    (buffer-string)))

;; (qutechat-insert-reply)
(defun qutechat-insert-reply ()
  "At the current point, insert the response for the last query
from a Web-based chat."
  (interactive)
  (let ((reply (qutechat--parse-reply)))
    (insert reply)))

;; (qutechat--timer-event)
(defun qutechat--timer-event ()
  (let ((buf (get-buffer-create qutechat-buffer-name))
	(reply (qutechat--parse-reply)))
    (with-current-buffer buf
      (if (string= qutechat--last-reply reply) 
	  ;; No update.
	  (setq qutechat--timer-count (1+ qutechat--timer-count))
	;; Updated.
	(erase-buffer)
	(insert reply)
	(setq qutechat--last-reply reply)
	(setq qutechat--timer-count 0))
      ;; Schedule next event if it seems reply is updating.
      (if (< qutechat--timer-count 10)
	  (qutechat--sched-timer-event)
	(insert "\n----")))))

;; (qutechat--sched-timer-event)
(defun qutechat--sched-timer-event ()
  (run-with-timer .25 nil 'qutechat--timer-event))
