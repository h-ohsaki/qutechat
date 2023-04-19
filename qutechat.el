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

(defvar qutechat--last-reply nil
  "The last reply returned by the server.")

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
		(format ":spawn -m -u %s -s %s" qutechat-proxy-prog qutechat-tmpfile)))
  
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
  (let (prefix str ch buf)
    (when arg
      (setq ch (read-char "Select query type ([w]what/[s]ummary/[j]apanese/[e]nglish/[p/P]roofread): "))
      (cond ((eq ch ?w)
	     (setq prefix "Explain the following in Japanese with definition, usage, and examples."))
	    ((eq ch ?s)
	     (setq prefix "Summarize the following in a plain Japanese."))
	    ((eq ch ?j)
	     (setq prefix "Translate the following in Japanese in an academic writing style."))
	    ((eq ch ?e)
	     (setq prefix "Translate the following in English in an academic writing style."))
	    ((eq ch ?p)
	     (setq prefix "Proofread following text and summarize all suggested changes."))
	    ((eq ch ?P)
	     (setq prefix "以下の文章の誤りを直して、変更点の一覧を出力して。"))))
    (if mark-active
	(qutechat-send-region (region-beginning) (region-end) prefix)
      ;; When mark is inactive.
      (setq str (qutechat--current-paragraph))
      (qutechat-send-string str prefix))
    ;; Display reply buffer and start reply monitor.
    (setq buf (get-buffer-create "*Qutechat reply*"))
    (delete-other-windows)
    (split-window)
    (set-window-buffer (next-window) buf)
    (qutechat--sched-timer-event)))

;; (qutechat-parse-reply)
(defun qutechat-parse-reply ()
  "Check the response for the last query from a Web-based chat
and return it as a string."
  (interactive)
  ;; Retrieve the response for the last query.
  (call-process "qutebrowser" nil nil nil
		(format ":spawn -m -u %s -r %s"
			qutechat-proxy-prog qutechat-tmpfile))
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
  "Insert the response for the last query from a Web-based
chat at the point."
  (interactive)
  (let ((reply (qutechat-parse-reply)))
    (insert reply)))

;; (qutechat--timer-event)
(defun qutechat--timer-event ()
  (let ((buf (get-buffer-create "*Qutechat reply*"))
	(reply (qutechat-parse-reply)))
    (with-current-buffer buf
      (cond ((string= qutechat--last-reply reply) ;; No update.
	     (setq qutechat--timer-count (1+ qutechat--timer-count))
	     ;; Stop the reply monitor after no update for 10 seconds.
	     (when (> qutechat--timer-count 10)
	       (qutechat--cancel-timer-event)))
	    (t ;; Updated.
	     (visual-line-mode 1)
	     (erase-buffer)
	     (insert reply)
	     (setq qutechat--last-reply reply)
	     (setq qutechat--timer-count 0))))))	  

;; (qutechat--sched-timer-event)
(defun qutechat--sched-timer-event ()
  (if (and qutechat--timer
	   (memq qutechat--timer timer-list))
      ;; Do not create a timer if already present.
      nil
    (setq qutechat--timer (run-with-timer
			   1 1 'qutechat--timer-event))
    (setq qutechat--timer-count 0)))

(defun qutechat--cancel-timer-event ()
  (when qutechat--timer
    (cancel-timer qutechat--timer)
    (setq qutechat--timer nil)))
