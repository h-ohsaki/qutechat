;; -*- Emacs-Lisp -*-
;;
;; Emacs interface to Web-based chat (e.g., ChatGPT).
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

;; Add the following lines to your ~/.emacs.
;; ;; qutechat
;; (autoload 'qutechat-send-region "qutechat" nil t)
;; (autoload 'qutechat-display-reply "qutechat" nil t)
;; (global-set-key "\C-cq" 'qutechat-send-region)
;; (global-set-key "\C-cQ" 'qutechat-display-reply)

;; Usage:
;; 1. Start a qutebrowser.
;; 2. Visit a Web-based chat such as ChatGPT (https://chat.openai.com/)
;;    in qutebrowser.
;; 3. Focus the input form field (e.g., `Send a messge...' field in
;;    ChatGPT).
;; 4. On Emacs, select the region in the current buffer as a query
;;    string, and execute `M-x qutechat-send-region`.
;; 5. The query string is submitted in qutebrower.  The response from the
;;    Web-based chat system should soon be displayed.
;; 6. After the response is fully displayed in qutebrower, execute M-x
;;    qutechat-display-reply from Emacs.

(defvar qutechat-proxy-prog "~/src/qutechat/chat-proxy"
  "The name of the qutebrowser userscript.")

;; FIXME: Avoid hard-coding.
(defvar qutechat-tmpfile "/tmp/qutechat.tmp")

;; (qutechat-send-string "Emacs と vi はどちらがいいですか。")
;; (qutechat-send-string "Emacs の面白い歴史は?")
(defun qutechat-send-string (str)
  "Send a query string STR to a Web-based chat via qutebrowser."
  (interactive)
  ;; Save the query string to a temporary file.
  (with-temp-buffer
    (insert str)
    (write-region (point-min) (point-max) qutechat-tmpfile))
  ;; Ask the qutebrowser to fill and send the query string.
  (shell-command (format "qutebrowser ':spawn -m -u %s -s %s'"
			 qutechat-proxy-prog qutechat-tmpfile)))
  
(defun qutechat-send-region (start end)
  "Send the region between START and END to a Web-based chat."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (qutechat-send-string str)))

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
    (insert-file-contents qutechat-tmpfile)
    (buffer-string)))

;; (qutechat-display-reply)
(defun qutechat-display-reply ()
  "Display the response for the last query from a Web-based
chat."
  (interactive)
  ;; Display the response in the other window.
  (let ((buf (get-buffer-create "*Qutechat Output*"))
	(reply (qutechat-parse-reply)))
    (with-current-buffer buf
      (erase-buffer)
      (insert reply)
      ;; Enable automatic text wrapping.
      (visual-line-mode 1)
      (delete-other-windows)
      (split-window)
      (set-window-buffer (next-window) buf))))
