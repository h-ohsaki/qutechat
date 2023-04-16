;; -*- Emacs-Lisp -*-
;;
;; Emacs interface to Web-based chat (e.g., ChatGPT).
;; Copyright (C) 2023 Hiroyuki Ohsaki.
;; All rights reserved.
;;

;; Add the following lines to your ~/.emacs.
;; ;; qutechat
;; (autoload 'qutechat-send-region "qutechat" nil t)
;; (autoload 'qutechat-display-reply "qutechat" nil t)
;; (global-set-key "\C-cq" 'qutechat-send-region)
;; (global-set-key "\C-cQ" 'qutechat-display-reply)

;; Usage:
;; 1. Start a qutebrowser.
;; 2. Visit a Web-based chat page (e.g., https://chat.openai.com/).
;; 3. Focus the input form field.
;; 4. Select a region and execute M-x qutechat-send-region.
;; 5. After the response is shown, execute M-x qutechat-display-reply.

(defvar qutechat-proxy-prog "~/.config/qutebrowser/chat-proxy"
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
