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

;; # INSTALLATION
;; 
;; 1. Copy `qutechat.el` to an Emacs-lisp directory such as
;;    `/usr/local/share/emacs/site-lisp`.
;; 
;; 2. Add the following lines to your `~/.emacs`.
;; 
;; ``` elisp
;; ;; qutechat
;; (autoload 'qutechat-send-region "qutechat" nil t)
;; (autoload 'qutechat-display-reply "qutechat" nil t)
;; (global-set-key "\C-cq" 'qutechat-send-region)
;; (global-set-key "\C-cQ" 'qutechat-insert-reply)
;; ```
;; 
;; 3. Restart your Emacs or eval `~/.emacs`.

;; # USAGE
;; 
;; 1. Start a qutebrowser.
;; 
;; 2. Visit ChatGPT (https://chat.openai.com/) in qutebrowser, and login
;;    with your OpenAI account.
;; 
;; 3. Focus the input form field (i.e., `Send a messge...' field at the
;;    bottom of ChatGPT).
;; 
;; 4. On Emacs, select the region in the current buffer as query
;;    sentences or move to the point (i.e., the cursor in Emacs) at
;;    around the query sentences.  Type `C-c q` or execute `M-x
;;    qutechat-send-region`.
;; 
;; 5. The query is automatically submitted to ChatGPT in your qutebrower.
;;    The response from ChatGPT should soon be displayed on the
;;    qutebrower
;; 
;; 6. Once the response is displayed, type `C-c Q` or execute M-x
;;    qutechat-display-reply from Emacs.  The response from ChatGPT is
;;    inserted at the current point in Emacs.

(defvar qutechat-proxy-prog "~/src/qutechat/chat-proxy"
  "The name of the qutebrowser userscript.")

;; FIXME: Avoid hard-coding.
(defvar qutechat-tmpfile "/tmp/qutechat.tmp")

;; (qutechat--continuous-block)
(defun qutechat--continuous-block ()
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
    (unless mark-active
      (setq str (qutechat--continuous-block)))
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
