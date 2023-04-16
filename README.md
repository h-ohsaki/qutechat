# NAME

qutechat - Emacs interface to Web-based chat (e.g., ChatGPT)

![video](https://github.com/h-ohsaki/qutechat/blob/master/screenshot/video.gif)

# DESCRIPTION

NOTE: **qutechat** enables access to ChatGPT **without ChatGPT API**.

**qutechat** is an Emacs Lisp program designed for interactive control
of web-based chat systems such as ChatGPT (https://chat.openai.com/)
from within Emacs.  **qutechat** is implemented using the userscript
feature of qutebrowser (https://qutebrowser.org/), and therefore
requires qutebrowser to function.  **qutechat** operates by remotely
controlling qutebrowser and running a userscript named `chat-proxy` on
it.  Therefore, it is necessary for the qutebrowser to be able to run
the userscript `chat-proxy`.

# INSTALLATION

1. Copy `qutechat.el` to an Emacs-lisp directory such as
   `/usr/local/share/emacs/site-lisp`.

2. Add the following lines to your `~/.emacs`.

``` elisp
;; qutechat
(autoload 'qutechat-send-region "qutechat" nil t)
(autoload 'qutechat-display-reply "qutechat" nil t)
(global-set-key "\C-cq" 'qutechat-send-region)
(global-set-key "\C-cQ" 'qutechat-display-reply)
```

3. Restart your Emacs or eval `~/.emacs`.

# USAGE

1. Start a qutebrowser.

2. Visit a Web-based chat such as ChatGPT (https://chat.openai.com/)
   in qutebrowser.

3. Focus the input form field (e.g., `Send a messge...' field in
   ChatGPT).

4. On Emacs, select the region in the current buffer as a query
   string, and execute `M-x qutechat-send-region`.

5. The query string is submitted in qutebrower.  The response from the
   Web-based chat system should soon be displayed.

6. After the response is fully displayed in qutebrower, execute M-x
   qutechat-display-reply from Emacs.

# PATCH TO QUTEBROWSER

On X Window System, qutebrowser raises the window when a command is
executed.  Due to this behavior, running **qutechat** may cause the
window focus to move from Emacs to qutebrowser.  To prevent this
annoying inconvenience, you can apply the following patch to
qutebrowser.

``` python
--- mainwindow/mainwindow.py.orig       2023-04-17 03:42:04.348059533 +0900
+++ mainwindow/mainwindow.py    2023-04-17 03:45:50.230284419 +0900
@@ -80,7 +80,9 @@
         window.show()
         should_raise = True
 
-    if should_raise and not no_raise:
+    # Apr 17, 2023 by Hiroyuki Ohsaki        
+    # Completely disable raising window.
+    if False and should_raise and not no_raise:
         raise_window(window)
 
     return window.win_id
```
