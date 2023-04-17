# NAME

qutechat - access ChatGTP from Emacs without OpenAI API

# WHAT IS QUTECHAT?

The catchphrase of **qutechat** suggested by ChatGPT:

- Interact with ChatGPT and other chat systems right from your Emacs with qutechat.

- Say goodbye to unstable OpenAI API and expensive bills with qutechat for Emacs.

- Access ChatGPT hassle-free from Emacs with qutechat - no OpenAI API key required.

- Boost your productivity by chatting with ChatGPT and other web-based chat systems solely from Emacs with qutechat.

- Unlock the full potential of ChatGPT with qutechat - the Emacs Lisp program that makes it easy and efficient to use.

![video](https://github.com/h-ohsaki/qutechat/blob/master/screenshot/video.gif)

# DESCRIPTION

**qutechat** is an Emacs Lisp program designed to interactively
access ChatGPT (https://chat.openai.com/) as well as other
Web-based chat systems solely from Emacs.  ChatGPT can be easily
accessed from a program using OpenAI API (e.g., using openai module in
the Python language), but accessing ChatGPT using OpenAI API has
several drawbacks.

1. Batch processing via OpenAI API is unstable and slow; Accessing
   OpenAI API sometimes takes a long time or, in some cases, does not
   complete indefinitely.  Such instability makes the experiences of
   accessing ChatGPT frustrating.

2. You must have a non-free OpenAI account with your credit card
   registration.  Frequently accessing ChatGPT may cost a lot, so that
   you have to keep watching your OpenAI billing record.

**qutechat** solves the above issues by accessing ChatGPT from Emacs
without OpenAI API key.

**qutechat** is implemented using the userscript feature of
qutebrowser (https://qutebrowser.org/), and therefore it requires
qutebrowser is running.  **qutechat** operates by remotely controlling
your instance of qutebrowser using the userscript called `chat-proxy`.
Therefore, your qutebrowser must be able to execute the userscript
`chat-proxy`.

Note that **qutechat** should work with any Web-based chat system (or
even any webpage with an input form), but **qutechat** has been tested
only with ChatGPT (https://chat.openai.com/).

# INSTALLATION

1. Copy `qutechat.el` to an Emacs-lisp directory such as
   `/usr/local/share/emacs/site-lisp`.

2. Add the following lines to your `~/.emacs`.  Do not forget to edit
   `/path/to/chat-proxy` with the valid absolute path of `chat-proxy`
   script.

``` elisp
;; qutechat
(autoload 'qutechat-send "qutechat" nil t)
(autoload 'qutechat-display-reply "qutechat" nil t)
(global-set-key "\C-cq" 'qutechat-send-region)
(global-set-key "\C-cQ" 'qutechat-insert-reply)
(setq qutechat-proxy-prog "/path/to/chat-proxy")
```

3. Restart your Emacs or eval `~/.emacs`.

# USAGE

1. Start a qutebrowser.

2. Visit ChatGPT (https://chat.openai.com/) in qutebrowser, and login
   with your OpenAI account.

3. Focus the input form field (i.e., `Send a messge...' field at the
   bottom of ChatGPT).

4. On Emacs, move the point (i.e., the cursor in Emacs) in or at the
   end of the paragraph of query sentence(s).  Alternatively, you can
   select the region containing query sentences.  Then, type `C-c q`
   or execute `M-x qutechat-send`.

5. The query is automatically submitted to ChatGPT in your qutebrower.
   The response from ChatGPT should soon be displayed on the
   qutebrower

6. Once the response is displayed, type `C-c Q` or execute M-x
   qutechat-insert-reply from Emacs.  The response from ChatGPT is
   inserted at the current point in Emacs.

# PATCH TO QUTEBROWSER

On X Window System, qutebrowser automatillcay raises its window when a
command is executed remotely.  Due to this behavior, running
**qutechat** may lost the window focus from Emacs; the window focus
may or may not change from Emacs to qutebrowser.  To prevent this
annoying behavior, you can apply the following patch to qutebrowser.
You can find `mainwindow/mainwindow.py` in the qutebrowser
installation (e.g.,
`/usr/local/lib/python3.9/dist-packages/qutebrowser`).

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
