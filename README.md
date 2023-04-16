# NAME

qutechat - Emacs interface to Web-based chat (e.g., ChatGPT)

# DESCRIPTOR

以下をオープンソースソフトウェアのマニュアルにふさわしい英語に直して。
qutechat は、ChatGPT のような Web ベースのチャットシステムを、Emacs か
ら対話的に操作するための Emacs Lisp プログラムです。qutechat は、
qutebrowser の userscript 機能を利用して実現されています。このため、
qutechat の動作には qutebrowser が必要です。qutechat は、qutebrowser
をリモートから操作し、qutebrowser 上で chat-proxy という名前の
userscript を実行します。したがって、chat-proxy という userscript を
qutebrowser から実行できることが必要です。

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
