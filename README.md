## hiwin-mode とは

hiwin-mode は 2009-09-13 に ksugita さんがブログで公開

http://ksugita.blog62.fc2.com/blog-entry-8.html

したアクティブな window を可視化する elisp です。

これをマイナーモード化して使いやすくしたものを Github に置きます。

## インストール方法

* 直接ファイルをダウンロードしてインストール

* 下記のコマンドを使ってインストール

    ~~~~~~~~~~~~~~~~~~~~
    M-x install-elisp https://raw.github.com/tomoya/hiwin-mode/master/hiwin.el
    M-x auto-install-from-url https://raw.github.com/tomoya/hiwin-mode/master/hiwin.el
    ~~~~~~~~~~~~~~~~~~~~

* 下記のS式を評価してインストール

    ~~~~~~~~~~~~~~~~~~~~
    (auto-install-from-url "https://raw.github.com/tomoya/hiwin-mode/master/hiwin.el")
    (install-elisp "https://raw.github.com/tomoya/hiwin-mode/master/hiwin.el")
    ~~~~~~~~~~~~~~~~~~~~

* MELPAを利用してインストール

    ~~~~~~~~~~~~~~~~~~~~
    M-x package-install hiwin
    ~~~~~~~~~~~~~~~~~~~~
