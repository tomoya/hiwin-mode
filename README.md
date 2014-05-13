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

## 更新

2013/09/01
ksugitaさんご本人によりマイナーモードかとバージョンアップが実施されましたので更新しました。

hiwin-mode 2.00 リリース - gnupackの開発メモ
http://d.hatena.ne.jp/ksugita0510/20111223/p1

またmyuheさんによるパッチも当ててあります。
https://gist.github.com/myuhe/5203057

hiwin.rcpはel-get用のrecipeになります。

```
(require 'hiwin)
(hiwin-activate)                           ;; hiwin-modeを有効化
(set-face-background 'hiwin-face "gray80") ;; 非アクティブウィンドウの背景色を設定
```
