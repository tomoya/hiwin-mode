;;; hiwin.el --- Visible active window mode.

;; Copyright (C) 2009 ksugita
;;               2010 tomoya  <tomoya.ton@gmail.com>
;;               2010 ksugita <ksugita0510@gmail.com>
;;               2010 myuhe   <yuhei.maeda@gmail.com>

;; Author: ksugita
;; Keywords: faces, editing, emulating
;; Version: 1.02

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Usage
;; put followings your .emacs
;;   (require 'hiwin)
;;
;; if you visible active window, type M-x hiwin-mode.

;;; Changes
;; 2010-08-13 tomoya
;; myuhe さんと ksugita さんの修正をマージ
;;
;; 2010-08-13 ksugita
;; *Completions*表示時にMiniBufの表示が崩れるのを修正
;; 手動で画面リフレッシュできるようhiwin-refresh-winをinteractive化
;; 個人的な設定だったため，recenterのadviceを削除
;;
;; 2010-08-11 myuhe
;; anything関連の関数を実行した時にミニバッファが非アクティブウィンドウとして扱われる問題を修正
;; anything起動時にanythingバッファ以外のバッファが非アクティブウィンドウとなったまま戻らない問題を修正
;;
;; 2010-07-04 ksugita
;; ローカルで再スクラッチしたファイルに tomoya氏，masutaka氏の修正を反映
;; readonlyなアクティブwindowの背景色を設定できるように機能変更
;;
;; 2010-06-07 tomoya
;; マイナーモード化
;;
;; 2009-09-13 ksugita
;; ブログで公開
;; http://ksugita.blog62.fc2.com/blog-entry-8.html


;;; Code:

(defvar hiwin-deactive-color "gray30"
  "非アクティブwindowの背景色")
(defvar hiwin-readonly-color "#000030"
  "読み取り専用windowの背景色")
(defvar hiwin-normal-color (background-color-at-point)
  "書き込み可能windowの背景色")
(defvar hiwin-ignore-buffer
  '(" *Minibuf-1*" "+draft/1" "+draft/2" "+draft/3")
  "ハイライト対象外buffer")
(defvar hiwin-ol-count 8
  "非アクティブwindowのoverlay数")
(defvar hiwin-ol-lines 128
  "非アクティブwindowの行数")

(defvar hiwin-temp-buf "*hiwin-temp*"
  "作業用バッファ")
(defvar hiwin-face nil
  "非アクティブwindow用face")
(defvar hiwin-ol nil
  "非アクティブwindow用overlay")
(defvar hiwin-window nil
  "アクティブwindowのwindow")
(defvar hiwin-buffer nil
  "アクティブwindowのbuffer")

(define-minor-mode hiwin-mode
  "Visible active window."
  :global t
  :lighter " hiwin"
  :group 'hiwin
  (hiwin))

(defun hiwin-create-ol ()
  ;; 非アクティブwindow用faceを作成
  (make-face 'hiwin-face)
  ;; 非アクティブwindow用faceの背景色を設定
  (set-face-background 'hiwin-face hiwin-deactive-color)
  (let ((num 0)      ;; カウンタ
        (buf nil))   ;; 作業用バッファ
    ;; 作業用バッファを作成
    (setq buf (get-buffer-create hiwin-temp-buf))
    ;; 作成するoverlay分を処理（ループ開始）
    (while (< num hiwin-ol-count)
      ;; 非アクティブwindow用overlayを作成
      (setq hiwin-ol (cons (make-overlay 1 1 buf nil t) hiwin-ol))
      ;; 非アクティブwindow用overlayの本文のfaceを設定
      (overlay-put (nth 0 hiwin-ol) 'face 'hiwin-face)
      ;; 非アクティブwindow用overlayのEOFのfaceを設定
      (overlay-put (nth 0 hiwin-ol) 'after-string
                   (propertize (make-string hiwin-ol-lines ?\n) 'face 'hiwin-face))
      ;; カウンタアップ
      (setq num (1+ num))
      ) ;; （ループ終了）
    ;; 作業用バッファを削除
    (kill-buffer buf)))

(defun hiwin-draw-ol ()
  ;; アクティブwindowのwindowを取得
  (setq hiwin-window (selected-window))
  ;; アクティブwindowのbufferを取得
  (setq hiwin-buffer (current-buffer))
  (let ((num 0)                 ;; カウンタ
        (win nil)               ;; 処理対象window
        (lst (window-list)))    ;; 表示windowのリスト
    ;; 表示winndowのすべてを処理（ループ開始）
    (while lst
      ;; 処理対象windowを取得
      (setq win (car lst))
      ;; 処理対象windowをリストから削除
      (setq lst (cdr lst))
      ;; 処理対象windowとアクティブwindowが一致する場合
      (if (or (eq win hiwin-window) (eq win (minibuffer-window)))
          (progn
            ;; EOB一つ前の場合，一文字進む
            (if (and (eq (point) (1- (point-max)))
                     (> (point-max) 1))
                (forward-char 1))
            ;; 読み取り専用か否かで背景色を変更
            (if buffer-read-only
                (set-background-color hiwin-readonly-color)
              (set-background-color hiwin-normal-color)))
        ;; 処理対象windowとアクティブwindowが一致しない場合
        (progn
          (let ((buf (window-buffer win)))
            (if (member buf hiwin-ignore-buffer)
                ()
              ;; 処理対象windowをアクティブ化
              (select-window win)
              ;; EOBの場合，一文字戻る
              (if (and (eq (point) (point-max))
                       (> (point-max) 1))
                  (backward-char 1))
              ;; 処理対象windowにoverlayを設定
              (move-overlay (nth num hiwin-ol)
                            (point-min) (point-max) (current-buffer))
              (overlay-put (nth num hiwin-ol) 'window win)
              ;; カウンタアップ
              (setq num (1+ num))
              ))))) ;; （ループ終了）
    ;; アクティブwindowをアクティブ化
    (select-window hiwin-window)))

(defun hiwin-delete-ol ()
  (let ((num 0))   ;; カウンタ
    ;; 作成されたoverlay分を処理（ループ開始）
    (while (< num hiwin-ol-count)
      ;; 非アクティブwindow用overlayを削除
      (delete-overlay (nth num hiwin-ol))
      ;; カウンタアップ
      (setq num (1+ num))
      )) ;; （ループ終了）
  ;; 非アクティブwindow用overlayの変数を初期化
  (setq hiwin-ol nil))

(defun hiwin-refresh-win ()
  (interactive)
  (let ((win (selected-window)) )
    ;; ミニバッファかanythingバッファ以外を選択している場合
    (unless (or (eq win (minibuffer-window))
                (eq 1 (string-match "anything" (buffer-name (window-buffer win)))))
      ;; 現在のウィンドウがアクティブウィンドウの場合
      ;; かつ，現在のバッファがカレントバッファの場合
      (if (and (eq hiwin-window win) (eq hiwin-buffer (current-buffer)))
          ;; 読み取り専用か否かで背景色を変更
          (if buffer-read-only
                (set-background-color hiwin-readonly-color)
              (set-background-color hiwin-normal-color)
              )
        ;; 描画を実行
        (hiwin-draw-ol)
        ))))

(defun hiwin ()
  (if (null hiwin-ol)
      (progn (hiwin-create-ol)
             (add-hook 'pre-command-hook  'hiwin-refresh-win)
             (add-hook 'post-command-hook 'hiwin-refresh-win)
             (add-hook 'window-configuration-change-hook 'hiwin-refresh-win)
             (hiwin-refresh-win)
             )
    (progn (hiwin-delete-ol)
           (remove-hook 'pre-command-hook  'hiwin-refresh-win)
           (remove-hook 'post-command-hook 'hiwin-refresh-win)
           (remove-hook 'window-configuration-change-hook 'hiwin-refresh-win)
           )))

(defadvice split-window-vertically
  (around hiwin-split-window-vertically activate)
  ad-do-it
  (if hiwin-ol (hiwin-draw-ol)))

(defadvice split-window-horizontally
  (around hiwin-split-window-horizontally activate)
  ad-do-it
  (if hiwin-ol (hiwin-draw-ol)))

(defadvice delete-window
  (around hiwin-delete-window activate)
  ad-do-it
  (when hiwin-ol (hiwin) (hiwin)))

(defadvice other-window
  (around hiwin-other-window activate)
  ad-do-it
  (if hiwin-ol (hiwin-draw-ol)))

(defadvice twittering-edit-close
  (around hiwin-twittering-edit-close activate)
  ad-do-it
  (when hiwin-ol (hiwin)(hiwin)))

(provide 'hiwin)
