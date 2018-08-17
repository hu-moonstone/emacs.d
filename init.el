;;; init.el -- Emacs Settings
;;; Commentary:
;;; Code:

(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; straight.elの設定
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; ---------------------------------
;; Emacsの通常設定
;; ---------------------------------
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq gc-cons-threshold (* 128 1024 1024))

;; ブラウザ
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; フォント設定
(add-to-list 'default-frame-alist '(font . "Ricty 12"))

;; 行間
(setq line-spacing 3)

;; スタートアップメッセージ非表示
(setq inhibit-startup-message t)

;; C-hをBackspaceとして使う
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-ch" 'help-command)

;; Tab: 全バッファーに対して
;; (setq default-tab-width 2)
(setq-default tab-width 4 indent-tabs-mode nil)

;; 改行コード表示
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; メニュー非表示
(menu-bar-mode 0)

;; ツールバー非表示
(tool-bar-mode 0)

;; 列番号表示
(column-number-mode t)

;; 行番号表示
(global-linum-mode t)

;; C-kで行全体削除
(setq kill-whole-line t)

;; 対応するカッコのハイライト
(show-paren-mode 1)

;; カッコのハイライトの色設定
(setq show-paren-delay 1)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#151019")
;;(set-face-foreground 'show-paren-match-face "#403047")

;; バックアップファイル無効
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; オートセーブ無効
(setq auto-save-default nil)

;; 末尾スペース処理
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ファイル名補完で大文字小文字の区別をなくす
(setq completion-ignore-case t)

;; Emacs以外から編集されたものも自動でバッファをリロード
(global-auto-revert-mode 1)

;; 現在の行をハイライト
;; (global-hl-line-mode t) ;; 重いのでコメントアウト


;; モードラインに列番号表示
(column-number-mode 1)

;; モードラインに行番号表示
(line-number-mode 1)

;; リージョンの強調表示を行う
(transient-mark-mode 1)

;; ノンアクティブなウインドウのカーソル表示を行わない
(setq cursor-in-non-selected-windows nil)

;; コードの色分けをON
(global-font-lock-mode t)

;; バッファリストをC-x C-bで表示
(global-set-key "\C-x\C-b" 'buffer-menu)

;; バックタブ

(add-hook 'text-mode-hook
          '(lambda()
;             (define-key text-mode-map "\C-i" 'tab-to-tab-stop)
             (define-key text-mode-map "\C-i" 'tab-to-tab-stop-line-or-region)
;             (define-key text-mode-map [backtab] 'backtab)
             (define-key text-mode-map [backtab] 'backtab-line-or-region)
             (setq tab-stop-list '(2 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124 128))
             (setq indent-tabs-mode nil)))

(defun tab-to-tab-stop-line-or-region ()
  (interactive)
  (if mark-active (save-excursion
                    (setq count (count-lines (region-beginning) (region-end)))
                    (goto-char (region-beginning))
                    (while (> count 0)
                      (tab-to-tab-stop)
                      (forward-line)
                      (setq count (1- count)))
                    (setq deactivate-mark nil))
    (tab-to-tab-stop)))

(defun backtab()
  "Do reverse indentation"
  (interactive)
  (back-to-indentation)
  (delete-backward-char
   (if (< (current-column) (car tab-stop-list)) 0
     (- (current-column)
        (car (let ((value (list 0)))
               (dolist (element tab-stop-list value)
                 (setq value (if (< element (current-column)) (cons element value) value)))))))))

(defun backtab-line-or-region ()
  (interactive)
  (if mark-active (save-excursion
                    (setq count (count-lines (region-beginning) (region-end)))
                    (goto-char (region-beginning))
                    (while (> count 0)
                      (backtab)
                      (forward-line)
                      (setq count (1- count)))
                    (setq deactivate-mark nil))
    (backtab)))

;; タブの表示
(setq whitespace-style '(face trailing tabs tab-mark))
(setq whitespate-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(require 'whitespace)
(global-whitespace-mode 1)

;; 遅延行ハイライト
(require 'hl-line)
(defun global-hl-line-timer-function()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.01 t 'global-hl-line-timer-function))


;; diredでoを入力した際、そのファイルをgnomeオープンする
(add-hook 'dired-load-hook
          (function (lambda ()
                      (define-key dired-mode-map "o" 'dired-open-file))))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))


;; power line
;; (use-package powerline
;;   :init
;;   (defun powerline-my-theme ()
;;     "Setup the my mode-line."
;;     (interactive)
;;     (setq powerline-current-separator 'utf-8)
;;     (setq-default mode-line-format
;;                   '("%e"
;;                     (:eval
;;                      (let* ((active (powerline-selected-window-active))
;;                             (mode-line (if active 'mode-line 'mode-line-inactive))
;;                             (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
;;                             (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
;;                             (separator-left (intern (format "powerline-%s-%s"
;;                                                             (powerline-current-separator)
;;                                                             (car powerline-default-separator-dir))))
;;                             (lhs (list (powerline-raw " " face1)
;;                                        (powerline-major-mode face1)
;;                                        (powerline-raw " " face1)
;;                                        (funcall separator-left face1 face2)
;;                                        (powerline-buffer-id nil )
;;                                        (powerline-raw " [ ")
;;                                        (powerline-raw mode-line-mule-info nil)
;;                                        (powerline-raw "%*" nil)
;;                                        (powerline-raw " |")
;;                                        (powerline-process nil)
;;                                        (powerline-vc)
;;                                        (powerline-raw " ]")
;;                                        ))
;;                             (rhs (list (powerline-raw "%4l" 'l)
;;                                        (powerline-raw ":" 'l)
;;                                        (powerline-raw "%2c" 'l)
;;                                        (powerline-raw " | ")
;;                                        (powerline-raw "%6p" )
;;                                        (powerline-raw " ")
;;                                        )))
;;                        (concat (powerline-render lhs)
;;                                (powerline-fill nil (powerline-width rhs))
;;                                (powerline-render rhs)))))))

;;   (defun make/set-face (face-name fg-color bg-color weight)
;;     (make-face face-name)
;;     (set-face-attribute face-name nil
;;                         :foreground fg-color :background bg-color :box nil :weight weight))
;;   (make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
;;   (make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
;;   (make/set-face 'mode-line-1-arrow  "#666666" "#3E4451" 'bold)
;;   (make/set-face 'mode-line-2-arrow  "#666666" "#3E4451" 'bold)
;;   (powerline-my-theme))

;; theme (cherry blossom)
(use-package cherry-blossom-theme)
;; (use-package solarized-theme)

;; elfeed
(use-package elfeed
  :init
  (progn
    (global-set-key (kbd "C-x w") 'elfeed)
    (setq elfeed-feeds
          '(;; Security
            ("http://vrda.jpcert.or.jp/feed/ja/atom.xml") ;; VRDA Security
            ("http://jvndb.jvn.jp/ja/rss/jvndb_new.rdf") ;; JVNDB Security
            ;; Linux, Debian, Free Software Foundation
            ("https://www.debian.org/News/news")
            ("https://www.debian.org/security/dsa")
            ("https://static.fsf.org/fsforg/rss/news.xml")
            ("https://static.fsf.org/fsforg/rss/blogs.xml")
            ("https://www.linux.com/feeds/news/rss")
            ("https://www.w3.org/blog/news/feed")
            ;; Tech
            ("https://thinkit.co.jp/rss.xml") ;; ThinkIT
            ("http://postd.cc/feed/") ;; PostD
            ("https://codeiq.jp/magazine/feed/") ;; CodeIQ
            ("http://rss.rssad.jp/rss/codezine/new/20/index.xml") ;; Codezine
            ("https://geechs-magazine.com/feed") ;; Geechs
            ("http://uxmilk.jp/feed") ;;Uxmilk
            ("http://jp.techcrunch.com/feed/") ;; Techcrunch
            ("http://www.seleqt.net/feed/") ;; Seleqt
            ("http://feeds.feedburner.com/WebmasterCentral?format=xml") ;; Google Web-Master Official Blog
            ("https://blogs.msdn.microsoft.com/bingdevcenter/feed/") ;; Bing Developer Blog
            ("https://hacks.mozilla.org/feed/") ;; Mozilla Hacks
            ("https://wired.jp/rssfeeder/") ;; Wired
            ("https://liginc.co.jp/feed") ;; LIG
            ("https://ferret-plus.com/.rss") ;; Ferret
            ("https://news.ycombinator.com/rss") ;; Hacker News
            ("https://www.infoq.com/jp/feed?token=6He6dTMXb4uv4glWb5XjKb3YeU0sB0QV") ;; InfoQ
            ("http://rss.rssad.jp/rss/itmnews/2.0/news_bursts.xml") ;; IT Media
            ("http://rss.rssad.jp/rss/itmnews/2.0/news_security.xml") ;; IT Media
            ("http://feeds.japan.cnet.com/rss/cnet/all.rdf") ;; CNET
            ("http://b.hatena.ne.jp/hotentry/it.rss") ;; Hatena1
            ("http://b.hatena.ne.jp/entrylist/it.rss") ;; Hatena2
            ("http://gihyo.jp/dev/feed/rss2") ;; Gihyo
            ;; /.
            ("https://srad.jp/slashdot.rss")
            ("https://srad.jp/linux.rss")
            ("https://srad.jp/developers.rss")
            ("https://srad.jp/opensource.rss")
            ("https://srad.jp/mobile.rss")
            ("https://srad.jp/it.rss")
            ("https://srad.jp/apple.rss")
            ("https://srad.jp/security.rss")))))


;; Rust
(use-package rust-mode)
;; nginx confファイル用のモード
(use-package nginx-mode)

;; codic.jp
(use-package codic)

;; Emacs上のターミナル環境
(use-package multi-term)

;; w3m
(use-package w3)

;; GIT
(use-package gitconfig-mode)
(use-package gitignore-mode)
(use-package magit)

;; SASS/Stylus
(use-package sass-mode)
(use-package stylus-mode
  :straight
  (stylus-mode :type git :host github :repo "vladh/stylus-mode")
  :init
  (add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode)))

;; Pug
(use-package pug-mode)

;; Yaml
(use-package yaml-mode)

;; PHP
(use-package php-mode)

;; JavaScript
(use-package js2-mode)

;; JSON
(use-package json-mode)

;; TypeScript
(use-package typescript-mode)
;; TypeScript(tsserver)
(use-package tide)

;; Web
(use-package web-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (defun web-mode-hook ()
      (setq web-mode-engines-alist
            '(("php" . "\\.ctp\\'"))))
    (add-hook 'web-mode-hook 'web-mode-hook)))

;; Clojure
(use-package clojure-mode)
(use-package cider)

;; Markdown
(use-package markdown-mode)
(use-package markdown-preview-mode)


;; C/C++
(defun c-c++-mode-init ()
  (c-set-style "gnu"))
(add-hook 'c-mode-hook 'c-c++-mode-init)
(add-hook 'c++-mode-hook 'c-c++-mode-init)

;; GLSL
(use-package glsl-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.vertex\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.fragment\\'" . glsl-mode))))

;; Python
(use-package python-mode)

;; jsx, tsx
(setq web-mode-content-types-alist '(("jsx" . ".*\\.tsx?")))

;; コード補完
(use-package auto-complete)

;; Undo拡張
(use-package undo-tree)

;; editorconfig対応
(use-package editorconfig
  :init
  (editorconfig-mode 1))

;; 括弧対応
(use-package smartparens)

;; 括弧色付け
(use-package rainbow-delimiters)

;; テキストの行単位での移動
(use-package move-text)

;; 補完・リストアップ
(use-package helm)

;; シンタックスチェック
(use-package flycheck)

(use-package adoc-mode)

;; テキストブラウザ
(use-package eww)

;; Pandoc
(use-package pandoc
  :init
  (pandoc-turn-on-advice-eww))

;; コードフォーマッタ
;; (use-package prettier-js
;;   :init
;;   (add-hook 'js2-mode-hook 'prettier-js-mode)
;;   (add-hook 'web-mode-hook 'prettier-js-mode))

;; org mode
;; (use-package org
;;   :init (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

;; init.elをバイトコンパイル
;; (add-hook 'after-init-hook
;;           '(lambda ()
;;              (let* ((el (expand-file-name "init.el" user-emacs-directory))
;;                     (elc (concat el "c")))
;;                (when (file-newer-than-file-p el elc)
;;                  (byte-compile-file el)))))
