;;; init.el -- Emacs Settings
;;; Commentary:
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; フォント設定
(add-to-list 'default-frame-alist '(font . "ricty-12"))
;; (add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono-11"))

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
(set-face-background 'show-paren-match-face "black")
(set-face-foreground 'show-paren-match-face "white")

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

;; 遅延行ハイライト
(require 'hl-line)
(defun global-hl-line-timer-function()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))


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

;;---------------------------
;; el-getの設定
;;---------------------------
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle 'editorconfig)
(editorconfig-mode 1)

;; Shell
(el-get-bundle 'multi-term)

;; (el-get-bundle 'smooth-scroll)
;; (smooth-scroll-mode t)

;; elfeed
(el-get-bundle 'elfeed)

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
        ("http://wired.jp/rssfeeder/") ;; Wired
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
        ("http://rss.rssad.jp/rss/slashdot/slashdot.rss")
        ("http://rss.rssad.jp/rss/slashdot/linux.rss")
        ("http://rss.rssad.jp/rss/slashdot/developers.rss")
        ("http://rss.rssad.jp/rss/slashdot/opensource.rss")
        ("http://rss.rssad.jp/rss/slashdot/mobile.rss")
        ("http://rss.rssad.jp/rss/slashdot/it.rss")
        ("http://rss.rssad.jp/rss/slashdot/apple.rss")
        ("http://rss.rssad.jp/rss/slashdot/security.rss")))


;; emacs-w3m
(el-get-bundle 'w3m)

(el-get-bundle 'auto-complete)
(el-get-bundle 'undo-tree)
;; (el-get-bundle evil) ;; VIモード

;; (el-get-bundle 'flycheck)
(el-get-bundle 'helm)
(el-get-bundle 'helm-ag)
(el-get-bundle 'helm-ls-git)

;; GIT
(el-get-bundle 'gitconfig-mode)
(el-get-bundle 'gitignore-mode)

;; SASS
(el-get-bundle 'sass-mode)

;; Stylus
(el-get-bundle 'vladh/stylus-mode)

;; OTHER
(el-get-bundle 'pkg-info)
(el-get-bundle 'rainbow-delimiters)
(el-get-bundle 'move-text)

;; C/C++

;; GLSL
(el-get-bundle 'glsl-mode)

;; LISP
;; CommonLisp

;; nginx-conf
(el-get-bundle 'nginx-mode)

;; Ruby
(el-get-bundle 'flymake-ruby)
(el-get-bundle 'motion-mode)
; rubyシンタックスハイライト
(el-get-bundle 'ruby-mode)
; 括弧、do-endの自動補完
(el-get-bundle 'ruby-electric)
;; 対応するブロックのハイライト
(el-get-bundle 'ruby-block)
(el-get-bundle 'projectile)
(el-get-bundle 'projectile-rails)

;; Inf-ruby (Pry)
(el-get-bundle 'inf-ruby)
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-eval-binding "Pry.toplevel_binding")

;; Coffee
(el-get-bundle 'coffee-mode)

;; pug-mode
(el-get-bundle 'pug-mode)
(require 'pug-mode)

;; Yaml
(el-get-bundle 'yaml-mode)

;; PHP
(el-get-bundle 'php-mode)
(el-get-bundle 'php-completion)

;; Java

;; JavaScript
(el-get-bundle 'js2-mode)

;; JSON
(el-get-bundle 'json-mode)

;; TypeScript
(el-get-bundle 'typescript-mode)
;; tsserverと連携
(el-get-bundle 'tide)
(el-get-bundle 'company)

;; Clojure
(el-get-bundle 'clojure-mode)
(el-get-bundle 'cider)

;; Markdown
(el-get-bundle 'markdown-mode)
(el-get-bundle 'markdown-preview-mode)

;; HTMLとその他の言語の混合
(el-get-bundle 'web-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(require 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))


;; php&html
(defun web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-engines-alist
        '(("php" . "\\.ctp\\'"))))

(add-hook 'web-mode-hook 'web-mode-hook)

;; Theme

;; # Monochrome
;; (el-get-bundle monochrome-theme)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/monochrome-theme")
;; (load-theme 'monochrome t)

;; # Borland Blue Theme
;; (el-get-bundle borland-blue-theme)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/borland-blue-theme")
;; (load-theme 'borland-blue t)

;; # Wombat
;;(load-theme 'wombat t)

;; # Zenburn Theme
(el-get-bundle zenburn-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/zenburn-theme")
(load-theme 'zenburn t)

;; # Solarized
;; (el-get-bundle 'solarized-theme)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/solarized-theme")
;; (load-theme 'solarized-dark t)

(set-face-background 'default' "#090960")

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; flycheckを有効に
(require 'flycheck)
(global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;;---------------------------
;; LISP
;;---------------------------
(add-to-list 'auto-mode-alist '("\\.ls$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))

;; Auto complete
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (setq indent-tabs-mode nil)
                                   (require 'auto-complete)
                                   (auto-complete-mode t)))
;; 関数情報表示
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; 対応括弧色付け
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;---------------------------
;; CommonLisp(SBCL)
;;---------------------------

;;---------------------------
;; C/C++
;;---------------------------
(defun c-c++-mode-init ()
  (setq c-set-style "gnu"))
(add-hook 'c-mode-hook 'c-c++-mode-init)
(add-hook 'c++-mode-hook 'c-c++-mode-init)

;;---------------------------
;; GLSL
;;---------------------------
(add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vertex\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fragment\\'" . glsl-mode))

;;---------------------------
;; PHP
;;---------------------------
(add-hook 'php-mode-hook '(lambda ()
                            (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))

;;---------------------------
;; TypeScript
;;---------------------------
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))


(require 'tide)
(add-hook 'typescript-mode-hook (lambda ()
                                  (tide-setup)
                                  (flycheck-mode t)
                                  (setq flycheck-check-syntax-automatically)
                                  (eldoc-mode t)
                                  (company-mode t)))



;; (setq web-mode-content-types-alist '(("jsx" . ".*\\.tsx?")))

;;---------------------------
;; JavaScript
;;---------------------------
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.pegjs$" . js2-mode))

;; (flycheck-add-mode 'javascript-eslint 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx$'" . js2-jsx-mode))

(add-hook 'js2-mode-hook '(lambda ()
                            (setq indent-tabs-mode nil)
                            (flycheck-mode 1)))

(add-hook 'js2-jsx-mode-hook '(lambda ()
                                (flycheck-mode 1)))

(eval-after-load 'flycheck '(custom-set-variables
                             '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-mode))))

;;---------------------------
;; JSON
;;---------------------------
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$'" . json-mode))

;;---------------------------
;; Ruby用の拡張ロードと設定
;;---------------------------
(require 'ruby-mode)
(setq ruby-insert-encoding-magic-comment nil)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda() (ruby-electric-mode) t))
(setq ruby-electric-expand-delimiters-list nil)

(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; Need gem (rubocop, ruby-lint)
(add-hook 'ruby-mode-hook '(lambda ()
                             (setq ruby-indent-tabs-mode nil)
                             (setq ruby-deep-indent-paren-style nil)
                             (setq flycheck-checker 'ruby-rubocop)
                             (flycheck-mode 1)
                             (setq ruby-deep-indent-paren-style nil)))
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-commit-mode-on)

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee?\\'" . coffee-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml?\\'" . yaml-mode))

(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;;---------------------------
;; Python
;;---------------------------
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;;---------------------------
;; Markdown Setting
;;---------------------------
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))

;; タブの表示
(setq whitespace-style '(face trailing tabs tab-mark))
(setq whitespate-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(require 'whitespace)
(global-whitespace-mode 1)

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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(package-selected-packages
   (quote
    (zenburn-theme company pug-mode w3m gitignore-mode gitconfig-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
