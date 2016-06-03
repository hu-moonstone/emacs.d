;;; init.el -- Emacs Settings
;;; Commentary:
;;; Code:

(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; フォント設定
(add-to-list 'default-frame-alist '(font . "ricty-12"))

;; スタートアップメッセージ非表示
(setq inhibit-startup-message t)

;; C-hをBackspaceとして使う
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-ch" 'help-command)

;; Tab
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
(setq show-paren-delay 0.2)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "gray")
(set-face-foreground 'show-paren-match-face "black")

;; バックアップファイル無効
(setq make-backup-files nil)

;; オートセーブ無効
(setq auto-save-default nil)

;; ファイル名補完で大文字小文字の区別をなくす
(setq completion-ignore-case t)

;; Emacs以外から編集されたものも自動でバッファをリロード
(global-auto-revert-mode 1)

;; 現在の行をハイライト
(global-hl-line-mode t)

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


;; Shell
(el-get-bundle 'multi-term)

;; emacs-w3m
(el-get-bundle 'w3m)

(el-get-bundle 'auto-complete)
(el-get-bundle 'undo-tree)
;; (el-get-bundle evil) ;; VIモード

;(el-get-bundle 'flycheck)
(el-get-bundle 'helm)
(el-get-bundle 'helm-ag)
(el-get-bundle 'helm-ls-git)

;; GIT
(el-get-bundle 'magit) ;; GITクライアント
(el-get-bundle 'git-gutter+) ;; 変更箇所マーク
(el-get-bundle 'gitconfig-mode)
(el-get-bundle 'gitignore-mode)

;; OTHER
(el-get-bundle 'pkg-info)
(el-get-bundle 'rainbow-delimiters)
(el-get-bundle 'move-text)

;; SCALA
(el-get-bundle 'scala-mode2)
(el-get-bundle 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; C/C++

;; GLSL
(el-get-bundle 'glsl-mode)

;; LISP
;; CommonLisp
(el-get-bundle 'slime)

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


;; Slim
(el-get-bundle 'slim-mode)

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
;; tsserverと連携する拡張
(el-get-bundle 'tide)
(el-get-bundle 'company)



;; Markdown
(el-get-bundle 'markdown-mode)
(el-get-bundle 'markdown-preview-mode)

;; HTMLとその他の言語の混合
(el-get-bundle 'web-mode)


;; Theme
;(el-get-bundle monochrome-theme)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/monochrome-theme")
;(load-theme 'monochrome t)
;; (el-get-bundle borland-blue-theme)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/borland-blue-theme")
;; (load-theme 'borland-blue t)

;; (load-theme 'wombat t)
(el-get-bundle zenburn-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/zenburn-theme")
(load-theme 'zenburn t)

;; (el-get-bundle 'solarized-theme)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/solarized-theme")
;; (load-theme 'solarized-dark t)

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
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))

;;---------------------------
;; C/C++
;;---------------------------
(defun c-c++-mode-init ()
  (setq c-basic-offset 4))
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
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . php-mode))

;;---------------------------
;; TypeScript
;;---------------------------
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(require 'tide)
(add-hook 'typescript-mode-hook (lambda ()
                                  (tide-setup)
                                  (flycheck-mode t)
                                  (setq flycheck-check-syntax-automatically)
                                  (eldoc-mode t)
                                  (company-mode t)))


(require 'web-mode)
(setq web-mode-content-types-alist '(("jsx" . ".*\\.tsx?")))

;;---------------------------
;; JavaScript
;;---------------------------
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$'" . js2-mode))
(flycheck-add-mode 'javascript-eslint 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.jsx$'" . js2-jsx-mode))
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

(add-hook 'js2-mode-hook '(lambda ()
                            (setq js2-basic-offset 2)
                            (setq indent-tabs-mode nil)
                            (flycheck-mode 1)))
(add-hook 'js2-jsx-mode-hook '(lambda ()
                               (flycheck-mode 1)))


;;---------------------------
;; JSON
;;---------------------------
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$'" . json-mode))
(add-hook 'json-mode-hook '(lambda ()
                             (make-local-variable 'js-indent-level)
                             (setq js-indent-level 2)))

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
                             (setq tab-width 2)
                             (setq ruby-indent-level 2)
                             (setq ruby-indent-tabs-mode nil)
                             (setq ruby-deep-indent-paren-style nil)
                             (setq flycheck-checker 'ruby-rubocop)
                             (flycheck-mode 1)
                             (setq ruby-deep-indent-paren-style nil)))
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-commit-mode-on)


(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee?\\'" . coffee-mode))

(require 'slim-mode)
(add-to-list 'auto-mode-alist '("\\.slim?\\'" . slim-mode))

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
