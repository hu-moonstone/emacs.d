;;; init.el -- Emacs Settings
;;; Commentary:
;;; Code:

(setq gc-cons-threshold (* 128 1024 1024))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Byte compile
;;$ emacs -batch -f batch-byte-compile /path/to/*.el
(setq load-path (cons "~/.emacs.d/conf" load-path))

(load "basic-init")

;; el-getの設定
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle 'elfeed)
(load "elfeed-init")

(el-get-bundle 'cherry-blossom-theme)
(load "theme-init")

(el-get-bundle 'powerline)
(el-get-bundle 'tabbar)
(load "ui-init")

(el-get-bundle 'projectile)
(el-get-bundle 'flycheck)
(el-get-bundle 'helm)
(el-get-bundle 'helm-ag)
(el-get-bundle 'helm-ls-git)
(el-get-bundle 'editorconfig)
(el-get-bundle 'company)

(load "project-init")

;;Ruby
;; (el-get-bundle 'flymake-ruby)
;; (el-get-bundle 'motion-mode)
;; (el-get-bundle 'ruby-mode)
;; (el-get-bundle 'ruby-electric)
;; (el-get-bundle 'ruby-block)
;; (el-get-bundle 'inf-ruby)
;; (el-get-bundle 'projectile-rails)
;; (load "ruby-init")

;; Nginx
(el-get-bundle 'nginx-mode)

;; Shell
(el-get-bundle 'multi-term)

;; Codic
(el-get-bundle 'codic)

;; (el-get-bundle 'smooth-scroll)
;; (smooth-scroll-mode t)

;; emacs-w3m
(el-get-bundle 'w3)

(el-get-bundle 'auto-complete)
(el-get-bundle 'undo-tree)
;; (el-get-bundle evil) ;; VIモード

;; GIT
(el-get-bundle 'gitconfig-mode)
(el-get-bundle 'gitignore-mode)

;; SASS
(el-get-bundle 'sass-mode)

;; OTHER
(el-get-bundle 'pkg-info)
(el-get-bundle 'rainbow-delimiters)
(el-get-bundle 'move-text)

;; C/C++

;; LISP
;; CommonLisp

;; nginx-conf
(el-get-bundle 'nginx-mode)

;; Coffee
(el-get-bundle 'coffee-mode)

;; pug-mode
(el-get-bundle 'hlissner/emacs-pug-mode)
(autoload 'pug-mode "pug-mode" nil t)

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

;; Clojure
(el-get-bundle 'clojure-mode)
(el-get-bundle 'cider)
(autoload 'clojure-mode "clojure-mode" nil t)
(add-hook 'clojure-mode-hook #'company-mode)

(autoload 'cider "cider" nil t)


;; Markdown
(el-get-bundle 'markdown-mode)
(el-get-bundle 'markdown-preview-mode)

;; HTMLとその他の言語の混合
(el-get-bundle 'web-mode)

(autoload 'web-mode "web-mode" nil t)
(eval-after-load "web-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))))
(defun web-mode-hook ()
  (setq web-mode-engines-alist
        '(("php" . "\\.ctp\\'"))))
(add-hook 'web-mode-hook 'web-mode-hook)

;; Stylus
(el-get-bundle 'vladh/stylus-mode)

(autoload 'stylus-mode "stylus-mode" nil t)
(eval-after-load "stylus-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))))



;;---------------------------
;; LISP
;;---------------------------
(eval-after-load 'lisp-mode
  '(progn
     (add-to-list 'auto-mode-alist '("\\.ls$" . lisp-mode))
     (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))))

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

;; C/C++
(defun c-c++-mode-init ()
  (setq c-set-style "gnu"))
(add-hook 'c-mode-hook 'c-c++-mode-init)
(add-hook 'c++-mode-hook 'c-c++-mode-init)

;; GLSL
(el-get-bundle 'glsl-mode)
(autoload 'glsl-mode "glsl-mode" nil t)
(eval-after-load "glsl-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))
     (add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))
     (add-to-list 'auto-mode-alist '("\\.vertex\\'" . glsl-mode))
     (add-to-list 'auto-mode-alist '("\\.fragment\\'" . glsl-mode))))

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

;; (require 'tide)
;; (add-hook 'typescript-mode-hook (lambda ()
;;                                   (tide-setup)
;;                                   (flycheck-mode t)
;;                                   (setq flycheck-check-syntax-automatically)
;;                                   (eldoc-mode t)
;;                                   (company-mode t)))

(setq web-mode-content-types-alist '(("jsx" . ".*\\.tsx?")))

;;---------------------------
;; JavaScript
;;---------------------------
(autoload 'js2-mode "js2-mode" nil t)
(eval-after-load "js2-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
     (add-to-list 'auto-mode-alist '("\\.pegjs$" . js2-mode))
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
                   (flycheck-mode))))))

;;---------------------------
;; JSON
;;---------------------------
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(autoload 'coffee-mode "coffee-mode" nil t)
(eval-after-load "coffee-mode"
  (add-to-list 'auto-mode-alist '("\\.coffee?\\'" . coffee-mode)))

(autoload 'yaml-mode "yaml-mode" nil t)
(eval-after-load "yaml-mode"
  (add-to-list 'auto-mode-alist '("\\.yaml?\\'" . yaml-mode)))

;;---------------------------
;; Python
;;---------------------------
(autoload 'python-mode "python-mode" nil t)
(eval-after-load "python-mode"
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

;;---------------------------
;; Markdown Setting
;;---------------------------
(autoload 'markdown-mode "markdown-mode" nil t)
(eval-after-load "markdown-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
     (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (w3 queue gitignore-mode gitconfig-mode esup company cherry-blossom-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
