;;; ruby-init.el -- Emacs Settings
;;; Commentary:
;;; Code:


;; Inf-ruby (Pry)
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-eval-binding "Pry.toplevel_binding")

(autoload 'projectile-rails "projectile-rails" nil t)
(eval-after-load "projectile-rails"
  (add-hook 'projectile-mode-hook 'projectile-rails-on))


;;---------------------------
;; Ruby用の拡張ロードと設定
;;---------------------------
(autoload 'ruby-mode "ruby-mode" nil t)
(eval-after-load "ruby-mode"
  '(progn
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
     (add-hook 'inf-ruby-mode-hook 'ansi-color-for-commit-mode-on)))
