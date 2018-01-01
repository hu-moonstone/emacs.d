;;; project-init.el -- Emacs Settings
;;; Commentary:
;;; Code:

(autoload 'helm "helm" nil t)
(eval-after-load "helm"
  (helm-mode 1))

(autoload 'helm-projectile "helm-projectile" nil t)
(autoload 'helm-ls-git "helm-ls-git" nil t)

(autoload 'editorconfig "editorconfig" nil t)
(eval-after-load "editorconfig"
  (editorconfig-mode 1))


(autoload 'projectile "projectile" nil t)
(eval-after-load "projectile"
  '(progn
     (projectile-global-mode)
     (setq projectile-completion-system 'helm)))


(autoload 'flycheck "flycheck" nil t)
(eval-after-load "flycheck"
  '(progn
     (global-flycheck-mode)
     (setq flycheck-check-syntax-automatically '(mode-enabled save))))

(autoload 'company "company" nil t)
(eval-after-load 'company
  '(progn
     (global-company-mode 1)
     (setq company-idle-delay 0.1
           company-minimum-prefix-length 2
           company-selection-wrap-around t)
     (define-key company-active-map (kbd "C-i") 'company-complete)
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (set-face-attribute 'company-tooltip nil
                         :foreground "black" :background "lightgrey")
     (set-face-attribute 'company-tooltip-common nil
                         :foreground "black" :background "lightgrey")
     (set-face-attribute 'company-tooltip-common-selection nil
                         :foreground "white" :background "steelblue")
     (set-face-attribute 'company-tooltip-selection nil
                         :foreground "black" :background "steelblue")
     (set-face-attribute 'company-preview-common nil
                         :background nil :foreground "lightgrey" :underline t)
     (set-face-attribute 'company-scrollbar-fg nil
                         :background "orange")
     (set-face-attribute 'company-scrollbar-bg nil
                         :background "gray40")))



(provide 'project-init)
;;; project-init ends here
