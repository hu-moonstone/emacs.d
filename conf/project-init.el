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

(provide 'project-init)
;;; project-init ends here
