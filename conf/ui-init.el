;;; ui-init.el -- Emacs Settings
;;; Commentary:
;;; Code:

(autoload 'powerline "powerline" nil t)
(defun powerline-my-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq powerline-current-separator 'utf-8)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
                          (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (lhs (list (powerline-raw " " face1)
                                     (powerline-major-mode face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-buffer-id nil )
                                     (powerline-raw " [ ")
                                     (powerline-raw mode-line-mule-info nil)
                                     (powerline-raw "%*" nil)
                                     (powerline-raw " |")
                                     (powerline-process nil)
                                     (powerline-vc)
                                     (powerline-raw " ]")
                                     ))
                          (rhs (list (powerline-raw "%4l" 'l)
                                     (powerline-raw ":" 'l)
                                     (powerline-raw "%2c" 'l)
                                     (powerline-raw " | ")
                                     (powerline-raw "%6p" )
                                     (powerline-raw " ")
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill nil (powerline-width rhs))
                             (powerline-render rhs)))))))

(defun make/set-face (face-name fg-color bg-color weight)
  (make-face face-name)
  (set-face-attribute face-name nil
                      :foreground fg-color :background bg-color :box nil :weight weight))

(with-eval-after-load "powerline"
  (make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
  (make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
  (make/set-face 'mode-line-1-arrow  "#666666" "#3E4451" 'bold)
  (make/set-face 'mode-line-2-arrow  "#666666" "#3E4451" 'bold)
  (powerline-my-theme))


(autoload 'tabbar "tabbar" nil t)
(eval-after-load "tabbar"
  '(progn
     (tabbar-mode 1)
     (setq tabbar-buffer-groups-function nil)
     (setq tabbar-auto-scroll-flag t)
     (dolist (btn '(tabbar-buffer-home-button
                    tabbar-scroll-left-button
                    tabbar-scroll-right-button))
       (set btn (cons (cons "" nil)
                      (cons "" nil))))

     ;; タブの長さ
     (setq tabbar-separator '(2.2))

     ;; キーに割り当てる
     (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
     (global-set-key (kbd "<C-iso-lefttab>") 'tabbar-backward-tab)
     (set-face-attribute
      'tabbar-default nil
      :family "Myrica M"
      :background "#503453"
      :foreground "#fff"
      :bold nil
      :height 1.0)
     (set-face-attribute
      'tabbar-unselected nil
      :background "#503453"
      :foreground "#fff"
      :bold nil
      :box nil)
     (set-face-attribute
      'tabbar-modified nil
      :background "#503453"
      :foreground "gray23"
      :bold t
      :box nil)
     (set-face-attribute
      'tabbar-selected nil
      :background "#000000"
      :foreground "#fff"
      :bold nil
      :box nil)
     (set-face-attribute
      'tabbar-button nil
      :box nil)
     (set-face-attribute
      'tabbar-separator nil
      :height 1.0)))

(provide 'ui-init)
;;; ui-init.el ends here
