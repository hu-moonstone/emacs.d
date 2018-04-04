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

(setq browse-url-browser-function 'browse-url-generic
  browse-url-generic-program "google-chrome")

;; フォント設定
(add-to-list 'default-frame-alist '(font . "Myrica M-14"))

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
