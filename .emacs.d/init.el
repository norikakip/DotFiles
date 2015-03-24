;; Melpa
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to_list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
(package-refresh-contents)

;; installing-package-list
(defvar my/favorite-packages
  '(
    ;;;; use-package
    use-package
    ;;;; auto-complete
    auto-complete
    ;;;; flymake
    flymake
    ;;;; lisp
    slime ac-slime
    ;;;; sly
    sly
    ;;;; helm
    helm helm-descbinds helm-ag 
    ;;;; jedi
    jedi
    ;;;; ruby
    rbenv inf-ruby robe flymake-ruby
    ;;; theme
    monokai-theme
    ;;; other
    markdown-mode codic
    ))
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

;;; 右から左に読む言語に対応させないことで描画高速化
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;;; 対応する括弧を表示する
(show-paren-mode t)

;;; 現在行に色をつける
(global-hl-line-mode 1)

;;; 不透明度調整
(set-frame-parameter nil 'alpha' 90)
;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; *.~とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#*とかのバックアップファイルを作らない
(setq auto-save-default nil)

;;; ^H をバックススペースへ
(global-set-key "\C-h" 'delete-backward-char)

;;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)

;;; ログの記録行数を増やす
(setq message-log-max 100000)

;;; 履歴をたくさん保存する
(setq history-length 10000)

;;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; カーソルキーでwindmove
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)


;;; helm
(when (require 'helm-config nil t)
  (helm-mode 1)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-c i") 'helm-imenu)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  (define-key helm-map(kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line` in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line` in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regex-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

;; emacsでGauche
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "/usr/bin/gosh -i")

(autoload 'scheme-mode "cmuscheme" "Major mode for Spcheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

;; gauche-manual
(autoload 'gauche-manua "gauche-manual" "jump to gauche online manual." t)
(add-hook 'scheme-mode-hook
	  '(lambda ()
	     (define-key scheme-mode-map "\C-c\C-f" 'gauche-manual)))

(defun scheme-other-window()
  "Run Gauche on other window"
  (interactive)
  (split-window-horizontally (/ (frame-width) 2))
  (let ((buf-name (buffer-name (current-buffer))))
    (scheme-mode)
    (switch-to-buffer-other-window
      (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window
      (get-buffer-create buf-name))))

(define-key global-map
  "\C-cG" 'scheme-other-window)

;; slime
(when (require 'slime nil t)
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))
  
;; path
(add-to-list 'exec-path "~/.rbenv/shims")
(add-to-list 'exec-path "~/.pyenv/shims")
;; theme
(load-theme 'monokai t)

;; electric-indent
(electric-indent-mode t)

;; line number
(global-linum-mode t)

;;; PROGRAMINNGS

;; ruby
; rbenv
(global-rbenv-mode)
(setq rbenv-installation-dir "~/.rbenv")

(add-to-list 'auto-mode-alist
             '("\\.rb$" . ruby-mode))
(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (robe-mode)
                      (robe-start)
                      (add-to-list 'electric-pair-pairs '(?| . ?|)))))
