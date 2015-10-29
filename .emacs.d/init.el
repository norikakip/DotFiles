;; Melpa
(require 'package)
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
    ;;;; completion
    auto-complete
    ac-emoji
    ;;;; snippet
    yasnippet
    ;;;; flymake
    flymake
    flymake-elixir
    flymake-haml
    flymake-jshint
    flymake-json
    flymake-puppet
    flymake-sass
    flymake-yaml
    ;;;; flycheck
    flycheck
    flycheck-irony
    flycheck-pyflakes
    flycheck-processing
    ;;;; quickrun
    quickrun
    ;;;; lisp
    slime ac-slime ;sly ac-sly
    ;;;; clojure
    clojure-mode inf-clojure cider ac-cider
    clojure-cheatsheet slamhound
    ;;;; helm
    helm helm-descbinds helm-ag
    helm-projectile
    ;;;; helm with programming
    helm-pydoc
    ;;;; cpp
    irony
;    auto-complete-clang-async
    ;;;; python
    pyenv-mode jedi pydoc
    django-mode django-snippets
    ;;;; ruby
    rake rbenv robe flymake-ruby rspec-mode
    yard-mode
    ;;;; erlang
    erlang
    ;;;; elixir
    elixir-mode alchemist ac-alchemist
    ;;;; javascript
    js2-mode tern tern-auto-complete
    ;;;; typescript
    typescript-mode tide requirejs requirejs-mode
    ;;;; web
    scss-mode markdown-mode web-mode
    json-mode
    ;;;; processing
    processing-mode
    ;;;; assembly
    llvm-mode nasm-mode
    ;;; theme
    monokai-theme
    ;;; git tools
    magit git-gutter gist
    ;;; other tools
    yatex
    adoc-mode
    bison-mode
    vimrc-mode
    yaml-mode
    idle-highlight-mode
    graphviz-dot-mode ninja-mode
    puppet-mode
    pandoc-mode
    systemd                             ; Major mode for editing systemd units
    codic
    undo-tree
    json-reformat
    kanban
    pomodoro
    projectile
    sudden-death
    ;;; hobby
    twittering-mode
    ))
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

;;; start splash screen disable
(setq inhibit-splash-screen t)
;;; load newer file compiled or not
(setq load-prefer-newer t)
;;; scratch initial message
(setq initial-scratch-message
"
(find-file \"/tmp/scratch.rb\")
(find-file \"/tmp/scratch.py\")

(find-file \"/tmp/scratch.c\") 

(find-file \"/tmp/scratch.cpp\")

(find-file \"/tmp/scratch.ex\")
(find-file \"/tmp/scratch.exs\")
"
)
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

;;; undo-tree
(use-package undo-tree
  :init
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo))

;;; auto-complete
(use-package auto-complete
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (setq ac-sources (append '(ac-source-gtags
                             ac-source-yasnippet)
                           ac-sources))
  (auto-complete))

;;; yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1))

;;; quickrun
(use-package quickrun
  :init
  (global-set-key "\C-x\C-r" 'quickrun))

;;; helm
(when (require 'helm-config nil t)
  (helm-mode 1)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-h") 'helm-recentf)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-c i") 'helm-imenu)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "M-g") 'helm-ag)

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

  
;; path
(add-to-list 'exec-path "~/.rbenv/shims")
(add-to-list 'exec-path "~/.pyenv/shims")
;; theme
(load-theme 'monokai t)

;; electric-indent
(electric-indent-mode t)

;; line number
(global-linum-mode t)

;; font
(set-face-attribute 'default nil
                    :family "Ricty")
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Ricty"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("Ricty"))
(add-to-list 'default-frame-alist
             '(font . "Ricty-16"))

;;;;;;;;;;;;;;;;;;;
;;  PROGRAMMING  ;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; python ;;
;;;;;;;;;;;;

;; jedi
(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t))

;;;;;;;;;;
;; ruby ;;
;;;;;;;;;;

(use-package ruby-mode
             :interpreter (("ruby"  . ruby-mode)
                           ("jruby" . ruby-mode))
             :mode (("\\.rb$" . ruby-mode)
                    ("Capfile$" . ruby-mode)
                    ("Gemfile$" . ruby-mode)
                    ;; shebangがrubyの場合、ruby-modeを開く
                    ("config.ru$" . ruby-mode))
             
             :config
             (add-to-list 'electric-pair-mode '(?| . ?|)))

;; rbenv
(use-package rbenv
             :config
             (global-rbenv-mode)
             (setq rbenv-installation-dir "~/.rbenv"))

;; robe
(use-package robe
  :commands (robe-mode robe-start)
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup))

;;;;;;;;;;;;
;; elixir ;;
;;;;;;;;;;;;

;; elixir-mode
(use-package elixir-mode
  :mode (("\\.ex$" . elixir-mode)
         ("\\.exs$" . elixir-mode)
         ("\\.elixir2\\'" . elixir-mode)))

;; alchemist
(use-package alchemist
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (setq alchemist-mix-command "mix")
  (setq alchemist-mix-test-task "test")
  (setq alchemist-compile-command "elixirc"))

(use-package ac-alchemist
  :init
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup)
  (add-hook 'alchemist-iex-mode-hook 'ac-alchemist-setup))

;;;;;;;;;;
;; lisp ;;
;;;;;;;;;;

;; ac-slime
(use-package ac-slime
  :init
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))

;;;;;;;;;;;;;
;; clojure ;;
;;;;;;;;;;;;;

;; cider
(use-package cider
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  :config
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-buffer-name-show-port t))

;; ac-cider
(use-package ac-cider
  :init
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (add-to-list 'ac-modes 'cider-mode)
  (add-to-list 'ac-modes 'cider-repl-mode))

;;;;;;;;;
;; cpp ;;
;;;;;;;;;

;; clang-complete
(use-package auto-complete-clang-async
  :init
  (defun ac-cc-mode-setup ()
    (setq ac-clang-complete-executable (expand-file-name "~/.emacs.d/bin/clang-complete"))
    (setq ac-sources (append '(ac-source-clang-async) ac-sources))
    (setq ac-clang-cflags (mapcar (lambda (item)
                                    (concat "-I" (expand-file-name item)))
                                  (split-string
                                   "/usr/include
                                    /usr/include/c++/4.9.2
                                    /usr/include/boost
                                    /usr/include/sys")))
    (setq ac-clang-cflags (append '("-std=c++1y") ac-clang-cflags))
    (ac-clang-launch-completion-process))
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'c++-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-set-key "\M-/" 'ac-start)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  :config
  (auto-complete-mode t))

;;;;;;;;;;;;;;;;;
;; javascript  ;;
;;;;;;;;;;;;;;;;;

;; tern
(use-package tern
  :init
  (add-hook 'js2-mode-hook 'tern-mode))

;; tern-auto-complete
(use-package tern-auto-complete
  :init
  (add-hook 'tern-mode-hook 'tern-ac-setup))

;;;;;;;;;;;
;; other ;;
;;;;;;;;;;;

;; yaml-mode
(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)))

;; web-mode
(use-package web-mode
  :mode (("\\.erb$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t))

;; git-gutter
(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

;;;;;;;;;;;
;; hobby ;;
;;;;;;;;;;;

;; twittering-mode
(use-package twittering-mode
  :init
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t)
  (setq twittering-timer0interval 300))


(global-set-key "\C-q" 'kill-buffer-and-window)
(global-set-key "\C-m" 'newline-and-indent)
