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
    ;;;; input method
    ddskk
    ;;;; path env
    exec-path-from-shell
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
    ;;;; quickrun
    quickrun
    ;;;; lisp
    slime ac-slime ;sly ac-sly
    ;;;; clojure
    clojure-mode inf-clojure cider ac-cider
    clojure-cheatsheet
    clj-refactor
    ;;;; helm
    helm helm-descbinds helm-ag
    helm-projectile
    ;;;; helm with programming
    helm-pydoc
    helm-gtags
    ;;;; cpp
    irony
    auto-complete-clang-async
    ;;;; rust
    rust-mode
    racer ac-racer flycheck-rust
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
    google-translate
    undo-tree
    json-reformat
    kanban
    pomodoro
    projectile
    popwin
    powerline
    
    ;;; hobby
    twittering-mode
    sudden-death
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

;;; skk
(use-package ddskk
  :init
  (global-set-key (kbd "C-`") 'skk-mode))

;;; path env
(use-package exec-path-from-shell
  :config
  (let ((envs '("PATH" "GOPATH")))
    (exec-path-from-shell-copy-envs envs)))

;;; quickrun
(use-package quickrun
  :init
  (global-set-key (kbd "C-x C-r") 'quickrun))

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
(autoload 'gauche-manual "gauche-manual" "jump to gauche online manual." t)
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
(add-to-list 'exec-path "~/.ndenv/shims")
(add-to-list 'exec-path "~/.exenv/shims")
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

;;;;;;;;;
;; cpp ;;
;;;;;;;;;

;; irony
(use-package irony
  :init
  :config
  (progn
    (setq irony-server-install-prefix "/home/kaki/.emacs.d/irony/bin")
    (add-hook 'irony-mode-hook 'irony-eldoc)
    (add-to-list 'ac-sources 'ac-source'irony)
    (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async)))

;;;;;;;;;;
;; rust ;;
;;;;;;;;;;
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;; rust-mode
(use-package rust-mode
  :mode (("\\.rs$" . rust-mode))
  :config
  (setq-default rust-format-on-save t)
  (setq-default  rust-run-clippy t))
;; racer
(use-package racer
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

;; ac-racer
(use-package ac-racer
  :config
  (add-hook 'racer-mode 'ac-racer-setup))

;;;;;;;;;;;;
;; python ;;
;;;;;;;;;;;;

;; jedi
(use-package jedi
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

;; exenv
(use-package exenv
  :load-path "github/exenv.el/"
  :config
  (global-exenv-mode))

;; alchemist
(use-package alchemist
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  ;; (setq alchemist-mix-command "mix")
  ;; (setq alchemist-mix-test-task "test")
  ;; (setq alchemist-compile-command "elixirc")
  (setq alchemist-project-compile-wehn-needed t)
  (setq alchemist-goto-elixir-source-dir "~/src/elixir/src"))

(use-package ac-alchemist
  :config
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
                                    /usr/include/gtk-2.0
                                    /usr/include/sys")))
    (setq ac-clang-cflags (append '("-std=c++1y") ac-clang-cflags))
    (ac-clang-launch-completion-process))
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'c++-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-set-key "\M-RET" 'ac-start)
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

;; powerline
(use-package powerline
  :config
  (powerline-default-theme))

;; google-translate
(use-package google-translate
  :config
  (global-set-key (kbd "C-x t") 'google-translate-at-point)
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja")))

;; popwin
(use-package popwin
  :config
  (popwin-mode 1)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  ;; google-translate.el's buffer
  (push '("*Google Translate*") popwin:special-display-config))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" default)))
 '(fci-rule-color "#49483E")
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (sudden-death twittering-mode powerline popwin pomodoro kanban undo-tree google-translate codic systemd pandoc-mode puppet-mode ninja-mode graphviz-dot-mode idle-highlight-mode yaml-mode vimrc-mode bison-mode adoc-mode yatex gist git-gutter magit monokai-theme nasm-mode llvm-mode processing-mode json-mode web-mode markdown-mode scss-mode requirejs-mode requirejs tide typescript-mode tern-auto-complete tern js2-mode ac-alchemist alchemist elixir-mode erlang yard-mode rspec-mode flymake-ruby robe rbenv rake django-snippets django-mode pydoc jedi pyenv-mode auto-complete-clang-async helm-gtags helm-pydoc helm-projectile helm-ag helm-descbinds clj-refactor yasnippet use-package quickrun inf-clojure flymake-yaml flymake-sass flymake-puppet flymake-json flymake-jshint flymake-haml flymake-elixir flycheck-pyflakes flycheck-irony exec-path-from-shell ddskk clojure-cheatsheet ac-slime ac-emoji ac-cider)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
