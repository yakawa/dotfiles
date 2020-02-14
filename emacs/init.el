;; Emacs init.el
;; -*- coding: utf-8 -*-
;;

;; Package 設定
(require 'package)

;; Milkypostman’s Emacs Lisp Package Archie
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
; Org Emacs Lisp Package Archive
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
; Tromey Emacs Lisp Package Archive
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-ensure-function
			(require 'use-package)
			(setq use-package-always-ensure t))

;; Backup
(add-to-list 'backup-directory-alist (cons "." (expand-file-name "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*", (expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-timeout 15)
(setq auto-save-interval 300)

(set-locale-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(menu-bar-mode -1) ;; メニューバーの非表示
(tool-bar-mode -1) ;; ツールバーの非表示
(column-number-mode t) ;; カラム番号の表示
(size-indication-mode t) ;; ファイルサイズの表示
(setq display-time-day-and-date t) ;; 時計の表示モード
(display-time-mode t) ;; 時計表示モード
(setq frame-title-format "%f") ;; Frameにファイル名を表示する
(global-linum-mode -1) ;; 行番号の非表示
(show-paren-mode t) ;; 対応するカッコを光らせる
(transient-mark-mode t) ;; リージョンに色を付ける
(setq-default tab-width 2) ;; <Tab> の Width を 2 * <space> にする
(setq inhibit-startup-screen t) ;; Start-up を表示しない
(defalias 'message-box 'message) ;; message-boxの代わりにmessageを使う
(setq use-dialog-box nil) ;; Dialog Boxを使わない
(setq which-function-mode t) ;; 現在の関数名を表示する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-min-dir-content 2)
(setq uniquify-ignore-buffers-re "*[^*]+*")

(savehist-mode t) ;; histの保存
(setq history-length 1500)
(require 'saveplace)
(save-place-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.1)
(define-key minibuffer-local-filename-completion-map (kbd "?") nil)
(setq ffap-pass-wildcards-to-dired t)
(ffap-bindings)

(setq kill-whole-line t)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq gc-cons-threshold (* 100 gc-cons-threshold))
(setq message-log-max 10000)
(setq large-file-warning-threshold (* 10 1024 1024))

(global-auto-revert-mode t)
(setq completion-ignore-case t)
(setq case-fold-search nil)
(setq isearch-case-fold-search nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq case-replace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(setq vc-follow-symlinks t)
(setq create-lockfiles nil)
(setq vc-handled-backends nil)

;; company-mode
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idele-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq completion-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  :bind
  (("C-M-i" . company-complete)
   ("TAB" . company-indent-or-complete-common)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("C-i" . company-complete-selection)
   ("TAB" . company-complete-selection)
   ("C-f" . comapny-complete-selection)
   :map company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))



;; anzu
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 500)))

(use-package which-key
	:ensure t
	:config
	(which-key-mode 1)
	(which-key-setup-side-window-right-bottom))
	
(use-package helm
	:ensure t
	:bind
	(("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-r" . helm-for-files)
	 ("C-x C-y" . helm-show-kill-ring)
	 ("C-x C-b" . helm-buffers-list)
	 :map helm-map
	 ("C-h" . delete-backward-char)
	 :map helm-find-files-map
	 ("C-h" . delete-backward-char)
	 ("TAB" . helm-execute-persistent-action)
	 :map helm-read-file-map
	 ("TAB" . helm-execute-presisteny-action))
	:config
	(helm-mode t)
	(when (executable-find "curl") (setq helm-google-suggest-use-curl-p t))
	:config
	(setq helm-split-window-in-side-p t)
	(setq helm-move-to-line-cycle-in-source t)
	(setq helm-ff-search-library-in-sexp t)
	(setq helm-scroll-amount t)
	(setq helm-ff-fine-name-history-use-recentf t)
	(setq helm-echo-input-in-heder-line t)
	(setq helm-autoresize-max-height 0)
	(setq helm-autoresize-min-height 20)
	(setq helm-M-x-fuzzy-matching t)
	(setq helm-buffers-fuzzy-matching t)
	(setq helm-recentf-fuzzy-match t)
	(setq helm-more-fuzzy-match t)
	(setq helm-completion-in-region-fuzzy-match t)
	(setq helm-delete-minibuffer-contents-from-point t)
	(setq helm-display-function #'display-buffer)
	(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
		"Emulate `kill-line` in helm minibuffer"
		(kill-new (buffer-substring (point) (field-end))))
	(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
		"Execute command lnly if CANDIDATE exists"
		(when (file-exists-p candidate) ad-do-it)))

(use-package helm-descbinds
	:ensure t
	:config
	(helm-descbinds-install))

(use-package use-package-chords
	:ensure t
	:config
	(key-chord-mode t))

(use-package color-moccur
	:ensure t
	:init
	(setq moccur-split-word t)
	:config
	)

(use-package goto-chg
	:ensure t
	:bind
	(("<F8>" . goto-last-change)
	 ("S-<f8>" . goto-last-change-reverse)))

(use-package key-chord
	:ensure t
	:init
	(setq key-chord-two-keys-delay 0.04)
	:config
	(key-chord-mode t))

(use-package minor-mode-hack
	:ensure t)

(use-package multicolumn
	:ensure t
	:bind
	(
	 :map multicolumn-map
				("C-x 4 4" . multicolumn-dekete-other-windows-and-split-with-follow-mode)
				)
	:config
	(setq multicolumn-min-width 100))

(use-package neotree
	:ensure t
	:config
	(setq neo-show-hidden-file t)
	(setq neo-persist-show t)
	(setq neo-keymap-style 'concise)
	(setq neo-smart-open t)
	(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
	(when neo-presist-show
		(add-hook 'popwin:before-popup-hook
							(lambda () (setq neo-persist-show nil)))
		(add-hook 'popwin:after-popup-hook
							(lmbda () (setq neo-persist-show t))))
	:bind
	(
	 ("C-x C-t" . neotree-toggle)
	 ))

(use-package open-junk-file
	:ensure t
	:init
	(setq open-junk-file-format (expand-file-name "~/.emacs.d/junk/%Y/%Y%m%d_%H%M%S.md"))
	:bind
	(("C-x C-j" . open-junk-file))
	)

(use-package pandoc
	:ensure t
	:config
	(pandoc-turn-on-advice-eww))

(use-package recentf-ext
	:ensure t
	:config
	(setq recentf-max-saved-items 3000))

(use-package undohist
	:ensure t
	:init
	(setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
	:config
	(undohist-initialize))

(use-package undo-tree
	:ensure t
	:config
	(global-undo-tree-mode))

(use-package viewer
	:ensure t
	:config
	(viewer-stay-in-setup)
	(viewer-change-modeline-color-setup)
	(setq viewer-aggressive-setup t)
	(setq viewer-modeline-color-unwritable "tomato")
	(setq viewer-modeline-color-view "orange")
	(setq view-read-only t)
	(setq view-mode-by-default-regexp "\\.log$")
	:bind
	(
	 :map view-mode-map
				("N" . view-search-last-regexp-backward)
				("?" . view-search-regexp-backward)
				("G" . view-goto-line-last)
				("b" . view-scroll-page-backward)
				("f" . view-scroll-page-forward)
				("h" . backward-char)
				("j" . next-line)
				("k" . previous-line)
				("l" . forward-char)
				("J" . view-scroll-line-forward)
				("K" . view-scroll-line-backward)
				))

(use-package magit
	:ensure t)

(use-package git-gutter
	:ensure t
	:config
	(global-git-gutter-mode t))

(use-package rainbow-delimiters
	:init
	(require 'cl-lib)
	(require 'color)
	(setq rainbow-delimiters-outermost-only-face-count 1)
	:config
	(rainbow-delimiters-mode t)
	(set-face-foreground 'rainbow-delimiters-depth-1-face "#9a4040")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#f0f0f0")
	:hook
	(
	 (emacs-lisp-mode . rainbow-delimiters-mode)
	 ))

(use-package flycheck
	:ensure t
	:hook
	(after-init-hook . global-flycheck-mode))

(use-package smart-cursor-color
	:ensure t
	:config
	(smart-cursor-color-mode t))

(use-package web-mode
	:ensure t
	:mode
	(("\\.html?$\\" . web-mode)))

(use-package markdown-mode
	:ensure t
	:mode
	(("\\.md$" . gfm-mode)
	 )
	:bind
	(("C-c C-p" . markdown-preview)))

(use-package yaml-mode
	:ensure t
	:mode
	(("\\.yml" . yaml-mode)
	 ("\\.yaml" . yaml-mode)
	 ))

(use-package python-mode
	:ensure t
	:mode
	(("\\.py$" . python-mode))
	:interpreter
	(("python" . python-mode)
	 ("python3" . python-mode))
	:config
	(python-mode)
	(setq python-indent-guess-indent-offset-verbose nil)
	(setq electric-indent-local-mode nil)
	(setq py-smart-indentation t)
	(setq python0indent-offset 2)
	(setq python-shell-interpreter "python3")
	(setq python-shell-completion-native-disabled-interpreters '("python3"))
	(flymake-mode nil)
	(outline-minor-mode t))

(defun go-fmt-hook()
	"Go formatter"
	(add-hook 'before-save-hook 'gofmt-before-save)
	(setq indent-tabs-mode nil)
	(setq c-basic-offet 4)
	(setq tab-width 4))

(use-package go-mode
	:ensure t
	:hook
	((go-mode . go-fmt)
	))

(use-package company-go
	:ensure t)

(use-package go-eldoc
	:ensure t
	:config
	(set-face-attribute 'eldoc-highlight-function-argument nil)
	:hook
	(go-mode . go-eldoc-setup))

(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/.go/bin"))

(defun set-exec-path-from-shell-PATH()
	"Read $PATH from shell"
	(interactive)
	(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
		(setenv "PATH" path-from-shell)
		(setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(defun paste-to-clipbord (text &optional push)
	(let ((process-connection-type nil))
		(let ((proc (start-process "pbcopy" "*Message*" "pbcopy")))
			(process-send-string proc text)
			(process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-clipboard)

(defun web-mode-hook2()
	"hook"
	(setq web-mode-html-offset 2)
	(setq web-mode-style-padding 2)
	(setq web-mode-css-offset 2)
	(setq web-mode-script-offset 2)
	(setq web-mode-javascript-offset 2)
	(setq web-mode-java-offset 2)
	(setq web-mode-asp-offset 2)
	(setq web-mode-tag-auto-close-style 2))
(add-hook 'web-mode-hook 'web-mode-hook2)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kdb "C-x ?") 'help-command)
(global-set-key (kbd "C-m") 'newlinw-and-indent)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-o") 'next-line)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "S-[") 'switch-to-prev-buffer)
(global-set-key (kbd "S-]") 'switch-to-next-buffer)

