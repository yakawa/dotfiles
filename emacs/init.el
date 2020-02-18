;;; init.el --- Configuration for emacs
;; -*- coding: utf-8 -*-
;;; Commentary:
;;

;;; Code:

;; Package / use-package 設定
(eval-when-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    )
  (defvar use-package-always-ensure t)
  (defvar use-package-expand-minimally t)

  (require 'use-package)
  )

(defun set-exec-path-from-shell-PATH()
  "Read $PATH from shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; Defaut Encoding
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "ja_JP.UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Startup
(setq inhibit-startup-screen t) ;; Start-up を表示しない
(setq inhibit-startup-message nil) ;; Start-up を表示しない
(setq inhibit-startup-echo-area-message nil) ;; Start-up を表示しない
(setq initial-scratch-message nil) ;; Start-up を表示しない

(setq ring-bell-function 'ignore)


;; Backup
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))
(setq auto-save-timeout 15)
(setq auto-save-interval 300)
(add-to-list 'backup-directory-alist (cons "." (expand-file-name "~/.emacs.d/backups")))
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 1)
(setq delete-old-versions t)

(if (or (equal window-system 'ns) (equal window-system 'mac))
    (menu-bar-mode nil)
  (menu-bar-mode t)
  )
(tool-bar-mode -1) ;; ツールバーの非表示
(column-number-mode t) ;; カラム番号の表示
(size-indication-mode t) ;; ファイルサイズの表示
(setq display-time-day-and-date t) ;; 時計の表示モード
(defvar display-time-24hr-format t)
(display-time-mode t) ;; 時計表示モード
(setq frame-title-format "%f") ;; Frameにファイル名を表示する
(global-linum-mode -1) ;; 行番号の非表示
(show-paren-mode t) ;; 対応するカッコを光らせる
(defvar show-paren-style 'expression)
(transient-mark-mode t) ;; リージョンに色を付ける
(setq-default tab-width 2) ;; <Tab> の Width を 2 * <space> にする
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

(setq initial-frame-alist
        (append (list
                 '(width . 137)
                 '(height . 137)
                 '(top . 0)
                 '(left . 0)
                 )
                initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config))

;; company-mode
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq completion-ignore-case nil)
  (setq company-selection-wrap-around t)
  (defvar company-dabbrev-downcase nil)
  (setq company-backends '(
                           company-files
                           (company-capf company-dabbrev)
                           (company-dabbrev-code company-gtags company-etags company-keywords)
                           ))
  :bind
  (("C-M-i" . company-complete)
   ("<tab>" . company-indent-or-complete-common)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("C-i" . company-complete-selection)
   ("<tab>" . company-complete-selection)
   ("C-f" . comapny-complete-selection)
   :map company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))

(defun edit-category-table-for-company-dabbrev (&optional table)
  "Table as for company Dabbrev."
  (define-category ?s "Word constituents for company-dabbrev" table)
  (let ((i 0))
    (while (< i 128)
      (if (equal ?w (char-syntax i))
          (modify-category-entry i ?s table)
        (modify-category-entry i ?s table t))
      (setq i (1+ i)))))
(edit-category-table-for-company-dabbrev)
(setq company-dabbrev-char-regexp "\\cs")

(use-package company-go
	:ensure t)
(push 'company-go company-backends)

(use-package irony
  :ensure t
  :after company
  :hook
  ((c-mode-hook . irony-mode)
   (c++-mode-hook . irony-mode)
   ))

(use-package company-irony
  :ensure t
  :config
  (push 'company-irony company-backends))

;; M-x jedi:install-server RET
(use-package company-jedi
  :ensure t
  :config
  (use-package jedi-core
    :ensure t
    :config
    (setq jedi:complete-on-dot t)
    (setq jedi:use-shortcuts t)
    :hook
    (python-mode-hook . jedi:setup)))
(push 'company-jedi company-backends)

(use-package helm-ag
  :ensure t
  :after helm
  :config
  (setq helm-ag-base-command "rg -S --vimgrep --no-heading")
  )

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
	(
   ("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
	 ("C-x C-r" . helm-for-files)
	 ("C-x C-y" . helm-show-kill-ring)
	 ("C-x C-b" . helm-buffers-list)
	 :map helm-map
	 ("C-h" . delete-backward-char)
   ("<tab>" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
	 :map helm-find-files-map
	 ("C-h" . delete-backward-char)
	 ("TAB" . helm-execute-persistent-action)
	 :map helm-read-file-map
	 ("<tab>" . helm-execute-presisteny-action))
	:config
	(helm-mode t)
	:config
	(setq helm-split-window-inside-p nil)
	(setq helm-move-to-line-cycle-in-source t)
	(setq helm-ff-search-library-in-sexp t)
	(setq helm-scroll-amount t)
	(setq helm-ff-fine-name-history-use-recentf t)
	(setq helm-echo-input-in-heder-line t)
	(setq helm-autoresize-max-height 0)
	(setq helm-autoresize-min-height 40)
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
	(helm-descbinds-mode))

(use-package color-moccur
	:ensure t
	:init
	(setq moccur-split-word t)
	:config
	)

(use-package goto-chg
	:ensure t
	:bind
	(("<f8>" . goto-last-change)
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

(use-package all-the-icons
  :ensure t)

(use-package neotree
	:ensure t
  :commands
  (neotree-show neotree-hide netree-dir neotree-find)
  :custom
  (neo-theme 'nerd2)
	:config
	(defvar neo-show-hidden-file t)
	(defvar neo-persist-show t)
	(setq neo-keymap-style 'concise)
	(setq neo-smart-open t)
	(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
	(when neo-persist-show
		(add-hook 'popwin:before-popup-hook
							(lambda () (setq neo-persist-show nil)))
		(add-hook 'popwin:after-popup-hook
							(lambda () (setq neo-persist-show t))))
	:bind
	(
	 ("<f9>" . neotree-toggle)
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

(use-package helm-swoop
  :ensure t
  :after helm
  :bind("M-i" . helm-swoop)
  )

(use-package recentf-ext
	:ensure t
	:config
	(setq recentf-max-saved-items 3000))

(use-package undohist
	:ensure t
	:init
	(setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
  :commands
  (undohist-initialize)
	:config
  (undohist-initialize))

(use-package undo-tree
	:ensure t
	:config
	(global-undo-tree-mode))

(use-package viewer
	:ensure t
  :commands
  (viewer-stay-in-setup viewer-change-modeline-color-setup viewer-aggressive-setup)
	:config
	(viewer-stay-in-setup)
	(viewer-change-modeline-color-setup)
	(viewer-aggressive-setup t)
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
	:ensure t
  :bind
  (("C-x g" . magit-status)
  ))

(use-package git-gutter
	:ensure t
	:config
	(global-git-gutter-mode t))

(use-package flycheck
	:ensure t)
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package pos-tip
  :ensure t)

;;(use-package flycheck-pos-tip
;;  :ensure t)
;;(flycheck-pos-tip-mode)

(use-package helm-flycheck
  :ensure t)

(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-dark t)

(use-package smart-cursor-color
	:ensure t
	:config
	(smart-cursor-color-mode t))

(use-package web-mode
	:ensure t
	:mode
	(
   ("\\.html?$" . web-mode)
   ("\\.js$" . web-mode)
   ("\\.css$" . web-mode)
   ("\\.json$" . web-mode)
   )
  )

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

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
	(("\\.yml$" . yaml-mode)
	 ("\\.yaml$" . yaml-mode)
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

(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/.go/bin"))

(use-package go-eldoc
	:ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred lsp-format-buffer lsp-organize-imports)
  :hook (go-mode . lsp-deferred))
(setq lsp-gopls-use-placeholders t)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "Lsp go install save hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends)
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package go-mode
  :ensure t
  :hook
  (
   (go-mode-hook . lsp)
   (go-mode-hook . company-mode)
   (go-mode-hook . go-eldoc-setup)
   (before-save-hook . gofmt-before-save)
   )
  )
(setq indent-tabs-mode nil)
(defvar c-basic-offet 2)
(setq tab-width 2)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :config
  (lsp-metals-treeview-enable t)
  )


(setq lsp-log-io nil)
(setq lsp-print-performance nil)
(setq lsp-auto-guess-root nil)
(setq lsp-response-timeout 5)

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-max-width 150)
(setq lsp-ui-doc-max-height 30)
(setq lsp-ui-peek-enable t)

(use-package direx
  :ensure t)

(use-package go-direx
  :ensure t
  :bind
  (("C-c C-j" . go-direx-pop-to-buffer)))

(use-package page-break-lines
  :ensure t)

(when (or (eq system-type 'gnu/linux) (and (eq system-type 'darwin)(eq window-system nil)))
  (use-package dashboard
    :ensure t
    :config
    (setq dashboard-center-content t)
    (dashboard-setup-startup-hook)
    )
  )
(when (and (eq system-type 'darwin) (not (eq window-system nil)))
  (use-package dashboard
    :ensure t
    :hook
    (after-init . dashboard-setup-startup-hook)
    :config
    (setq dashboard-banner-logo-title
          (concat "GNU Emacs " emacs-version " kernel "
                  (car (split-string (shell-command-to-string "uname -r"))) " x86_64 Mac OS X"
                  (car (split-string (shell-command-to-string "sw_vers -productVersion") "-"))))
    (setq dashboard-center-content t)
    )
  )

(use-package hide-mode-line
  :hook
  ((minimap-pode) . hide-mode-line-mode))

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package review-mode
  :ensure t
  :mode
  ("(\\.re$" . review-mode)
  )

(use-package git-commit
  :ensure t)

(use-package flyspell
  :if (executable-find "aspell")
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (before-save-hook . flyspell-buffer)
  (flyspell-mode . (lambda ()
                     (dolist (key '("C-;" "C-," "C-."))
                       (unbind-key key flyspell-mode-map))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))


(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

(when (eq system-type 'darwin)
  (defun copy-from-osx()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcoy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  )

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-o") 'next-line)
(global-set-key (kbd "S-[") 'switch-to-prev-buffer)
(global-set-key (kbd "S-]") 'switch-to-next-buffer)
(global-set-key (kbd "M-r") 'rename-file)


(add-hook 'python-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'go-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'xml-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))

(unless (eq window-system nil)
  (set-frame-font "MigMix 1M" 12))

(setq custom-file (expand-file-name "~/.emacs.d/customize.el"))
;;(if (file-exists-p (expand-file-name custom-file))
;;    (load (expand-file-name custom-file)t nil nil)
;;    )

;;; init.el ends here
