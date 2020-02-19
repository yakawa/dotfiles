;;; init.el --- Configuration for emacs
;; -*- coding: utf-8 -*-
;;; Commentary:
;; (setq debug-on-error t)

;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

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
(setq inhibit-startup-message t) ;; Start-up を表示しない
(setq inhibit-startup-echo-area-message t) ;; Start-up を表示しない
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

(when (or (eq window-system 'ns) (eq window-system 'mac))
  (set-frame-parameter nil 'fullscreen 'maximized))

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
  (("C-x g" . magit-status))
  :config
  (defadvice magit-status (around magit-fullscreent activate)
    "Magit-status always in full screen."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  )

(use-package git-gutter
	:ensure t
	:config
	(global-git-gutter-mode t)
  (defun git-gutter:toggle-popup-hunk ()
    "Toggle git-gutter hunk window."
    (interactive)
    (if (windows-live-p (git-gutter:popup-buffer-window))
        (delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk)))
  )

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
  (setq lsp-metals-treeview-show-when-views-received t))

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
  (setq default-frame-alist
        (append (list
                 '(font . "MigMix 1M-14"))
                default-frame-alist))
  )

(use-package hydra
  :ensure t)

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   F flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("F" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)

(bind-key
 [f7]
 (defhydra hydra-flycheck
   (:pre (flycheck-list-errors)
         :post (quit-windows-on "*Flycheck errors*")
         :hint nil)
   "Errors"
   ("f" flycheck-error-list-set-filter "Filter")
   ("j" flycheck-next-error "Next")
   ("k" flycheck-previous-error "Previous")
   ("gg" flycheck-first-error "First")
   ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
   ("q" nil)))

(bind-key
 (kbd "C-S-g")
 (defhydra hydra-git-gutter (:color ref :hint nil)
    "
_m_agit  _b_lame  _d_ispatch  _t_imemachine  |  hunk: _p_revious  _n_ext  _s_tage  _r_evert  pop_u_p  _SPC_:toggle"
    ("m" magit-status :exit t)
    ("b" magit-blame :exit t)
    ("t" git-timemachine :exit t)
    ("d" magit-dispatch :exit t)
    ("p" git-gutter:previous-hunk)
    ("n" git-gutter:next-hunk)
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("u" git-gutter:popup-hunk)
    ("SPC" git-gutter:toggle-popup-hunk)))

(bind-key
 [f10]
 (defhydra hydra-browse (:hint nil :exit t)
  "
 ^Shop^                           ^Repos^          ^GH^           ^Favorite^      ^Others^       ^Applications
 ^^^^^^---------------------------------------------------------------------------------------------------
 _a_: Amazon      _b_: BicCamera  _w_: weather    _c_: Browse-url
 _r_: Rakuten     _t_: Twitter    _j_: jma
 _y_: Yodobashi   _g_: github     _q_: Qiita
 _0_: Gist        _e_: Weather Tokyo
"
   ("a" (browse-url "https://www.amazon.co.jp/"))
   ("r" (browse-url "https://www.rakuten.co.jp/"))
   ("y" (browse-url "https://www.yodobashi.com/"))
   ("b" (browse-url "https://www.bivvamera.com/"))
   ("t" (browse-url "https://twitter.com"))
   ("g" (browse-url "https://github.com/yakawa"))
   ("0" (browse-url "https://gist.github.com/yakawa"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("j" (browse-url "https://www.jma.go.jp/"))
   ("q" (browse-url "https://qiita.com/yakawa"))
   ("e" (eww "https://www.jma.go.jp/jp/week/319.html"))
   ("/" kill-other-buffers)
   ("," hydra-window/body)
   ("." hydra-work1/body)
   ("c" browse-url-at-point)
   ("<f10>" nil)))

(bind-key
  [f3]
  (defhydra hydra-lsp (:exit t :hint nil)
    "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace)))

(add-hook 'view-mode-hook
      (lambda ()
        (define-key view-mode-map "i" 'View-exit)
        (define-key view-mode-map ":" 'View-exit)
        (define-key view-mode-map "g" 'beginning-of-buffer)
        (define-key view-mode-map "G" 'end-of-buffer)
        (define-key view-mode-map "e" 'end-of-line)
        (define-key view-mode-map "a" 'beginning-of-line)
        (define-key view-mode-map "b" 'scroll-down-command)
        (define-key view-mode-map "D" 'my/view-kill-whole-line)
        (define-key view-mode-map "u" 'my/view-undo)
        (define-key view-mode-map "X" 'my/view-del-char)
        (define-key view-mode-map "w" 'my/view-forward-word+1)
        (define-key view-mode-map "W" 'backward-word)
        (define-key view-mode-map "s" 'swiper-for-region-or-swiper)
        (define-key view-mode-map "t" 'git-timemachine)
        (define-key view-mode-map "v" 'vc-diff)
        (define-key view-mode-map "[" 'forward-list)
        (define-key view-mode-map "]" 'backward-list)
        (define-key view-mode-map "l" 'goto-line)
        (define-key view-mode-map ";" 'recenter-top-bottom)
        (define-key view-mode-map "m" 'magit-status)
        (define-key view-mode-map "B" 'magit-blame)
        (define-key view-mode-map "j" 'git-gutter:next-hunk)
        (define-key view-mode-map "k" 'git-gutter:previous-hunk)
        (define-key view-mode-map "r" 'git-gutter:revert-hunk)
        (define-key view-mode-map "S" 'git-gutter:stage-hunk)
        (define-key view-mode-map "p" 'git-gutter:popup-hunk)
        (define-key view-mode-map "," 'hydra-window/body)
        (define-key view-mode-map "_" 'delete-other-windows)
        (define-key view-mode-map "." 'hydra-view-mode/body)))


;; Function to edit in view-mode
(defun my/view-forward-word+1 ()
  "Forward word+1 in view mode."
  (interactive)
  (forward-word)
  (forward-char))
(defun my/view-kill-whole-line ()
  "Kill whole line in view mode."
  (interactive)
  (view-mode 0)
  (kill-whole-line)
  (save-buffer)
  (view-mode 1)
  (message "kill-whole-line and save!"))
(defun my/view-del-char ()
  "Delete character in view mode."
  (interactive)
  (view-mode 0)
  (delete-char 1)
  (save-buffer)
  (view-mode 1)
  (message "delete-char"))
(defun my/view-undo ()
  "Undo in view mode."
  (interactive)
  (view-mode 0)
  (undo)
  (save-buffer)
  (view-mode 1)
  (message "undo and save!"))


;; hydra-view-mode
(defhydra hydra-view-mode (:hint nil :color pink)
  "
_SPC_: next page   _a_: top of line  _u_: view undo      _m_: magit-status  _j_: gg:next-hunk   _s_: swiper
  _b_: prev page   _e_: end of line  _w_: forward word   _B_: magit-blame   _k_: gg:prev-hunk   _d_: dired-jump
  _g_: page top    _l_: goto line    _W_: backward word  _t_: timemachine   _p_: gg:popup-hunk  _i_: view exit
  _G_: page end    _D_: delete line  _[_: forward pair   _v_: vc-diff       _S_: gg:stage-hunk  _q_: view quit
  _;_: top-bottom  _X_: delete char  _]_: backward pair  _h_: github        _r_: gg:revert-hun  _._: close
"
  ;; Move page
  ("SPC" scroll-up-command)
  ("b" scroll-down-command)
  ("g" beginning-of-buffer)
  ("G" end-of-buffer)
  ;; Move line
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("w" my/view-forward-word+1)
  ("W" backward-word)
  ("D" my/view-kill-whole-line)
  ("X" my/view-del-char)
  ("u" my/view-undo)
  ;; Misc
  ("i" View-exit :exit t)
  ("q" View-quit :exit t)
  (":" View-exit :exit t)
  ("[" forward-list)
  ("]" backward-lis)
  ("l" goto-line)
  ;; git
  ("v" vc-diff)
  ("m" magit-status :exit t)
  ("B" magit-blame :exit t)
  ("t" git-timemachine :exit t)
  ("h" my/github)
  ;; gitgutter
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("p" git-gutter:popup-hunk)
  ("S" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  (";" recenter-top-bottom)
  ;; Others
  ("d" dired-jump :exit t)
  ("_" delete-other-windows :exit t)
  ("s" swiper-for-region-or-swiper)
  ("," hydra-window/body :exit t)
  ("." nil :color blue))

(require 'open-godoc)
(defun go-internal-toggle-to-test-file ()
  "Open Test File."
  (let ((current-file (buffer-file-name))
        (tmp-file (buffer-file-name)))
    (cond ((string-match "_test.go$" current-file)
           (setq tmp-file (replace-regexp-in-string "_test.go$" ".go" tmp-file)))
          ((string-match ".go$" current-file)
           (setq tmp-file (replace-regexp-in-string ".go$" "_test.go" tmp-file))))
    (unless (eq current-file tmp-file)
      (find-file tmp-file)))
  )

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(defun go-open-with-test-file ()
  "Open Test file."
  (interactive)
  (other-window-or-split)
  (go-internal-toggle-to-test-file))

(defvar my/helm-go-source
  '((name . "Helm Go")
    (candidates . (lambda ()
                    (cons "builtin" (go-packages))))
    (action . (("Show document" . godoc)
               ("Import package" . my/helm-go-import-add)))))
(defun my/helm-go-import-add (candidate)
  "Helm Go import Add."
  (dolist (package (helm-marked-candidates))
    (go-import-add current-prefix-arg package)))

(defun my/helm-go ()
  (interactive)
  (helm :source '(my/helm-go-source) :buffer "*helm go*"))

(use-package gotest
  :ensure t
  :config
  (setq go-test-verbose t)
  :bind
  (
   :map go-mode-map
        (
         ("C-c C-t" . go-test-current-file)
         ("C-c t" . go-test-current-test)
         )
        )
  )

(use-package smartrep
  :ensure t)
(eval-after-load "flycheck"
  '(progn
     (smartrep-define-key
         go-mode-map "C-c" '(("C-n" . (lambda ()
                                        (flycheck-next-error)))
                             ("C-p" . (lambda ()
                                        (flycheck-previous-error))))))
  )

(use-package helm-bind-key
  :ensure t)

(bind-key
  [f4]
  (defhydra hydra-go-mode (:exit t :hint nil)
    "
  Go-mode^^
-------------------------------------------------------------------------------------
 [_f_] format               [_o_] Open Test File     [_M-r_] restart     [_j_] jump (M-.)
 [_m_] imenu                [_r_] rename             [M-s] session     [_b_] back (M-,)
 [_a_] add import (C-c C-a) [_x_] eXecute            [M-S] shutdown    [_T_] Test this file
 [_d_] delete import        [_D_] Desctibe                               [_t_] Test current
 [_n_] next error           [_p_] Preivious error"
    ("f" go-fmt)
    ("m" lsp-ui-imenu)
    ("a" go-import-add)
    ("d" go-remove-unused-imports)
    ("o" go-open-with-test-file)
    ("r" lsp-rename)
    ("x" lsp-execute-code-action)
    ("D" godef-describe)
    ("j" godef-jump)
    ("b" pop-tag-mark)
    ("T" go-test-current-file)
    ("t" go-test-current-test)
    ("n" flycheck-next-error)
    ("p" flycheck-previous-error)
    ("M-s" lsp-describe-session)
    ("M-r" lsp-workspace-restart)
    ("S" lsp-workspace-shutdown)))


(setq custom-file (expand-file-name "~/.emacs.d/customize.el"))
;;(if (file-exists-p (expand-file-name custom-file))
;;    (load (expand-file-name custom-file)t nil nil)
;;    )

;;; init.el ends here
