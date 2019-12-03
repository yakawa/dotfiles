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

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;;;;;;;;;;;
;; 環境設定
;;;;;;;;;;;
;; 総合環境
;; Backup 設定
(add-to-list 'backup-directory-alist (cons (expand-file-name "~/.emacs.d/backups") "."))
(setq auto-save-file-name-transforms `((".*", (expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-timeout 15)
(setq auto-save-interval 60)

;; 表示環境
(prefer-coding-system 'utf-8) ;; UTF-8をデフォルトに設定
(menu-bar-mode -1) ;; メニューバーを表示しない
(tool-bar-mode -1) ;; ツールバーを表示しない
(column-number-mode t) ;; カラム番号
(size-indication-mode t) ;; ファイルサイズの表示
(setq display-time-day-and-date t) ;; 時計の表示モード
(display-time-mode t) ;; 時計の表示
(setq frame-title-format "%f") ;; Frameのファイル名表示
(global-linum-mode -1) ;; 行番号の表示
;(global-display-line-numbers-mode -1)
(show-paren-mode t) ;; 対応するカッコの表示
(transient-mark-mode t) ;; リージョンに色を付ける
(setq-default tab-width 2) ;; Tab を 2 スペース
(setq-default default-tab-width 2) ;; Tab を 2 スペース
(setq inhibits-startup-screen t) ;; Startup 画面を表示しない
(setq use-dialog-box nil) ;; Dialog の使用禁止
(defalias 'message-box 'message) ;; message boxの代わりに message
;; 現在の関数の表示
(setq which-function-mode t)
(setq mode-line-format (delete (assoc 'which-func-mode
																			mode-line-format) mode-line-format)
			which-func-header-line-format '(which-func-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
	(when which-func-mode
		(setq mode-line-format (delete (aasoc 'which-func-mode
																					mode-line-format) mode-line-format)
					header-line-format which-func-header-line-format)))
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "#939595"))
    (t ()))
  "*Face used by hl-line.")
;; (global-hl-line-mode t) ;; 現在行のハイライト
;; バッファー名をわかりやすくする
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
(setq initial-scratch-message nil) ;; Scratch を表示しない

;; 環境保存
(savehist-mode t)
(setq history-length 1500)
;; カーソル位置の保存
(setq-default save-place t)
(require 'saveplace)
;; ブックマークの保存
(setq bookmark-save-flag t)
(progn (setq bookmark-sort-flag nil)
			 (defun bookmark-arrange-latest-top ()
				 (let ((latest (bookmark-get-bookmark bookmark)))
					 (setq bookmark-alias (cons latest (delq latest bookmark-alist))))
				 (bookmark-save))
			 (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))

;; 操作系
(defalias 'yes-or-no-p 'y-or-n-p) ;; y or n に変更
(setq enable-recursive-minibuffers t) ;; mini buffer の再帰呼び出し
(setq echo-keystrokes 0.1) ;; キーのエコーバックを早くする
;; C-x C-f の改善
(define-key minibuffer-local-filename-completion-map (kbd "?") nil)
(setq ffap-pass-wildcards-to-dired t)
(ffap-bindings)
(setq kill-whole-line t) ;; C-k で行末の改行を削除

;; 動作系
(add-hook 'after-save-hook
					'executable-make-buffer-file-executable-if-script-p) ;; #!で始まれば +x する
(setq gc-cons-threshold (* 100 gc-cons-threshold)) ;; GC の回数を減らす
(setq message-log-max 10000) ;; ログを増やす
(setq large-file-warning-threshold (* 50 1024 1024)) ;; 大きいサイズのファイルを開くときに警告

(defadvice abort-recursive-edit (before minibuffer-save activate)
	(when (eq (selected-window) (active-minibuffer-window))
		(add-to-history minibuffer-history-variable (minibuffer-contents))))
(global-auto-revert-mode t) ;; バッファの自動再読み込み
(setq completion-ignore-case t) ;; 補完で大文字小文字を区別しない
(setq case-fold-search t) ;; 検索で大文字小文字を区別
(setq iserach-case-fold-search nil) ;; i-Search で大文字小文字を区別
(setq read-buffer-completion-ignore-case t) ;; Mini Buffer Search で大文字小文字を区別しない
(setq read-file-name-completion-ignore-case t) ;; ファイルを開くときに大文字小文字を区別しない
(setq case-replace t) ;; 置換の際に大文字小文字を区別
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; 保存するときに余分な空白を削除
(setq-default indent-tabs-mode nil) ;; スペースをタブに変換しない
(setq vc-follow-symlinks t) ;; Symlink をめぐる
(setq create-lockfiles nil) ;; Lock file を作らない
(setq vc-handled-backends ()) ;; Disable VC-mode


;;;;;;;;;;;;;;;;;;;
;; 外部パッケージ系
;;;;;;;;;;;;;;;;;;;
;; anzu (マッチ数を表示)
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode t)
	:config
	(custom-set-variables
	 '(anzu-mode-lighter "")
	 '(anzu-deactivate-region t)
	 '(anzu-search-threshold 1000)))

;; auto-async byte compile
;(use-package auto-async-byte-compile
;  :config
;  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;; auto-complete
(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (global-auto-complete-mode t))
(defun emacs-lisp-ac-setup ()
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-symbols)))
(with-eval-after-load 'auto-complete-config
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy t)
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-ac-setup))

;; bookmark
(use-package bm
  :ensure t
  :bind (("S-M-SPC" . bm-toggle)
         ("M-]" . bm-previous)
         ("M-]" . bm-next)
         )
  :init
  (setq-default bm-buffer-presistence nil)
  (setq bm-restore-repository-on-load t)
  :config
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil (bm-buffer-save-all) (bm-repository-save)))
  )

(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode t))

;; col-highlight
;(use-package col-highlight
;  :ensure
;  :config
;  (taggle-highlight-column-when-idele t)
;  (col-highlight-set-interval 10))

(use-package color-moccur
  :ensure t
  :init
  (setq moccur-split-word t))

;(use-package moccur-edit
;  :config
;  (setq moccur-split-word t))

(use-package goto-chg
  :ensure t
  :bind (("<F8>" . goto-last-change)
         ("S-<F8>" . goto-last-change-reverse)
         )
  )

(use-package helm
  :ensure t
  :bind (
         ("M-x" . helm-M-x)
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
         ("TAB" . helm-execute-persistent-action)
         )
  :init
  (helm-mode t)
  (when (executable-find "curl") (setq helm-google-suggest-use-curl-p t))
  :config
  (setq helm-split-window-in-side-p t)
  (setq helm-move-to-line-cycle-in-source t)
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-scroll-amount t)
  (setq helm-ff-fine-name-history-use-recentf t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-delete-minibuffer-contents-from-point t)
  (setq helm-display-function #'display-buffer)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))
  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate) ad-do-it))
  )

(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.04)
  (key-chord-mode t))

(use-package minor-mode-hack
  :ensure t
  )

(use-package multicolumn
  :ensure t
  :bind(
        :map multicolumn-map
             ("C-x 4 4" . multicolumn-delete-other-windows-and-split-with-follow-mode)
        )
  :config
  (setq multicolumn-min-width 100))

(use-package neotree
  :ensure t
  )

(use-package open-junk-file
  :ensure t
  :config
  (setq open-junk-file-format (expand-file-name "~/.emacs.d/junk/%Y/%m%d_%H%M%S.md")))

(use-package pandoc
  :ensure t
  :config
  (pandoc-turn-on-advice-eww))

(use-package recentf-ext
  :config
  (setq recentf-max-saved-items 3000))

(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package undohist
  :ensure t
  :config
  (setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
  (undohist-initialize))

(use-package viewer
  :ensure t
  :config
  (viewer-stay-in-setup)
  (viewer-change-modeline-color-setup)
  (viewer-aggressive-setup t)
  (setq viewer-modeline-color-unwritable "tomato")
  (setq viewer-modeline-color-view "orange")
  (setq view-read-only t)
  (setq view-mode-by-default-regexp "\\.log$")
  :bind(
        :map view-mode-map
             ("N" . view-search-last-regxp-backward)
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
             ("m" . bm-toggle)
             ("[" . bm-previous)
             ("]" . bm-next)
        )
  )

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?$" . web-mode)))

;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

;;;;;;;;;;
;; Python
;;;;;;;;;;
(use-package python
  :ensure t
  )

;;;;;;;;;
;; golang
;;;;;;;;;
(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (set-face-attribute 'eldoc-highlight-function-argument nil))

(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin"))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-hook 'go-mode-hook 'flycheck-mode)
(defun go-fmt-hook()
  "Go-fmt"
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4))
(add-hook 'go-mode-hook 'go-fmt-hook)
(eval-after-load "go-mode"
  '(progn
     (use-package go-autocomplete)))

;;;;;;;;;;;
;; 関数定義
;;;;;;;;;;;
;; Shell から PATH を読む
;; https://qiita.com/catatsuy/items/3dda714f4c60c435bb25
(defun set-exec-path-from-shell-PATH ()
	"Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
	(interactive)
	(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
		(setenv "PATH" path-from-shell)
		(setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; Copy and paste
(defun paste-to-clipboard (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Message*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-clipboard)

(defun web-mode-hook2()
  "Hook for web-mode"
  (setq web-mode-html-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-css-offset 2)
  (setq web-mode-script-offset 2)
  (setq web-mode-javascript-offset 2)
  (setq web-mode-java-offset 2)
  (setq web-mode-asp-offset 2)
  (setq web-mode-tag-auto-close-style 2))
(add-hook 'web-mode-hook 'web-mode-hook2)


;;;;;;;;;;;;;;;;;;;
;; Key Bind
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-h") 'delete-backwaed-char)
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-o") 'next-line)
(global-set-key (kbd "C-x C-j") 'open-junk-file)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Added by use-package
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(package-selected-packages
   (quote
    (go-autocomplete web-mode viewer undohist recentf-ext pandoc open-junk-file neotree multicolumn helm-config auto-complete auto-async-byte-compile anzu use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
