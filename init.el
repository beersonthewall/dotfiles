;; boostrapping for straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; tell straight symbol for use-package
(straight-use-package 'use-package)

;; misc
(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.emacs_saves"))
      delete-old-versions t
      kept-new-versions 6
      version-control t)

(use-package yasnippet
  :straight t
  :demand t
  :config (yas-global-mode))

(use-package zeno-theme
  :straight t)
(load-theme 'zeno)

(use-package projectile
  :straight t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/Code"))
  :custom
  ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :straight t
  :config (counsel-projectile-mode))

(use-package ivy
  :straight t
  :bind
  (("C-s" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package swiper
  :straight t)

(use-package counsel
  :straight t)

(use-package company
  :straight t)

(use-package flycheck
  :straight t)

(use-package magit
  :straight t)

(use-package ace-window
  :straight t
  :bind
  (("M-o" . ace-window)))

(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package paredit
  :straight t)

(use-package rainbow-delimiters
  :straight t)

;; language *-mode and other programming language packages

(use-package go-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package zig-mode
  :straight t)

(use-package rustic
  :straight t)

(use-package ruby-mode
  :straight t)

(use-package clojure-mode
  :straight t)

(use-package terraform-mode
  :straight t)

(use-package markdown-mode
  :straight t)

(use-package elpy
  :straight t
  :init
  (elpy-enable))

;; language server protocol (lsp)

(use-package lsp-mode
  :straight t
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :hook ((zig-mode . lsp)
	 (terraform-mode . lsp)
	 (rust-mode . lsp)
	 (ruby-mode . lsp)
	 (go-mode . lsp)
	 (clojure-mode . lsp)
	 (c-mode . lsp)
	 (elisp-mode . lsp)
	 (yaml-mode . lsp)))

(use-package lsp-ui
  :straight t)

(use-package lsp-ivy
  :straight t)

;; rss feeds

(use-package elfeed
  :straight t
  :bind (("C-x w" . elfeed))
  :init (setq elfeed-feeds '("http://nullprogram.com/feed/"
			     "https://cate.blog/feed"
			     "https://danluu.com/atom.xml"
			     "https://smallcultfollowing.com/babysteps/atom.xml"
			     "https://eli.thegreenplace.net/feeds/all.atom.xml"
			     "https://jvns.ca/atom.xml"
			     "https://archlinux.org/feeds/news/")))

;; terminal emulators

(use-package vterm
  :commands vterm vterm-mode
  :straight t
  :init (setq vterm-kill-buffer-on-exit t))

(use-package multi-vterm
  :straight t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("10fef6d73ae453f39c9f325915386d41894870b72926e8e9b0c39d030447b703"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
