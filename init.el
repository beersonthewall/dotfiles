(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
	(with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package ace-window
  :ensure t
  :bind
  (("M-o" . ace-window)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package magit
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package ivy
  :ensure t
  :diminish
  :bind
  (("C-s" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/Code"))
  :custom
  ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package lsp-ui
  :ensure t)

(use-package lsp-ivy
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t)

(use-package tide
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package rebecca-theme
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package vterm
  :commands vterm vterm-mode
  :ensure t
  :init (setq vterm-kill-buffer-on-exit t))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config (setq which-key-idle-delay 0.5))

(use-package rustic
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package janet-mode
  :ensure t)

(use-package elfeed
  :ensure t
  :bind (("C-x w" . elfeed))
  :init (setq elfeed-feeds '("http://nullprogram.com/feed/"
			     "https://cate.blog/feed"
			     "https://danluu.com/atom.xml"
			     "https://smallcultfollowing.com/babysteps/atom.xml"
			     "https://eli.thegreenplace.net/feeds/all.atom.xml"
			     "https://jvns.ca/atom.xml")))

(use-package clojure-mode
  :ensure t)

(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (cider-mode . enable-paredit-mode)
   (cider-repl-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (clojure-mode . enable-paredit-mode)
   (clojurescript-mode . enable-paredit-mode)))

(use-package rainbow-delimiters
  :ensure t)

(use-package tagedit
  :ensure t)

(use-package cider
  :ensure t)

(use-package dap-mode
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package web-mode  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
	 ("\\.json\\'" . web-mode))
  :commands web-mode
  :config
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'"))))

(use-package lsp-mode
  :config
  (setq lsp-yaml-schema-store-enable t)
  (setq lsp-completion-enable t)
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :ensure t
  :hook ((web-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
	 (ruby-mode . lsp-deferred)
	 (go-mode . lsp-deferred)
	 (clojure-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (elisp-mode . lsp-deferred)
	 (yaml-mode . lsp-deferred)))

;; UI changes
(global-display-line-numbers-mode 1)
(load-theme #'rebecca t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)
(tooltip-mode -1)
(scroll-bar-mode -1)

(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.emacs_saves"))
      delete-old-versions t
      kept-new-versions 6
      version-control t)

(server-start)

