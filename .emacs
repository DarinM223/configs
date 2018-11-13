;
; NOTE: DON'T FORGET TO GO INTO ~/.emacs.d/elpa/parinfer
; AND DELETE ALL *.elc FILES AND RESTART EMACS
; DUE TO ISSUE https://github.com/DogLooksGood/parinfer-mode/issues/46
;

(setq inhibit-startup-screen t)

(require 'package)

(setq package-list '(evil use-package parinfer racket-mode))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p package-list)
  (unless (package-installed-p p)
    (package-install p)))

(eval-when-compile (require 'use-package))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil))         ; If you use Evil.

    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)
    (add-hook 'racket-mode-hook #'parinfer-mode)))

(setq evil-want-C-u-scroll t)

(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "M-.")
  `(menu-item "" evil-repeat-pop :filter
              ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

