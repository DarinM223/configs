;
; NOTE: DON'T FORGET TO GO INTO ~/.emacs.d/elpa/parinfer
; AND DELETE ALL *.elc FILES AND RESTART EMACS
; DUE TO ISSUE https://github.com/DogLooksGood/parinfer-mode/issues/46
;

(setq inhibit-startup-screen t)
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-indentation))
(setq lisp-indent-function 'common-lisp-indent-function)
(setq common-lisp-style-default "sbcl")

(require 'package)

(setq package-list '(auto-complete
                     evil
                     elscreen
                     use-package
                     parinfer
                     racket-mode
                     slime
                     ac-slime
                     gruvbox-theme
                     solarized-theme
                     linum-relative))

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

; NOTE: Run (ql:quickload "clhs") first and follow the instructions.
(load "~/quicklisp/clhs-use-local.el" t)

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            smart-tab))    ; Allows changing indentation of regions with tab.

    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)
    (add-hook 'racket-mode-hook #'parinfer-mode)))

(setq-default indent-tabs-mode nil)

(setq evil-want-C-u-scroll t)
(setq make-backup-files nil)

(ac-config-default)
(global-auto-complete-mode t)

(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(slime-setup '(slime-fancy))

(require 'evil)
(evil-mode 1)
(load "elscreen" "ElScreen" t)
(elscreen-start)
(linum-relative-global-mode)

; Fix for Evil overriding Slime's M-. keybinding
(define-key evil-normal-state-map (kbd "M-.")
  `(menu-item "" evil-repeat-pop :filter
              ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-medium))
 '(custom-safe-themes
   '("7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default))
 '(package-selected-packages
   '(use-package solarized-theme racket-mode py-autopep8 parinfer neotree monokai-theme lispy linum-relative gruvbox-theme evil elscreen elpy dirtree async ac-slime)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
