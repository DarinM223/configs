;
; NOTE: DON'T FORGET TO GO INTO ~/.emacs.d/elpa/parinfer
; AND DELETE ALL *.elc FILES AND RESTART EMACS
; DUE TO ISSUE https://github.com/DogLooksGood/parinfer-mode/issues/46
;

(setq inhibit-startup-screen t)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-cl-indent))

(require 'package)

(setq package-list '(auto-complete evil elscreen use-package parinfer racket-mode slime ac-slime gruvbox-theme solarized-theme linum-relative))

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

(require 'evil)
(evil-mode 1)
(load "elscreen" "ElScreen" t)
(elscreen-start)
(linum-relative-global-mode)

(define-key evil-normal-state-map (kbd "M-.")
  `(menu-item "" evil-repeat-pop :filter
              ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))

(define-key evil-normal-state-map (kbd "C-t") 'elscreen-create)   ; create tab with `C-t`
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill)   ; kill tab with `C-w x`
(define-key evil-normal-state-map "gT" 'elscreen-previous)        ; previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next)            ; next tab


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruvbox-dark-soft)))
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(nil nil t))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-bound-variable-face ((t nil)))
 '(agda2-highlight-coinductive-constructor-face ((t (:foreground "#b58900"))))
 '(agda2-highlight-datatype-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-dotted-face ((t nil)))
 '(agda2-highlight-error-face ((t (:foreground "#dc322f" :underline t))))
 '(agda2-highlight-field-face ((t (:foreground "#dc322f"))))
 '(agda2-highlight-function-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-incomplete-pattern-face ((t (:background "#cb4b16" :foreground "#002b36"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#859900"))))
 '(agda2-highlight-keyword-face ((t (:foreground "#cb4b16"))))
 '(agda2-highlight-module-face ((t (:foreground "#6c71c4"))))
 '(agda2-highlight-number-face ((t (:foreground "#6c71c4"))))
 '(agda2-highlight-operator-face ((t nil)))
 '(agda2-highlight-postulate-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-primitive-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-record-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-string-face ((t (:foreground "#d33682"))))
 '(agda2-highlight-symbol-face ((((background "#fdf6e3")) (:foreground "#586e75"))))
 '(agda2-highlight-termination-problem-face ((t (:background "#cb4b16" :foreground "#002b36"))))
 '(agda2-highlight-typechecks-face ((t (:background "#2aa198" :foreground "#002b36"))))
 '(agda2-highlight-unsolved-constraint-face ((t (:background "#002b36" :foreground "#b58900"))))
 '(agda2-highlight-unsolved-meta-face ((t (:background "#002b36" :foreground "#b58900")))))
