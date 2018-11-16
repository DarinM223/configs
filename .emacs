;
; NOTE: DON'T FORGET TO GO INTO ~/.emacs.d/elpa/parinfer
; AND DELETE ALL *.elc FILES AND RESTART EMACS
; DUE TO ISSUE https://github.com/DogLooksGood/parinfer-mode/issues/46
;

(setq inhibit-startup-screen t)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(require 'package)

(setq package-list '(auto-complete evil use-package parinfer racket-mode slime ac-slime solarized-theme))

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

(define-key evil-normal-state-map (kbd "M-.")
  `(menu-item "" evil-repeat-pop :filter
              ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
  (let ((base03    "#002b36")
        (base02    "#073642")
        (base01    "#586e75")
        (base00    "#657b83")
        (base0     "#839496")
        (base1     "#93a1a1")
        (base2     "#eee8d5")
        (base3     "#fdf6e3")
        (yellow    "#b58900")
        (orange    "#cb4b16")
        (red       "#dc322f")
        (magenta   "#d33682")
        (violet    "#6c71c4")
        (blue      "#268bd2")
        (cyan      "#2aa198")
        (green     "#859900"))
     (custom-set-faces
         `(agda2-highlight-keyword-face ((t (:foreground ,orange))))
         `(agda2-highlight-string-face ((t (:foreground ,magenta))))
         `(agda2-highlight-number-face ((t (:foreground ,violet))))
         `(agda2-highlight-symbol-face ((((background ,base3)) (:foreground ,base01))))
         `(agda2-highlight-primitive-type-face ((t (:foreground ,blue))))
         `(agda2-highlight-bound-variable-face ((t nil)))
         `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
         `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
         `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
         `(agda2-highlight-field-face ((t (:foreground ,red))))
         `(agda2-highlight-function-face ((t (:foreground ,blue))))
         `(agda2-highlight-module-face ((t (:foreground ,violet))))
         `(agda2-highlight-postulate-face ((t (:foreground ,blue))))
         `(agda2-highlight-primitive-face ((t (:foreground ,blue))))
         `(agda2-highlight-record-face ((t (:foreground ,blue))))
         `(agda2-highlight-dotted-face ((t nil)))
         `(agda2-highlight-operator-face ((t nil)))
         `(agda2-highlight-error-face ((t (:foreground ,red :underline t))))
         `(agda2-highlight-unsolved-meta-face ((t (:background ,base03 :foreground ,yellow))))
         `(agda2-highlight-unsolved-constraint-face ((t (:background ,base03 :foreground ,yellow))))
         `(agda2-highlight-termination-problem-face ((t (:background ,orange :foreground ,base03))))
         `(agda2-highlight-incomplete-pattern-face ((t (:background ,orange :foreground ,base03))))
         `(agda2-highlight-typechecks-face ((t (:background ,cyan :foreground ,base03)))))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
