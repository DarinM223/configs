(setq inhibit-startup-screen t)

(require 'package)

(setq package-list '(evil))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p package-list)
  (unless (package-installed-p p)
    (package-install p)))

(setq evil-want-C-u-scroll t)

(require 'evil)
(evil-mode 1)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
