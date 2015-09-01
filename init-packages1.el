;;;
;;; Setup package manager and packages
;;;
;;; Auto loading adaped from:
;;;  - http://stackoverflow.com/a/10093312
;;;  - http://stackoverflow.com/a/10095853
;;;  - http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
;;;

; list of packages that will auto install:
(defvar my-package-list
      '(ace-jump-mode
        ag
        batch-mode
        dash
        edit-server
        epl
        etags-select
        f
        flx
        flx-ido
        flycheck
        flycheck-google-cpplint
        google-c-style
        grin
        haskell-mode
        hungry-delete
        magit
        markdown-mode
        php-mode
        pkg-info
        protobuf-mode
        s
        solarized-theme
        writeroom-mode))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'cl)

; check if all packages are installed:
(defun packages-installed-p ()
  (loop for p in my-package-list
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; install any missing packages:
(unless (packages-installed-p)
  ; check for new packages:
  (message "%s" "Emacs is now refreshing it's package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages:
  (dolist (p my-package-list)
    (when (not (package-installed-p p))
      (package-install p))))

;;; Misc settings for packages:

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-cppcheck
                                'c/c++-googlelint)))
(custom-set-variables
 '(flycheck-c/c++-googlelint-executable "cpplint"))

;; Should be loaded by package-initialize, but isn't.
(require 'google-c-style)

;; Explicitly set path to git on windows (seems to speed up magit a bit):
(if (eq system-type 'windows-nt)
    (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"))
;; Disable startup instructions:
(setq magit-last-seen-setup-instructions "1.4.0")

;; ace-jump-mode (doesn't seem to auto load by itself)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
