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
      '(flx
        flx-ido
        ))

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

;; flycheck init
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
