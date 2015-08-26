(load "~/.emacs.d/init-packages.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq magit-last-seen-setup-instructions "1.4.0")
;; Highlight long lines
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t);; auto cleanup trailing whitespace:
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
;; Should be loaded by package-initialize, but isn't.
(require 'google-c-style)
;;Disable scroll and menu bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; c mode
(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(setq c-basic-offset 2)
(setq c-default-style
      (quote ((c-mode . "k&r")
              (c++-mode . "k&r")
              (java-mode . "java")
              (awk-mode . "awk")
              (other . "gnu"))))

;; c++-mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(global-set-key (kbd "M-.") 'etags-select-find-tag)
(global-set-key (kbd "M-?") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
  	; NOTE: Set C-tab and C-S-tab to work in all modes (including org-mode):
(defvar my-keys-minor-mode-map (make-keymap))
(define-key my-keys-minor-mode-map (kbd "C-<tab>") 'next-multiframe-window)
(define-key my-keys-minor-mode-map (kbd "C-S-<tab>") 'previous-multiframe-window)
(define-minor-mode my-keys-minor-mode t 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)
(setq-default backup-inhibited t)
;;;
;;; auto inserted stuff (This goes at the very bottom of your init.el file.)
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(indent-tabs-mode nil)
 '(matlab-indent-level 2)
 '(tab-width 2))
