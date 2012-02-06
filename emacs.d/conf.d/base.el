;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic emacs configuration                            ;;
;;   contains tweaks, that doesn't require any packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; backups customization
(setq backup-directory-alist `((".*" . "~/.emacs.tmp"))
      auto-save-file-name-transforms `((".*" "~/.emacs.tmp" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; minimal view
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(setq inhibit-splash-screen t)

;; correct language switching
(set-language-environment "UTF-8")
(set-language-environment-input-method "russian")

;; identation settings
(custom-set-variables
 '(indent-tabs-mode nil))
(add-hook
 'write-file-hooks
 (lambda ()
   ;; automatic untabify all files while saving
   (if (not indent-tabs-mode)
       (untabify (point-min) (point-max)))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; date and time display settings
(setq display-time-24hr-format t)
(setq display-time-string-forms
      (quote
       ((if (and (not display-time-format)
                 display-time-day-and-date)
            (format-time-string "%a %b %e   " now) "  ")
        (format-time-string
         (or display-time-format
             (if display-time-24hr-format "%H:%M %d-%m-%Y")) now))))
(display-time-mode 1)

;; show line on 80th column
(require 'fill-column-indicator)
(setq fci-rule-width 2)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Using ibuffer-mode instead standard buffer (is part of Emacs>=23)
(global-unset-key (kbd "C-x b"))
(global-set-key (kbd "C-x b") 'ibuffer)
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; ido mode (is part of Emacs>=22)
(require 'ido)
(ido-mode)

;; TODO: need to place org-mode to contrib
;; ;; org-mode
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

;; mercurial-mode
(require 'mercurial)

;; lambda mode
(require 'lambda-mode)
(add-hook 'lisp-mode-hook #'lambda-mode 1)
(add-hook 'python-mode-hook #'lambda-mode 1)
(add-hook 'emacs-lisp-mode-hook #'lambda-mode 1)

;; highlighting parentheses
(require 'highlight-parentheses)
(add-hook 'lisp-mode-hook #'highlight-parentheses-mode 1)
(add-hook 'python-mode-hook #'highlight-parentheses-mode 1)
(add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode 1)

(server-start)