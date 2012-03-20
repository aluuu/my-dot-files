;; Scheme-related things
(add-hook 'scheme-mode-hook #'lambda-mode 1)
(custom-set-variables
 '(inhibit-startup-screen t)
 '(quack-global-menu-p nil)
 '(quack-programs (quote ("mzscheme" "racke" "racket" "gracket")))
 '(quack-remap-find-file-bindings-p t))
(setq auto-mode-alist (append '(("\\.rkt$" . scheme-mode)
                                ("\\.scm$" . scheme-mode)
                                ("\\.ss$" . scheme-mode))
                              auto-mode-alist))
(autoload 'run-scheme "racket" "Run an inferior Scheme" t)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)
(setq scheme-program-name "racket")