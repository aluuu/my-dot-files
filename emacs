(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(add-to-list 'load-path "~/.emacs.d/elpa/")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(make-directory "/tmp/emacs/" t)
(package-initialize)

(defun aluuu/check-package (package)
  (when (not (require package nil 'noerror))
    (package-install package)))

(defun aluuu/check-packages (packages)
  (mapcar #'aluuu/check-package packages))

(defun aluuu/mode-for-hooks (mode hooks)
  (mapcar (lambda (hook)
            (add-hook hook mode))
          hooks))

(defun aluuu/untabify ()
  (interactive)
  (if (not indent-tabs-mode) (untabify (point-min) (point-max))))

(aluuu/check-packages
 (list
  'uniquify
  'multi-web-mode
  'paredit
  'clojure-mode
  'haskell-mode
  'color-theme
  'color-theme-sanityinc-tomorrow
  'yaml-mode
  'smex
  'js2-mode
  'epa))

(aluuu/mode-for-hooks
 #'enable-paredit-mode
 '(emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook
   clojure-mode-hook))

(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "guerry")
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-time-mode 1)
(global-whitespace-mode)
(paredit-mode 1)
(multi-web-global-mode 1)
(epa-file-enable)

(ido-mode 1)
(global-set-key (kbd "C-s-SPC") 'aluuu/mode-line-in-header)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "M-x") 'smex)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "/tmp/emacs\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/tmp/emacs"))))
 '(before-save-hook (quote (delete-trailing-whitespace aluuu/untabify)))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(display-time-default-load-average nil)
 '(display-time-format "%H:%M %d.%m.%Y")
 '(display-time-mode t)
 '(haskell-font-lock-symbols (quote unicode))
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-font-lock)))
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "sbcl")
 '(mweb-default-major-mode (quote html-mode))
 '(mweb-filename-extensions (quote ("php" "htm" "html" "ctp" "phtml" "php4" "php5")))
 '(mweb-tags (quote ((tpl-mode "{%|{{" "}}|%}") (php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>") (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>") (css-mode "<style +type=\"text/css\"[^>]*>" "</style>"))))
 '(show-paren-mode t)
 '(slime-repl-history-size 1000)
 '(tool-bar-mode nil)
 '(whitespace-style (quote (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
'(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 97 :width normal :foundry "paratype" :family "PT Mono"))))
 '(mode-line ((t (:background "black" :foreground "#f6f3e8"))))
 '(whitespace-newline ((t (:foreground "dim gray" :weight normal))))
 '(whitespace-space ((t (:foreground "gray25")))))
