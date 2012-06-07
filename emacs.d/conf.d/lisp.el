(require 'slime-autoloads)
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 1)

(defun my-lisp-mode-hook ()
  (hs-minor-mode 1)
  ;; (paredit-mode 1)
  (highlight-symbol-mode)
  (slime-mode)
  (setq highlight-symbol-face '((:underline t)))
  (custom-set-faces '(highlight-symbol-face ((((class color) (background dark)) (:background "yellow"))))))

(eval-after-load "slime"
  `(progn
     (global-set-key "\C-z" 'slime-selector)
     (global-set-key "\C-x \C-e" 'slime-eval-last-expression)
     (global-set-key "\C-c \C-r" 'slime-eval-region)
     (global-set-key "\C-c \C-c" 'slime-compile-file)
     (slime-setup '(slime-fancy slime-indentation slime-tramp slime-asdf slime-sprof))
     (custom-set-variables
      '(slime-complete-symbol*-fancy t)
      '(slime-net-coding-system 'utf-8-unix)
      '(slime-lisp-implementations '((sbcl ("/home/aluuu/bin/sbcl") :coding-system utf-8-unix))))))

(add-hook 'lisp-mode-hook #'my-lisp-mode-hook 1)