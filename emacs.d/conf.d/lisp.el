(require 'slime-autoloads)
(slime-setup '(slime-fancy))
(eval-after-load "slime"
  `(progn
     (slime-setup '(slime-repl))
     (custom-set-variables
      '(slime-complete-symbol*-fancy t)
      '(slime-net-coding-system 'utf-8-unix)
      '(slime-startup-animation nil)
      '(slime-lisp-implementations '((sbcl ("/usr/bin/sbcl")))))))

(global-set-key "\C-z" 'slime-selector)
(slime-setup '(slime-repl))
