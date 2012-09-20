(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(load-file "/home/aluuu/.emacs.d/conf.d/base.el")
(load-file "/home/aluuu/.emacs.d/conf.d/appearance.el")
(load-file "/home/aluuu/.emacs.d/conf.d/auto-complete.el")
(load-file "/home/aluuu/.emacs.d/conf.d/lisp.el")
(load-file "/home/aluuu/.emacs.d/conf.d/flymake.el")
(load-file "/home/aluuu/.emacs.d/conf.d/python.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(slime-complete-symbol*-fancy t)
 '(slime-lisp-implementations (quote ((sbcl ("/home/aluuu/bin/sbcl") :coding-system utf-8-unix) (cljs ("/home/aluuu/bin/browser-repl") :coding-system utf-8-unix))) t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(tab-width 4))
