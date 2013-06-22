(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(load-file "/home/aluuu/.emacs.d/conf.d/base.el")
(load-file "/home/aluuu/.emacs.d/conf.d/appearance.el")
(load-file "/home/aluuu/.emacs.d/conf.d/auto-complete.el")
(load-file "/home/aluuu/.emacs.d/conf.d/lisp.el")
(load-file "/home/aluuu/.emacs.d/conf.d/python.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(tab-width 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#1d1f21" :foreground "#c5c8c6" :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Anonymous Pro")))))
(put 'upcase-region 'disabled nil)
