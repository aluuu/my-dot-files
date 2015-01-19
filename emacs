(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(add-to-list 'load-path "~/.emacs.d/elpa/")
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

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

(defun aluuu/ocaml-run-tests ()
  ;; TODO: copy user's environment before `make` execution.
  ;;       maybe, this action should be made not only for this function,
  ;;       but for `emacs` process in general
  (let ((closest-makefile (get-closest-pathname)))
    (set (make-local-variable 'compile-command)
         (format "make -f %s test -C %s"
                 closest-makefile
                 (file-name-directory closest-makefile)))))

(defun aluuu/untabify ()
  (interactive)
  (if (not indent-tabs-mode) (untabify (point-min) (point-max))))

(aluuu/check-packages
 (list
  'compile
  'uniquify
  'multi-web-mode
  'paredit
  'clojure-mode
  'haskell-mode
  'color-theme
  'color-theme-sanityinc-solarized
  'yaml-mode
  'smex
  'js2-mode
  'epa
  'magit
  'tuareg
  'emmet-mode
  'jsx-mode))

(aluuu/mode-for-hooks
 #'enable-paredit-mode
 '(emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook
   clojure-mode-hook))

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation
  Source: http://www.emacswiki.org/emacs/CompileCommand"
  (let ((root (expand-file-name "/")))
    (expand-file-name
     file
     (loop
      for d = default-directory then (expand-file-name ".." d)
      if (file-exists-p (expand-file-name file d))
      return d
      if (equal d root)
      return nil))))

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
(server-mode)

(ido-mode 1)
(global-set-key (kbd "C-s-SPC") 'aluuu/mode-line-in-header)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "M-x") 'smex)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 '(auto-save-file-name-transforms (quote ((".*" "/tmp/emacs\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/tmp/emacs"))))
 '(before-save-hook (quote (delete-trailing-whitespace aluuu/untabify)))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes (quote ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(display-time-default-load-average nil)
 '(display-time-format "%H:%M %d.%m.%Y")
 '(display-time-mode t)
 '(exec-path (cons "/home/aluuu/.cabal/bin" exec-path))
 '(fci-rule-color "#efefef")
 '(haskell-font-lock-symbols (quote unicode))
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-font-lock)))
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "sbcl")
 '(multi-web-global-mode t nil (multi-web-mode))
 '(mweb-default-major-mode (quote html-mode))
 '(mweb-filename-extensions (quote ("php" "htm" "html" "ctp" "phtml" "php4" "php5")))
 '(mweb-tags (quote ((tpl-mode "{%|{{" "}}|%}") (php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>") (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>") (css-mode "<style +type=\"text/css\"[^>]*>" "</style>"))))
 '(show-paren-mode t)
 '(slime-repl-history-size 1000)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#c82829") (40 . "#f5871f") (60 . "#eab700") (80 . "#718c00") (100 . "#3e999f") (120 . "#4271ae") (140 . "#8959a8") (160 . "#c82829") (180 . "#f5871f") (200 . "#eab700") (220 . "#718c00") (240 . "#3e999f") (260 . "#4271ae") (280 . "#8959a8") (300 . "#c82829") (320 . "#f5871f") (340 . "#eab700") (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil)
 '(whitespace-style (quote (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
'(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 97 :width normal :foundry "paratype" :family "PT Mono"))))
 '(mode-line ((t (:background "black" :foreground "#f6f3e8"))))
 '(web-mode-current-column-highlight-face ((t nil)) t)
 '(web-mode-whitespace-face ((t (:foreground "gray25"))) t)
 '(whitespace-newline ((t (:foreground "light gray"))))
 '(whitespace-space ((t (:foreground "gainsboro")))))

(add-to-list 'auto-mode-alist '("emacs\\'" . emacs-lisp-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "js")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("css" (or (name . "\\.css\\'")
                          (mode . css-mode)))
               ("js" (or (name . "\\.js\\'")
                         (mode . js2-mode)
                         (mode . js-mode)))
               ("ocaml" (or (name . "\\.ml\\'")
                            (name . "_oasis\\'")))
               ("ocaml interface" (name . "\\.mli\\'"))
               ("make" (or (mode . makefile-gmake-mode)
                           (mode . makefile-mode)))
               ("yaml" (mode . yaml-mode))
               ;; ("agda" (mode . agda2-mode))
               ("python" (mode . python-mode))
               ("git" (or (mode . magit-mode)
                          (name . "^\\*magit")))
               ("org" (mode . org-mode))
               ("emacs" (or
                         (name . "emacs\\'")
                         (name . "\\.emacs\\'")
                         (name . "\\*scratch\\*\\'")
                         (name . "\\*Messages\\*\\'")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'tuareg-mode-hook 'aluuu/ocaml-run-tests)

(setq org-publish-project-alist
      '(("notes"
         :base-directory "~/notes/"
         :publishing-directory "~/notes/build"
         :section-numbers nil
         :table-of-contents nil
         :style "<link rel=\"stylesheet\"
                     href=\"style.css\"
                     type=\"text/css\"/>")))

;; Agda support
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
(autoload 'forward-whitespace "thingatpt" nil t)
