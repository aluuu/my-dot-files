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

(defun aluuu/ocaml-setup ()
  (interactive)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (let* ((share-path (shell-command-to-string "opam config var share 2> /dev/null | tr -d '\012'"))
         (share-path-exists (car (file-attributes share-path)))
         (bin-path (shell-command-to-string "opam config var bin 2> /dev/null | tr -d '\012'"))
         (bin-path-exists (car (file-attributes bin-path))))
    (when (and bin-path-exists share-path-exists)
      (let ((share-path (substring share-path 0 -1))
            (bin-path (substring bin-path 0 -1))))
      (add-to-list 'load-path (concat share-path "/emacs/site-lisp"))
      (require 'merlin)
      (require 'utop)
      (require 'ocp-indent)
      (autoload 'utop "utop" "Toplevel for OCaml" t)

      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      (setq merlin-command (concat bin-path "/ocamlmerlin"))
      (custom-set-variables
       '(ocp-indent-path (concat bin-path "/ocp-indent")))
      (setq utop-command "opam config exec -- utop -emacs")))

  (defun aluuu/ocaml-run-tests ()
    ;; TODO: copy user's environment before `make` execution.
    ;;       maybe, this action should be made not only for this function,
    ;;       but for `emacs` process in general
    (let ((closest-makefile (get-closest-pathname)))
      (set (make-local-variable 'compile-command)
           (format "make -f %s test -C %s"
                   closest-makefile
                   (file-name-directory closest-makefile))))))

(defun aluuu/untabify ()
  (interactive)
  (if (not indent-tabs-mode) (untabify (point-min) (point-max))))

(aluuu/check-packages
 (list
  'ergoemacs-mode
  'exec-path-from-shell
  'compile
  'uniquify
  'paredit
  'clojure-mode
  'haskell-mode
  'color-theme
  'yaml-mode
  'smex
  'js2-mode
  'epa
  'magit
  'tuareg
  'emmet-mode
  'jsx-mode
  'markdown-mode
  'company
  'rubocop
  'flycheck
  'slim-mode
  'sass-mode
  'web-mode
  'elm-mode
  'ensime
  'etags-table
  'org-journal))

(aluuu/mode-for-hooks
 #'enable-paredit-mode
 '(emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook
   clojure-mode-hook))

(aluuu/mode-for-hooks
 #'flycheck-mode
 '(ruby-mode-hook))

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

(exec-path-from-shell-initialize)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "guerry")
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(display-time-mode 1)
; (global-whitespace-mode)
(paredit-mode 1)
(epa-file-enable)
(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)
;; NOTE:
;; MacOS hint - if your emacscleint can't find any emacs
;; server running do the following:
;; $ sudo mv /usr/bin/emacsclient /usr/bin/emacsclient.old
;; $ sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/bin/emacsclient
(server-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "/tmp/emacs\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/tmp/emacs"))))
 '(before-save-hook (quote (delete-trailing-whitespace aluuu/untabify)))
 '(blink-cursor-mode t)
 '(case-fold-search nil)
 '(column-number-mode t)
 '(cursor-type (quote bar))
 '(dabbrev-case-fold-search nil)
 '(dabbrev-upcase-means-case-search t)
 '(display-time-default-load-average nil)
 '(display-time-format "%H:%M %d.%m.%Y")
 '(display-time-mode t)
 '(elm-indent-offset 2)
 '(exec-path
   (cons "/usr/local/bin"
         (cons "~/.cabal/bin"
               (cons "~/.opam/system/bin" exec-path))))
 '(flycheck-elm-main-file nil)
 '(global-undo-tree-mode nil)
 '(haskell-font-lock-symbols (quote unicode))
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-font-lock)))
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-faces t)
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "sbcl")
 '(js-indent-level 2)
 '(mweb-default-major-mode (quote html-mode))
 '(mweb-filename-extensions (quote ("php" "htm" "html" "ctp" "phtml" "php4" "php5")))
 '(mweb-tags
   (quote
    ((tpl-mode "{%|{{" "}}|%}")
     (php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
     (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
     (css-mode "<style +type=\"text/css\"[^>]*>" "</style>"))))
 '(ns-function-modifier (quote none))
 '(ocp-indent-path (concat bin-path "/ocp-indent"))
 '(org-journal-date-format "%A, %Y-%m-%d")
 '(org-journal-dir "~/workspace/personal/journal/")
 '(org-journal-enable-encryption nil)
 '(org-journal-file-format "%Y%m%d.org")
 '(reb-re-syntax (quote string))
 '(show-paren-mode t)
 '(slime-repl-history-size 1000)
 '(sql-postgres-program "/usr/local/bin/psql")
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
'(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130))))
 '(window-divider ((t (:foreground "dark gray")))))


(if (eq system-type 'darwin)
    (custom-set-faces '(default ((t (:height 130)))))
  (custom-set-faces '(default ((t (:height 98))))))

(add-to-list 'auto-mode-alist '("emacs\\'" . emacs-lisp-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "js")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("elm" (or (name . "\\.elm\\'")
                          (mode . elm-mode)))
               ("elixir" (name . "\\.ex\\'"))
               ("css" (or (name . "\\.css\\'")
                          (mode . css-mode)))
               ("js" (or (name . "\\.js\\'")
                         (name . "\\.jsx\\'")
                         (mode . js2-mode)
                         (mode . js-mode)))
               ("ocaml" (or (name . "\\.ml\\'")
                            (name . "\\.mli\\'")
                            (name . "_oasis\\'")))
               ("ruby" (or (name . "\\.rb\\'")
                           (name . "Gemfile\\'")
                           (name . "config.ru\\'")))
               ("make" (or (mode . makefile-gmake-mode)
                           (mode . makefile-mode)))
               ("yaml" (mode . yaml-mode))
               ("python" (mode . python-mode))
               ("git" (or (mode . magit-mode)
                          (name . "^\\*magit")))
               ("org" (mode . org-mode))
               ("markdown" (mode . markdown-mode))
               ("haskell" (mode . haskell-mode))
               ("emacs" (or
                         (name . "emacs\\'")
                         (name . "\\.emacs\\'")
                         (name . "\\*scratch\\*\\'")
                         (name . "\\*Messages\\*\\'")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(aluuu/ocaml-setup)
(add-hook 'tuareg-mode-hook 'aluuu/ocaml-run-tests)
(add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'elm-mode-hook 'elm-oracle-setup-completion)

(autoload 'forward-whitespace "thingatpt" nil t)

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("group_vars\\/all$" . yaml-mode))
(setq inferior-octave-prompt ">> ")
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(setq magit-last-seen-setup-instructions "1.4.0")
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; (add-to-list 'company-backends 'company-elm)
(setq elm-format-on-save t)

(add-hook 'elm-mode-hook (lambda ()
                           (setq default-directory (elm--find-dependency-file-path))))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))


(when (window-system)
  (set-default-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
               (35 . ".\\(?:[(?[_{]\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (58 . ".\\(?:[:=]\\)")
               (59 . ".\\(?:;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:[:=?]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:[=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))


(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ("notes"
         :base-directory "~/workspace/notes/"
         :base-extension "org"
         :publishing-directory "~/workspace/notes_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )
        ))

(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(set-register ?a '(file . "~/workspace/notes/work/alternatus.org"))

(ido-mode 1)
(global-set-key (kbd "C-s-SPC") 'aluuu/mode-line-in-header)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<f8>") 'call-last-kbd-macro)
(define-key org-mode-map (kbd "C-c C-x d") nil)
(define-key org-mode-map (kbd "C-c C-x d") 'org-decrypt-entry)
