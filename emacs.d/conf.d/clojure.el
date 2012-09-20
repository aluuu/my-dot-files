
(defun my-clojure-mode-hook ()
  (hs-minor-mode 1)
  ;; (paredit-mode 1)
  (highlight-symbol-mode)
  (lambda-mode 1)
  (slime-mode 1)
  (setq highlight-symbol-face '((:underline t)))
  (custom-set-faces '(highlight-symbol-face ((((class color) (background dark)) (:background "yellow"))))))

(setq auto-mode-alist
      (append '(("\\.clj$" . clojure-mode)
                ("\\.cljs$" . clojure-mode))
              auto-mode-alist))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
