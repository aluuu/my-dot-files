;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance settings                                  ;;
;;   @requires: tabbar-mode                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tabbar)
(tabbar-mode)

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
 This function is a custom function for tabbar-mode's tabbar-buffer-groups.
 This function group all buffers into 3 groups:
 Those Dired, those user buffer, and those emacs buffer.
 Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs Buffer")
    ((eq major-mode 'dired-mode) "Dired")
    (t "User Buffer")))) ;; from Xah Lee
(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

;; font settings
(set-default-font "-unknown-Anonymous Pro-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(global-linum-mode 1)
(require 'color-theme)

;; (require 'color-theme-solarized)
;; (color-theme-solarized-dark)
;; (set-face-foreground 'tabbar-default "#657b83")
;; (set-face-background 'tabbar-default "#002b36")
;; (set-face-foreground 'tabbar-selected "#586e75")
;; (set-face-bold-p 'tabbar-selected nil)
;; (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#002b36"))

(require 'color-theme-solarized)
(color-theme-solarized-light)
(set-face-foreground 'tabbar-default "#586e75")
(set-face-background 'tabbar-default "#fdf6e3")
(set-face-foreground 'tabbar-selected "#073642")
(set-face-bold-p 'tabbar-selected nil)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#073642"))

;; (require 'color-theme-gruber-darker)
;; (color-theme-gruber-darker)
