;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance settings                                  ;;
;;   @requires: tabbar-mode                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-linum-mode 1)
(global-hl-line-mode 1)
(set-default-font "-unknown-Anonymous Pro-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(require 'tabbar)
(tabbar-mode)
(set-face-font 'tabbar-default "-unknown-Anonymous Pro-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(defun aluuu/choose-theme (name)
  (cond ((eq name `solarized-dark)
         (color-theme-solarized-dark)
         (set-face-foreground 'tabbar-default "#657b83")
         (set-face-background 'tabbar-default "#002b36")
         (set-face-foreground 'tabbar-selected "#586e75")
         (set-face-bold-p 'tabbar-selected nil)
         (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#002b36")))
        ((eq name `solarized-light)
         (color-theme-solarized-light)
         (set-face-foreground 'tabbar-default "#586e75")
         (set-face-background 'tabbar-default "#fdf6e3")
         (set-face-foreground 'tabbar-selected "#073642")
         (set-face-bold-p 'tabbar-selected nil)
         (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#073642")))
        ((eq name `gruber)
         (color-theme-gruber-darker))
        ((eq name `tomorrow) 
         (color-theme-sanityinc-tomorrow-night)
         (set-face-foreground 'tabbar-default "#c5c8c6")
         (set-face-background 'tabbar-default "#1d1f21")
         (set-face-foreground 'tabbar-selected "#8abeb7")
         (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#1d1f21")))))

(aluuu/choose-theme `tomorrow)

;; (setq tabbar-ruler-global-tabbar 't) 
(setq tabbar-ruler-global-ruler 't)
;; (setq tabbar-ruler-popup-menu 't) 
;; (setq tabbar-ruler-popup-toolbar 't)


