(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style "linux")
 '(case-fold-search t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inferior-lisp-program "gcl")
 '(inhibit-startup-screen t)
 '(nxhtml-skip-welcome t)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(org-agenda-files (quote ("~/todo.org" "~/homework.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-reverse-note-order t)
 '(php-mode-force-pear t)
 '(reb-re-syntax (quote rx))
 '(sgml-basic-offset 4)
 '(show-paren-mode t)
 '(user-full-name "Glen Nelson")
 '(user-mail-address "glen.nelson@emc.com")
 '(which-function-mode t)
 '(whitespace-line-column 80))

(if (string-match "Emacs 2[4-9]" (version))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
     '(rst-level-3-face ((t (:background "#000000" :foreground "#7cb8bb"))) t))
  (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
   '(whitespace-line ((t (:foreground "dark violet"))))
   '(whitespace-space ((((class color) (background light)) (:foreground "darkgray"))))
   )
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(rst-level-3-face ((t (:background "#000000" :foreground "#7cb8bb"))) t))
