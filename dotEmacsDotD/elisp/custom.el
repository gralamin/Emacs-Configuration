(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)        ; Set basic offset for lots of stuff
 '(c-default-style "linux") ; Set basic style for lots of stuff
 '(case-fold-search t)      ; Case insensitive search
 '(column-number-mode t)    ; Display column numbers
 '(current-language-environment "UTF-8") ; Enforce UTF-8 environment
 '(default-input-method "latin-1-prefix") ;  Enforce input method
 '(global-font-lock-mode t nil (font-lock)) ; Put on font-lock mode.
 '(indicate-empty-lines t) ; Indicate if a line is empty in the fringe.
 '(nxhtml-auto-mode-alist (quote (("\\.x?html?\\'" . nxhtml-mumamo) ("\\.x?htmlf?\\'" . nxhtml-mumamo) ("\\.php\\'" . nxhtml-mumamo) ("\\.phtml\\'" . nxhtml-mumamo) ("\\.jsp\\'" . jsp-nxhtml-mumamo) ("\\.asp\\'" . asp-nxhtml-mumamo) ("\\.djhtml\\'" . django-nxhtml-mumamo) ("\\.rhtml\\'" . eruby-nxhtml-mumamo) ("\\.phps\\'" . smarty-nxhtml-mumamo) ("\\.epl\\'" . embperl-nxhtml-mumamo) (".lzx\\'" . laszlo-nxml-mumamo) ("\\.js\\'" . js2-mode) ("\\.css\\'" . css-mode)))) ;;Nxhtml stuff
 '(nxhtml-default-encoding (quote utf-8)) ;; nxhtml stuff
 '(nxhtml-minor-mode-modes (quote (nxhtml-mode nxml-mode html-mode sgml-mode xml-mode php-mode css-mode java-mode image-mode dired-mode))) ; nxhtml stuff
 '(nxhtml-skip-welcome t) ; skip welcome
 '(show-paren-mode t) ; see matching parentheses
 '(transient-mark-mode t) ; Makes emacs a bit more like most editors
 '(which-function-mode t) ; Reminds you which function you are in.
 '(user-mail-address "glen.nelson@emc.com") ; Yasnippet variable
 '(user-full-name "Glen Nelson") ; Yasnippet variable
 '(cua-mode t nil (cua-base)) ; Use windows copy/paste
 '(indent-tabs-mode nil) ; No tabs ever.
 '(inhibit-startup-screen t) ; No startup screen
  ;; Org options
 '(org-agenda-files (quote ("~/todo.org")))
 '(org-default-notes-file "~/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 ;; Other options
 '(sgml-basic-offset 4)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(php-mode-force-pear t)
 '(whitespace-line-column 120)
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "grey17" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown")))))
