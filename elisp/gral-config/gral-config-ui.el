;; No need for server, due to running emacs as a daemon.
;;;;;;;;;;;;;;;;;;;;
;; Generic options
;;;;;;;;;;;;;;;;;;;;

; highlight current line
(global-hl-line-mode 1)
; display line numbers in margin (fringe). Emacs 23 only.
(global-linum-mode 1) ; always show line numbers
(global-visual-line-mode 1) ; Word wrap option.
(size-indication-mode t); Show file size

; Add .emacs.d/elisp

; for xml files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

; Use org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1) ; don't show the toolbar
(menu-bar-mode -1) ; don't show the menu

(setq visible-bell t) ; flash instead of beep
(blink-cursor-mode -1) ; flash instead of beep

; copy and paste with clipboard
(setq x-select-enable-clipboard t)

; geometry
; TODO: this should be dependent on the screen resolution
;(add-to-list 'default-frame-alist '(height . 39))
;(add-to-list 'default-frame-alist '(width . 80))

; scroll bar on the right
(set-scroll-bar-mode 'right)
(setq scroll-step 2)
; always font lock
(global-font-lock-mode t)
(setq-default show-trailing-whitespace t)

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; delete \b at line ends before saving a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Turn off fill-column key
(global-unset-key "\C-xf")
(setq fill-column 80)
(add-hook 'rst-mode-hook 'turn-on-auto-fill)

;; Autofill comments
(add-hook 'c-mode-common-hook
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))

;; Shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Tramp
(setq tramp-default-method "ssh")

;; For daemon mode, with-selected-frame seems to be required.  Normal
;; mode seems to require with-selected-frame to be absent.
(defun gral-config-frame-config (frame)
  "Custom behaviours for new frames."
  (if (eq system-type 'darwin)
      (if (server-running-p)
          (with-selected-frame frame
            (if (display-graphic-p)
                (modify-frame-parameters frame '((menu-bar-lines . 1)))
              (modify-frame-parameters frame '((menu-bar-lines . 0)))))
        (if (display-graphic-p)
            (modify-frame-parameters frame '((menu-bar-lines . 1)))
          (modify-frame-parameters frame '((menu-bar-lines . 0)))))
    (menu-bar-mode -1)))

(gral-config-frame-config (selected-frame))
(add-hook 'after-make-frame-functions 'gral-config-frame-config)

;;;;;;;;;;;;;;;;;;;;
;; Custom settings
;;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat data-dir "custom.el"))
(setq dis-temp (concat data-dir "disable-tempfiles.el"))
(if (file-exists-p custom-file)
    (load-file custom-file)
  (message (concat "** Could not load custom file: " custom-file))
)
(if (file-exists-p client-log)
    (lambda ()
      (load client-log)
      (add-to-list 'auto-mode-alist '("/client\\.log_" . client-log-mode))
      )
  (message (concat "** Could not load custom file: " client-log))
)
(if (file-exists-p dis-temp)
      (load dis-temp)
  (message (concat "** Could not load custom file: " dis-temp))
)

;; Load packages if it exists and this is emacs 23
(setq package-file (concat data-dir "package.el"))
(setq version-value-foo (version))
(if (string-match "Emacs 23" version-value-foo)
  (if (file-exists-p package-file)
      (load package-file)
    (message (concat "** Could not load custom file: " package-file))
  )
  (message (concat "** Emacs version not 23: " version-value-foo))
)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " gral-config " (:eval (if (buffer-file-name)
                                                      (abbreviate-file-name (buffer-file-name))
                                                    "%b"))))

;; use zenburn as the default theme
(load-theme 'zenburn t)

(provide 'gral-config-ui)
