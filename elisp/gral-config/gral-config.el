;; No need for server, due to running emacs as a daemon.
;;;;;;;;;;;;;;;;;;;;
;; Generic options
;;;;;;;;;;;;;;;;;;;;

 ; highlight current line
(global-hl-line-mode 1)
; display line numbers in margin (fringe). Emacs 23 only.
(global-linum-mode 1) ; always show line numbers
(global-visual-line-mode 1) ; Word wrap option.

; Add .emacs.d/elisp
(setq data-dir (expand-file-name "~/.emacs.d/elisp/"))
(add-to-list 'load-path data-dir)

; for xml files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

; Use org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1) ; don't show the toolbar
(menu-bar-mode -1) ; don't show the menu

(setq visible-bell t) ; flash instead of beep

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

;;;;;;;;;;;;;;;;;;;;
;; Searching
;;;;;;;;;;;;;;;;;;;;
;; add occur to searching to get all occurences of search string
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

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

;;;;;;;;;;;;;;;;;;;;
;; Modes / packages
;;;;;;;;;;;;;;;;;;;;
;; Generic
(require 'gral-packages)
(autoload 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(autoload 'auto-complete)
(autoload 'auto-complete)
(autoload 'auto-complete-yasnippet)
(autoload 'smart-operator)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp//ac-dict")

;; Python
;;(load-library "init_python.el")
(global-auto-complete-mode t)

;; CSS
(require 'dot-css)

(require 'eval-after-load)

;; HTML + XML
(autoload (concat data-dir "nxhtml/autostart.el"))
(setq mumamo-background-colors nil)

;; HTML-FONT-IFY
(require 'htmlfontify)

;; Stop myself from being an idoit:
;;(global-set-key (kbd "C-x C-c") 'kill-buffer)
;;(global-set-key (kbd "C-x C-k") 'keyboard-quit)
;; Disabled, since keyboard-quit doesn't work if I do this remapping.

(require 'whitespace)
(global-whitespace-mode)

;; Ensure auto-complete
(ac-flyspell-workaround)

;; Enable flyspell
(setq flyspell-issue-welcome-flag nil)
(flyspell-mode)

(put 'scroll-left 'disabled nil)

;; Bind rectangle commands to Ctrl+Shift+X instead:
;;C-x r k
;;Kill the text of the region-rectangle, saving its contents as the “last killed rectangle” (kill-rectangle).
;;C-x r d
;;Delete the text of the region-rectangle (delete-rectangle).
;;C-x r y
;;Yank the last killed rectangle with its upper left corner at point (yank-rectangle).
;;C-x r o
;;Insert blank space to fill the space of the region-rectangle (open-rectangle). This pushes the previous contents of the region-rectangle rightward.
;;C-x r c
;;Clear the region-rectangle by replacing all of its contents with spaces (clear-rectangle).
;;M-x delete-whitespace-rectangle
;;Delete whitespace in each of the lines on the specified rectangle, starting from the left edge column of the rectangle. (ADDED:Ctrl+
;;C-x r t string <RET>
;;Replace rectangle contents with string on each line (string-rectangle).
;;M-x string-insert-rectangle <RET> string <RET>
;;Insert string on each line of the rectangle. (ADDED:Ctrl+x r i)
(global-set-key (kbd "C-X r k") 'kill-rectangle)
(global-set-key (kbd "C-X r d") 'delete-rectangle)
(global-set-key (kbd "C-X r y") 'yank-rectangle)
(global-set-key (kbd "C-X r o") 'open-rectangle)
(global-set-key (kbd "C-X r c") 'clear-rectangle)
(global-set-key (kbd "C-X r w") 'delete-whitespace-rectangle)
(global-set-key (kbd "C-X r t") 'string-rectangle)
(global-set-key (kbd "C-X r i") 'string-insert-rectangle)

;; Refresh Buffer with f5.
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

(global-set-key [f5] 'refresh-file)
(global-set-key (kbd "M-p") 'occur-python)
(defun occur-python ()
  (interactive)
  (occur "^\\s-*\\(class .*:\\|def .*:\\)")
)

(defun occur-ssh ()
  (interactive)
  (occur "\\(Thread:\\|at: ch.ethz.ssh2.Connection\\)")
)

;; Add in the ability to set the size of a window.
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-122-columns ()
  "Set the selected window to 122 columns."
  (interactive)
  (set-window-width 122))

(global-set-key (kbd "C-X ~") 'set-122-columns)

;; Make flymake work with html
(defun flymake-html-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                   'flymake-create-temp-inplace))
         (local-file (file-relative-name
                    temp-file
                    (file-name-directory buffer-file-name))))
    (list "tidy" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.html$\\|\\.ctp" flymake-html-init))

(add-to-list 'flymake-err-line-patterns
             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
               nil 1 2 4))

;; Run unit tests asynchronously
(defun run-chimera-unit-tests ()
  (interactive)
  (shell-command "cd ~/git/chimera/yycli && ./runUnitTests.py &"))

(defun run-metrics-unit-tests ()
  (interactive)
  (shell-command "/usr/bin/python ~/git/django_chimera/manage.py test metrics &"))

(global-set-key (kbd "C-X t c") 'run-chimera-unit-tests)
(global-set-key (kbd "C-X t m") 'run-metrics-unit-tests)

;; Press PAUSE to dedicate a window.
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(global-set-key [pause] 'toggle-current-window-dedication)

(require 'git-emacs)
(require 'git-blame)

(defcustom th-frame-config-register ?
  "The register which is used for storing and restoring frame
configurations by `th-save-frame-configuration' and
`th-jump-to-register'.")

(defun th-save-frame-configuration (arg)
  "Stores the current frame configuration in register
`th-frame-config-register'. If a prefix argument is given, you
can choose which register to use."
  (interactive "P")
  (let ((register (if arg
                      (read-char "Which register? ")
                    th-frame-config-register)))
    (frame-configuration-to-register register)
    (message "Frame configuration saved in register '%c'."
             register)))

(defun th-jump-to-register (arg)
  "Jumps to register `th-frame-config-register'. If a prefix
argument is given, you can choose which register to jump to."
  (interactive "P")
  (let ((register (if arg
                      (read-char "Which register? ")
                    th-frame-config-register)))
    (jump-to-register register)
    (message "Jumped to register '%c'."
             register)))

(global-set-key [f8]
                'th-save-frame-configuration)
(global-set-key [f9]
                'th-jump-to-register)

(require 'windows)
(require 'revive)

;; Bit of jumping around windows
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

(global-set-key (kbd "M-<right>") 'select-next-window)
(global-set-key (kbd "M-<left>")  'select-previous-window)