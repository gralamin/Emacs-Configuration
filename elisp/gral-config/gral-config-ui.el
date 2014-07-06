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

;(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;;;;;;;;;;;;;;;;;;;;
;; Requires
;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(require 'volatile-highlights)

;(require 'flex-isearch)

(require 'auto-complete)
(require 'auto-complete-yasnippet)

(require 'multiple-cursors)
(require 'expand-region)

(require 'smart-operator)
(require 'auto-complete-config)

(require 'whitespace)
(require 'magit)
(require 'jedi)

(require 'flymake)
(require 'flymake-cursor)
(require 'flymake-pycheckers)
(require 'flymake-shell)
(require 'flymake-sass)
(require 'flymake-css)
(if (string-match "Emacs 24" (version))
    (require 'rainbow-mode)
)

;;(require 'php-mode)
;;(require 'flymake-python-pyflakes)
;;(require 'flymake-js)
;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;
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

;; Daemon-mode on make frame
(add-hook 'after-make-frame-functions 'gral-config-frame-config)

(add-hook 'python-mode-hook 'flymake-python-pycheckers-load)

;; Jedi setup
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(defvaralias 'venv-location 'gral-venv-location)

(defun project-directory (buffer-name)
  "Returns the root directory of the project that contains the
given buffer. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun project-name-old (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (file-name-nondirectory
         (directory-file-name root-dir))
      nil)))


(defun project-name (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (let ((p-name (file-name-nondirectory
                       (directory-file-name root-dir))))
          (if (string= p-name "vats")
              "v"
            (if (string= p-name "chimera_metrics")
                "ms"
              p-name)))
      nil)))


(defun jedi-setup-venv ()
  "Activates the virtualenv of the current buffer."
  (let ((project-name (project-name buffer-file-name)))
    (when project-name (venv-workon project-name))))

(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi-setup-venv)
(add-hook 'python-mode-hook 'jedi:setup)

;;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'sh-mode-hook 'flymake-shell-load)
(add-hook 'sass-mode-hook 'flymake-sass-load)

(add-hook 'js-mode-hook 'flymake-jslint-load)
(add-hook 'css-mode-hook 'flymake-css-load)

;;;;;;;;;;;;;;;;;;;;
;; Global modes
;;;;;;;;;;;;;;;;;;;;
(volatile-highlights-mode t)
(global-whitespace-mode)
(global-auto-complete-mode t)
;(global-flex-isearch-mode t)
(yas-global-mode 1)

;; Ensure auto-complete
(ac-flyspell-workaround)

;;;;;;;;;;;;;;;;;;;;
;; Custom settings
;;;;;;;;;;;;;;;;;;;;
;; Disable temporary files
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))
(setq delete-auto-save-files t)                ; no "#" files after a save
(setq auto-save-list-file-prefix nil)        ; don't record sessions

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " gral-config " (:eval (if (buffer-file-name)
                                                      (abbreviate-file-name (buffer-file-name))
                                                    "%b"))))

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

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp//ac-dict")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode: Highlight ints / floats as constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar font-lock-number "[+-]?[0-9]+\\([eE][+-]?[0-9]*\\)?")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
(defun add-font-lock-numbers ()
        (font-lock-add-keywords nil (list
                      (list (concat "\\<\\(" font-lock-number "\\)\\>" )
                       0 font-lock-constant-face)
                      (list (concat "\\<\\(" font-lock-hexnumber "\\)\\>" )
                       0 font-lock-constant-face)
                      )))

(add-hook 'python-mode-hook 'add-font-lock-numbers)

;;;;;;;;;;;;;;;;;;;;
;; On Save cleanup
;;;;;;;;;;;;;;;;;;;;
;; (add hook 'before-save-hook (lambda ()
;;                               (whitespace-cleanup)
;;                               (ident-region (point-min) (point-max))))

;; (add-hook 'makefile-mode-hook
;;           (lambda ()
;;             (setq indent-line-function
;;                   (lambda ()
;;                     (let*
;;                         ((p (thing-at-point 'paragraph))
;;                          (lines (split-string p "\n" t)))
;;                       (when (and
;;                              (string-match-p "^[^[:space:]]+:" (car lines))
;;                              (not (string-match-p "^[^[:space:]]+:" (line-at-point))))
;;                         (save-excursion
;;                           (indent-line-to 8)
;;                           (mark-paragraph)
;;                           (tabify (region-beginning) (region-end)))))))))

(add-hook 'html-mode-hook
      '(lambda()
        (setq c-basic-offset 4)
        (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;
(if (string-match "Emacs 24" (version))
    (load-theme 'zenburn t) ;; use zenburn as the default theme
  (message "You are not running Emacs 24. Theming disabled")
)

(require 'zone)
 (defun lock-screen ()
   "Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
   (interactive)
   (save-excursion
     ;(shell-command "xtrlock &")
     (set-process-sentinel
      (start-process "xtrlock" nil "xtrlock")
      '(lambda (process event)
         (zone-leave-me-alone)))
     (zone-when-idle 1)))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(provide 'gral-config-ui)
