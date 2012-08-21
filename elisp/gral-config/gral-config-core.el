;;;;;;;;;;;;;;;;;;;;
;; Modes / packages
;;;;;;;;;;;;;;;;;;;;
;; Generic
(autoload 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp//ac-dict")

;; Python
;;(load-library "init_python.el")
(global-auto-complete-mode t)

;; HTML + XML
(load (concat data-dir "nxhtml/autostart.el"))
(setq mumamo-background-colors nil)

(global-whitespace-mode)

(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;; Ensure auto-complete
(ac-flyspell-workaround)

;; Enable flyspell
(setq flyspell-issue-welcome-flag nil)
(flyspell-mode)

(put 'scroll-left 'disabled nil)

;; Refresh Buffer with f5.
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

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

;; Press PAUSE to dedicate a window.
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

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

;; Bit of jumping around windows
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))

(defun gral-config-indent-buffer ()
  "Indents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun gral-config-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun gral-config-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (gral-config-indent-buffer)
  (gral-config-untabify-buffer)
  (whitespace-cleanup))

(defun gral-config-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(provide 'gral-config-core)