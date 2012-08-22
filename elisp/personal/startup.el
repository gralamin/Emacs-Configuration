;; Bunch of things to start up.
(require 'auto-complete)
(require 'auto-complete-yasnippet)
(require 'smart-operator)
(require 'auto-complete-config)
;; CSS
(require 'dot-css)

(require 'eval-after-load)

(require 'htmlfontify)
(require 'whitespace)

(require 'revive)

(require 'flymake)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp//ac-dict")

(global-auto-complete-mode t)

(global-whitespace-mode)

;; Ensure auto-complete
(ac-flyspell-workaround)

;; Enable flyspell
(setq flyspell-issue-welcome-flag nil)
(flyspell-mode)

(put 'scroll-left 'disabled nil)

;; Bunch of things to start up.
(require 'auto-complete)
(require 'auto-complete-yasnippet)
(require 'smart-operator)
(require 'auto-complete-config)
;; CSS
(require 'dot-css)

(require 'htmlfontify)
(require 'whitespace)

(require 'revive)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp//ac-dict")

(global-auto-complete-mode t)

(global-whitespace-mode)

;; Ensure auto-complete
(ac-flyspell-workaround)

;; Enable flyspell
(setq flyspell-issue-welcome-flag nil)
(flyspell-mode)

(put 'scroll-left 'disabled nil)

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
