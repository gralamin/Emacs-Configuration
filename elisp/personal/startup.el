;; Bunch of things to start up.
;; CSS
(require 'dot-css)

(require 'eval-after-load)

(require 'htmlfontify)
(require 'revive)

;; Enable flyspell
(setq flyspell-issue-welcome-flag nil)
(flyspell-mode)

(put 'scroll-left 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
