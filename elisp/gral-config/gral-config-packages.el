;; Contact package server, and install basic packages
(require 'cl)
(require 'package)
;; Custom package archive
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar gral-packages
  '(apache-mode
    auto-complete
    color-theme
    csv-mode
    css-mode
    flymake-css
    flymake-jslint
    flymake-sass
    flymake-shell
    magit
    magithub
    org
    rainbow-mode
    revive
    js2-mode
    smart-operator
    yasnippet
    volatile-highlights)
  "A List of packages to ensure are installed at launch.")

(if (string-match "Emacs 24" (version))
    (add-to-list 'gral-packages 'zenburn-theme) ;; use zenburn as the default theme
  (message "You are not running Emacs 24. Theming disabled")
)

(defun gral-packages-installed-p ()
  (loop for p in gral-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t))
)

(defun gral-install-packages ()
  (unless (gral-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p gral-packages)
      (unless (package-installed-p p)
        (package-install p)
      )
    )
  )
)

(gral-install-packages)

(defmacro gral-auto-install (extension package mode)
`(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar gral-auto-install-alist
  '(
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.py\\'" python python-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
  )
)

(dolist (entry gral-auto-install-alist)
  (let ((extension (first entry))
        (package (second entry))
        (mode (third entry)))
    (unless (package-installed-p package)
      (gral-auto-install extension package mode))))

(provide 'gral-config-packages)
;;; auto-package.el ends here