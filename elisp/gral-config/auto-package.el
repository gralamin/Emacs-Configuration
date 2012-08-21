;; Contact package server, and install basic packages
(require 'cl)
(require 'package)
;; Custom package archive
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar gral-packages
  '(apache-mode
    auto-complete
    color-theme
    csv-mode
    flymake-css
    flymake-jslint
    flymake-sass
    flymake-shell
    magit
    magithub
    org
    rainbow-mode
    js2mode
    smart-operator
    yasnippet
    volatile-highlight
    zenburn-theme)
  "A List of packages to ensure are installed at launch.")

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
                                 unless (package-installed-p ',package)
                                 (package-intsall ',package))
                             (,mode))))

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

(provide 'gral-packages)
;;; auto-package.el ends here