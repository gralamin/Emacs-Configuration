;; Contact package server, and install basic packages
(require 'cl)
(require 'package)

;; Custom package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar gral-packages-24
  '(apache-mode
    auto-complete
    csv-mode
    css-mode
    expand-region
    flex-isearch
    flymake
    flymake-css
    flymake-cursor
    flymake-jslint
    flymake-sass
    flymake-shell
    flymake-python-pyflakes
    git-commit-mode
    git-rebase-mode
    gitconfig-mode
    gitignore-mode
    multiple-cursors
    org
    php-mode
    rainbow-mode
    revive
    js3-mode
    yasnippet
    volatile-highlights
    zenburn-theme)
  "A List of packages to ensure are installed at launch.")

(defvar gral-packages-23
  '(apache-mode
    auto-complete
    csv-mode
    flex-isearch
    flymake
    flymake-css
    flymake-cursor
    flymake-jslint
    flymake-sass
    flymake-shell
    flymake-python-pyflakes
    htmlfontify
    git-commit-mode
    git-rebase-mode
    gitconfig-mode
    gitignore-mode
    org
    revive
    js3-mode
    yasnippet
    volatile-highlights)
  "A List of packages to ensure are installed at launch.")

(if (string-match "Emacs 24" (version))
    (defvar gral-packages 'gral-packages-24)
  (defvar gral-packages 'gral-packages-23)
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
        (message "Installing %s" p)
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

(autoload 'python "python" "Python Mode." t)

(defvar gral-auto-install-alist
  '(
    ("\\.csv\\'" csv-mode csv-mode)
;;    ("\\.css\\'" css-mode css-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.sass\\'" sass-mode sass-mode)
;;    ("\\.php\\'" php-mode php-mode)
    ("\\.py\\'" python python)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.pl\\'" prolog prolog-mode)
  )
)

(dolist (entry gral-auto-install-alist)
  (let ((extension (first entry))
        (package (second entry))
        (mode (third entry)))
    (unless (package-installed-p package)
      (gral-auto-install extension package mode))))

(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(provide 'gral-config-packages)
;;; auto-package.el ends here
