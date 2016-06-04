;; Load this to get everything running

;; load Org-mode from source when the ORG_HOME environment variable is set

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the config after all packages.
(add-hook 'after-init-hook
 `(lambda ()
    (setq gral-config-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  ,(file-directory-p (expand-file-name "lisp"
                                                       (getenv "ORG_HOME"))))
       '(require 'org))
    (org-babel-load-file (expand-file-name "gral_config.org" gral-config-dir))))
