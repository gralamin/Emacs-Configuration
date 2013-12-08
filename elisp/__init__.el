;;; init.el Use as your .emacs

(message "Gral-config is powering up... Be patient, Master %s!" (getenv "USER"))

(defvar gral-repo-root (concat (file-name-directory load-file-name) ".emacs.d/")
  "The root dir of the Emacs Gral-config distribution.")
(defvar gral-config-scripts (concat gral-repo-root "scripts/")
  "scripts to use with gral-config.")
(defvar gral-config-dir (concat gral-repo-root "elisp/")
  "The elisp dir of the Emacs Gral-config distribution.")
(defvar gral-config-modules-dir (concat gral-config-dir "gral-config/")
  "This directory houses all of the built-in Gral-Config module. You should
avoid modifying the configuration there.")
(defvar gral-config-personal-dir (concat gral-config-dir "personal/")
  "Users of Emacs Gral-Config are encouraged to keep their personal configuration
changes in this directory. All Emacs Lisp files there are loaded automatically
by Gral-Config.")
(defvar gral-config-vendor-dir (concat gral-config-dir "required/")
  "This directory house Emacs Lisp packages that are not yet available in
ELPA (or MELPA).")
(defvar gral-config-magit-dir (concat gral-config-dir "required/magit-git")
  "This directory houses magit which is broken in packages.el")
(defvar gral-config-savefile-dir (concat gral-config-dir "savefile/")
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p gral-config-savefile-dir)
  (make-directory gral-config-savefile-dir))

;; add Gral-Config's directories to Emacs's `load-path'
(add-to-list 'load-path gral-config-dir)
(add-to-list 'load-path gral-config-modules-dir)
(add-to-list 'load-path gral-config-vendor-dir)
(add-to-list 'load-path gral-config-magit-dir)

;; Load packages if it exists and this is emacs 23
(setq package-file (concat gral-config-vendor-dir "package.el"))
(setq version-value-foo (version))
(if (string-match "Emacs 24" version-value-foo)
    (message "Not loading package.el, as emacs 24 has it included.")
  (if (file-exists-p package-file)
      (load package-file)
    (message (concat "** Could not load custom file: " package-file))
  )
)

;; the core stuff
(require 'gral-config-packages)
(require 'gral-config-ui)
(require 'gral-config-core)
;;(require 'gral-config-mode)
;;(require 'gral-config-editor)
(require 'gral-config-global-keybindings)

;; config changes made through the customize UI will be store here
(setq custom-file (concat gral-config-personal-dir "custom.el"))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p gral-config-personal-dir)
  (mapc 'load (directory-files gral-config-personal-dir 't "^[^#].*el$")))

(message "Gral-Config is ready to do thy bidding, Master %s!" (getenv "USER"))

;;; init.el ends here
