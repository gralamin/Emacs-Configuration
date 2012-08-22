;; flymake-pycheckers.el
;; Provides pep8, pyflakes, and pylint to python coders.
;; From http://stackoverflow.com/questions/1259873/how-can-i-use-emacs-flymake-mode-for-python-with-pyflakes-and-pylint-checking-co
;; Also pased on flymake-python-pyflakes.el

(defvar pycheckers-script (concat gral-config-scripts "pycheckers.sh"))
(defvar flymake-python-pycheckers-allowed-file-name-masks '(("\\.py\\'" flymake-python-pycheckers-init)))

(defun flymake-python-pycheckers-init ()
  (list pycheckers-script
        (list
         (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))

;;;;#autoload
(defun flymake-python-pycheckers-load ()
  (interactive)
  (set (make-local-variable 'flymake-allowed-file-name-masks)
       flymake-python-pycheckers-allowed-file-name-masks)
  (if (executable-find pycheckers-script)
      (flymake-mode t)
    (message "Not enabling flymake: executable '%s' not found." pycheckers-script)))

(provide 'flymake-pycheckers)