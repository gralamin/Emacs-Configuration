;; Most of this file stolen from
;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/
;;Additional bits stolen from http://code.google.com/p/dot-emacs/source/browse/trunk/brettatoms/emacs/config/my-python.el?r=118
(require 'python)

;; Use Python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Autofill inside of comments
;(defun python-auto-fill-comments-only ()
;  (auto-fill-mode 1)
;  (set (make-local-variable 'fill-nobreak-predicate)
;       (lambda ()
;         (not (python-in-string/comment)))))

;; pymacs
;(if (featurep 'pymacs) ; always load it instead
    ; If installed, then load it
;    (lambda ()
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
      ;; This section isn't required.
      ;;(eval-after-load "pymacs"
      ;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;      ))

(require 'python-mode)
(defun my-python-mode-hook ()
  ;(if (fboundp 'pymacs-load)
      ; If pymacs was installed, then load ropemacs
      ;(lambda ()
  (pymacs-load "ropemacs" "rope-")
  (ropemacs-mode t)
  (set-variable 'py-indent-offset 4)
  (set-variable 'py-smart-indentation nil)
  (set-variable 'python-indent 4)
  (set-variable 'indent-tabs-mode nil)
;       (set-variable 'indent-tabs-mode nil)
  (define-key py-mode-map (kbd "RET") 'newline-and-indent)
        ;(define-key py-mode-map [tab] 'yas/expand)
  (setq yas/after-exit-snippet-hook 'indent-according-to-mode)
  (require 'smart-operator)
  (require 'whitespace)
  (smart-operator-mode)
  (global-whitespace-mode)
   (setq ropemacs-enable-autoimport t)
        ;);)
)

(add-hook 'python-mode-hook 'my-python-mode-hook)


;; autofill
;  (auto-fill-mode 1)
;  (python-auto-fill-comments-only)
;  (set (make-local-variable 'fill-nobreak-predicate)
;       (lambda ()
;         (not (eq (get-text-property (point) 'face)
;                  'font-lock-comment-face))))
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun prefix-list-elements (list prefix)
;  (let (value)
;    (nreverse
;     (dolist (element list value)
;      (setq value (cons (format "%s%s" prefix element) value))))))
;(defvar ac-source-rope
;  '((candidates
;     . (lambda ()
 ;        (prefix-list-elements (rope-completions) ac-target))))
;  "Source for Rope")
;(defun ac-python-find ()
;  "Python `ac-find-function'."
;  (require 'thingatpt)
;  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;    (if (null symbol)
;        (if (string= "." (buffer-substring (- (point) 1) (point)))
;            (point)
;          nil)
;      symbol)))
;(defun ac-python-candidate ()
;  "Python `ac-candidates-function'"
;  (let (candidates)
;    (dolist (source ac-sources)
;      (if (symbolp source)
;          (setq source (symbol-value source)))
;      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;             (requires (cdr-safe (assq 'requires source)))
;             cand)
;        (if (or (null requires)
;                (>= (length ac-target) requires))
;            (setq cand
;                  (delq nil
;                        (mapcar (lambda (candidate)
;                                  (propertize candidate 'source source))
;                                (funcall (cdr (assq 'candidates source)))))))
;        (if (and (> ac-limit 1)
;                 (> (length cand) ac-limit))
;            (setcdr (nthcdr (1- ac-limit) cand) nil))
;        (setq candidates (append candidates cand))))
;    (delete-dups candidates)))
;(add-hook 'python-mode-hook
;          (lambda ()
;                 (auto-complete-mode 1)
;                 (set (make-local-variable 'ac-sources)
;                      (append ac-sources '(ac-source-rope) ;'(ac-source-yasnippet)))
;                 (set (make-local-variable 'ac-find-function) ;'ac-python-find)
;                 (set (make-local-variable 'ac-candidate-function) ;'ac-python-candidate)
;                 (set (make-local-variable 'ac-auto-start) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Syntax Error Hightlight
;; From http://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(when (load "flymake" t)
;  (defun flymake-pyflakes-init ()
;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;		       'flymake-create-temp-inplace))
;	   (local-file (file-relative-name
;			temp-file
;			(file-name-directory buffer-file-name))))
;      (list "pyflakes" (list local-file))))
    (defun flymake-pylint-init()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
;               '("\\.py\\'" flymake-pyflakes-init)))
               '("\\.py\\'" flymake-pylint-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)
(require 'flymake-cursor)

(set (make-local-variable 'ac-auto-start) t)

;; Note: This was named such when I stole it
;;Ryan's python specific tab completion
;(defun ryan-python-tab ()
  ; Try the following:
  ; 1) Do a yasnippet expansion
  ; 2) Do a Rope code completion
  ; 3) Do an indent
 ; (interactive)
;  (if (eql (ac-start) 0)
;      (indent-for-tab-command)))
 ;(defadvice ac-start (before advice-turn-on-auto-start activate)
 ;  (set (make-local-variable 'ac-auto-start) t))
 ;(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  ; (set (make-local-variable 'ac-auto-start) nil))

; (define-key py-mode-map "\t" 'ryan-python-tab)
; (define-key py-mode-map [(control tab)] 'indent-for-tab-command)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion

(smart-operator-mode)
(provide 'init_python)
