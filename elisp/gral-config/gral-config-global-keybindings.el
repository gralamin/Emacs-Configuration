;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind rectangle commands to Ctrl+Shift+X instead, to make them work with CUA-mode
(global-set-key (kbd "C-X r k") 'kill-rectangle)
(global-set-key (kbd "C-X r d") 'delete-rectangle)
(global-set-key (kbd "C-X r y") 'yank-rectangle)
(global-set-key (kbd "C-X r o") 'open-rectangle)
(global-set-key (kbd "C-X r c") 'clear-rectangle)
(global-set-key (kbd "C-X r w") 'delete-whitespace-rectangle)
(global-set-key (kbd "C-X r t") 'string-rectangle)
(global-set-key (kbd "C-X r i") 'string-insert-rectangle)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-p") 'occur-python)
(global-set-key (kbd "C-X ~") 'set-122-columns)
(global-set-key (kbd "C-X t c a") 'run-chimera-unit-tests)
(global-set-key (kbd "C-X t c p") 'run-chimera-unit-tests-at-path)
(global-set-key (kbd "C-X t c c") 'run-current-chimera-unit-test)
(global-set-key (kbd "C-X t m") 'run-metrics-unit-tests)
(define-key global-map (kbd "C-<kp-add>") 'text-scale-increase)
(define-key global-map (kbd "C-<kp-subtract>") 'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key [f5] 'refresh-file)
;; frame configuration within a session
(global-set-key [f8]
                'th-save-frame-configuration)
(global-set-key [f9]
                'th-ump-to-register)

;; Dedicated windows
(global-set-key [pause] 'toggle-current-window-dedication)

;; Switch windows with alt.
(global-set-key (kbd "M-<right>") 'select-next-window)
(global-set-key (kbd "M-<left>")  'select-previous-window)

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(require 'find-file-in-repository)
(global-set-key (kbd "C-x f") 'find-file-in-repository)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-.") 'mc/edit-lines)
(global-set-key (kbd "C-]") 'mc/mark-next-like-this)
;; C-[ Does weird things... find better
;;(global-set-key (kbd "C-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-{") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;
;; Searching
;;;;;;;;;;;;;;;;;;;;
;; add occur to searching to get all occurences of search string
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))
;;(global-set-key (kbd "C-s") 'flex-isearch-forward)
;;(global-set-key (kbd "C-r") 'flex-isearch-backward)

(provide 'gral-config-global-keybindings)
