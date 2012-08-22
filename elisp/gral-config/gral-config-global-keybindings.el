;; useful keybindings

;; Bind rectangle commands to Ctrl+Shift+X instead:
;;C-x r k
;;Kill the text of the region-rectangle, saving its contents as the “last killed rectangle” (kill-rectangle).
;;C-x r d
;;Delete the text of the region-rectangle (delete-rectangle).
;;C-x r y
;;Yank the last killed rectangle with its upper left corner at point (yank-rectangle).
;;C-x r o
;;Insert blank space to fill the space of the region-rectangle (open-rectangle). This pushes the previous contents of the region-rectangle rightward.
;;C-x r c
;;Clear the region-rectangle by replacing all of its contents with spaces (clear-rectangle).
;;M-x delete-whitespace-rectangle
;;Delete whitespace in each of the lines on the specified rectangle, starting from the left edge column of the rectangle. (ADDED:Ctrl+
;;C-x r t string <RET>
;;Replace rectangle contents with string on each line (string-rectangle).
;;M-x string-insert-rectangle <RET> string <RET>
;;Insert string on each line of the rectangle. (ADDED:Ctrl+x r i)
(global-set-key (kbd "C-X r k") 'kill-rectangle)
(global-set-key (kbd "C-X r d") 'delete-rectangle)
(global-set-key (kbd "C-X r y") 'yank-rectangle)
(global-set-key (kbd "C-X r o") 'open-rectangle)
(global-set-key (kbd "C-X r c") 'clear-rectangle)
(global-set-key (kbd "C-X r w") 'delete-whitespace-rectangle)
(global-set-key (kbd "C-X r t") 'string-rectangle)
(global-set-key (kbd "C-X r i") 'string-insert-rectangle)

(global-set-key [f5] 'refresh-file)
(global-set-key (kbd "M-p") 'occur-python)
(global-set-key (kbd "C-X ~") 'set-122-columns)

(global-set-key (kbd "C-X t c") 'run-chimera-unit-tests)
(global-set-key (kbd "C-X t m") 'run-metrics-unit-tests)

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

(define-key global-map (kbd "C-<kp-add>") 'text-scale-increase)
(define-key global-map (kbd "C-<kp-subtract>") 'text-scale-decrease)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-s") 'flex-isearch-forward)
(global-set-key (kbd "C-r") 'flex-isearch-backward)

(provide 'gral-config-global-keybindings)
