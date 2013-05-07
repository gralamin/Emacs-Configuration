(setq yas-snippet-dirs
    (append yas-snippet-dirs
        (cons
            (concat
                (expand-file-name "~")
                "/.emacs.d/elisp/personal/snippets/"
            )
            ()
        )
    )
)
