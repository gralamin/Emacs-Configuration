;; Run all tests

;; load all files in current directory
(require 'org)
(org-babel-load-file (expand-file-name "gral_config.org" gral-config-dir))
(load-file "./test_path.el")
(load-file "./test_project.el")

(require 'test-gral-path)
(require 'test-gral-project)

(ert "test:gral")
