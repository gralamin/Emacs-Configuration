;; Contains ERT tests for the project library

;; Local Variables:
;; gral:prj-unit-test-path: "test/unit"
;; gral:prj-integration-test-path: "test/integration"
;; gral:prj-test-format: "test_%s"
;; gral:prj-code-path: ""
;; gral:prj-unit-test-runner: "nosetests"
;; gral:prj-current-unit-test-remove-file: nil
;; gral:prj-test-reverse: (lambda (file) (car (last (split-string file "test_") 1)))
;; gral:prj-build-test-command: (lambda (args) (concat "cd " (projectile-project-root) " && " (gral:unit-test-runner) " " args " &"))
;; End:

(require 'el-mock)

(ert-deftest test:gral:switch-int-test--in-int-test ()
  "Tests gral:switch-int-test"
  (message "int-test--in-int-test")
  (let ((gral:prj-unit-test-path "test/unit")
        (gral:prj-packages '("prj1" "prj2"))
        (gral:prj-integration-test-path "test/integration"))
    (mocklet ((file-truename => "/home/gralamin/project/test/integration/prj1/test_foo.py")
              (projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:test-to-file "test_foo.py"
      (should (equal (__gral:switch-int-test "test_foo.py")
                     "/home/gralamin/project/prj1/foo.py"))))
)

(ert-deftest test:gral:switch-int-test--in-code ()
  "Tests gral:switch-int-test"
  (let ((gral:prj-unit-test-path "test/unit")
        (gral:prj-packages '("prj1" "prj2"))
        (gral:prj-integration-test-path "test/integration"))
    (mocklet ((file-truename => "/home/gralamin/project/prj1/foo.py")
              (projectile-project-root => "/home/gralamin/project/"))
      (should (equal (__gral:switch-int-test "foo.py")
                     "/home/gralamin/project/test/integration/prj1/test_foo.py"))))
)

(ert-deftest test:gral:switch-int-test--in-unit-test ()
  "Tests gral:switch-int-test"
  (let ((gral:prj-unit-test-path "test/unit")
        (gral:prj-packages '("prj1" "prj2"))
        (gral:prj-integration-test-path "test/integration"))
    (mocklet ((file-truename => "/home/gralamin/project/test/unit/prj1/test_foo.py")
              (projectile-project-root => "/home/gralamin/project/"))
      (should (equal (__gral:switch-int-test "test_foo.py")
                     "/home/gralamin/project/test/integration/prj1/test_foo.py"))))
)

(ert-deftest test:gral:switch-unit-test--in-unit-test ()
  "Tests gral:switch-int-test"
  (let ((gral:prj-unit-test-path "test/unit")
        (gral:prj-packages '("prj1" "prj2"))
        (gral:prj-integration-test-path "test/integration"))
    (mocklet ((file-truename => "/home/gralamin/project/test/unit/prj1/test_foo.py")
              (projectile-project-root => "/home/gralamin/project/"))
      (should (equal (__gral:switch-unit-test "test_foo.py")
                     "/home/gralamin/project/prj1/foo.py"))))
)

(ert-deftest test:gral:switch-unit-test--in-code ()
  "Tests gral:switch-int-test"
  (let ((gral:prj-unit-test-path "test/unit")
        (gral:prj-packages '("prj1" "prj2"))
        (gral:prj-integration-test-path "test/integration"))
    (mocklet ((file-truename => "/home/gralamin/project/prj1/foo.py")
              (projectile-project-root => "/home/gralamin/project/"))
      (should (equal (__gral:switch-unit-test "foo.py")
                     "/home/gralamin/project/test/unit/prj1/test_foo.py"))))
)

(ert-deftest test:gral:switch-unit-test--in-int-test ()
  "Tests gral:switch-int-test"
  (let ((gral:prj-unit-test-path "test/unit")
        (gral:prj-packages '("prj1" "prj2"))
        (gral:prj-integration-test-path "test/integration"))
    (mocklet ((file-truename => "/home/gralamin/project/test/integration/prj1/test_foo.py")
              (projectile-project-root => "/home/gralamin/project/"))
      (should (equal (__gral:switch-unit-test "test_foo.py")
                     "/home/gralamin/project/test/unit/prj1/test_foo.py"))))
)


(provide 'test-gral-project)
