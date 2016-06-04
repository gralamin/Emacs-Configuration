;; Contains ERT tests for the path library

;; Local Variables:
;; gral:prj-packages: nil
;; gral:prj-unit-test-path: "test/unit"
;; gral:prj-test-format: "test_%s"
;; gral:prj-code-path: nil
;; gral:prj-unit-test-runner: "nosetests"
;; gral:prj-current-unit-test-remove-file: nil
;; gral:prj-test-reverse: (lambda (file) (car (last (split-string file "test_") 1)))
;; gral:prj-build-test-command: (lambda (args) (concat "cd " (projectile-project-root) " && " (gral:unit-test-runner) " " args " &"))
;; End:

(require 'el-mock)

(ert-deftest test:gral:path-join ()
  "Tests gral:path-join"
  (should (equal (gral:path-join "tmp" "foo" "bar") "tmp/foo/bar"))
)

(ert-deftest test:gral:path-absolute ()
  "Tests gral:path-absolute"
  (mocklet ((projectile-project-root => "/home/gralamin/project/"))
    (should (equal (gral:path-project-absolute "tmp/foo/bar")
                   "/home/gralamin/project/tmp/foo/bar"))
    (should (equal (gral:path-project-absolute "some/path")
                   "/home/gralamin/project/some/path")))
  (mocklet ((projectile-project-root => nil))
    (should (equal (gral:path-project-absolute "tmp/foo/bar")
                   "tmp/foo/bar"))
    (should (equal (gral:path-project-absolute "some/path")
                   "some/path")))
)

(ert-deftest test:gral:get-file-name ()
  "Tests gral:get-file-name"
  (should (equal (gral:get-file-name "/tmp/foo.py") "foo.py"))
)

(ert-deftest test:gral:get-package-prj-packages ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages nil))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-package "some/path/foo.py")
                     "/home/gralamin/project/some/path"))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages nil))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-package "some/path/foo.py")
                     "some/path"))))
)

(ert-deftest test:gral:get-package-prj-packages-first ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-package "prj1/foo.py") "prj1"))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root nil))
      (should (equal (gral:get-package "prj1/foo.py") "prj1"))))
)

(ert-deftest test:gral:get-package-prj-packages-second ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-package "prj2/foo.py") "prj2"))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-package "prj2/foo.py") "prj2"))))
)

(ert-deftest test:gral:get-package-prj-packages-no-match ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-package "prj3/foo.py") nil))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-package "prj3/foo.py") nil))))
)

(ert-deftest test:gral:get-package-test-prj-packages ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages nil))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-test-package "unit/foo.py" "unit")
                     "/home/gralamin/project/some/path"))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages nil))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-test-package "unit/foo.py" "unit")
                     "some/path"))))
)

(ert-deftest test:gral:get-package-test-prj-packages-first ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-test-package "unit/prj1/foo.py" "unit") "prj1"))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-test-package "unit/prj1/foo.py" "unit") "prj1"))))
)

(ert-deftest test:gral:get-package-test-prj-packages-second ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-test-package "unit/prj2/foo.py" "unit") "prj2"))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-test-package "unit/prj2/foo.py" "unit") "prj2"))))
)

(ert-deftest test:gral:get-package-test-prj-packages-no-match ()
  "Tests gral:get-package"
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-test-package "unit/prj3/foo.py" "unit") nil))))
  (let ((gral:prj-code-path "some/path")
        (gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-test-package "unit/prj3/foo.py" "unit") nil))))
)

(ert-deftest test:gral:get-test-path-for-package ()
  "Tests gral:get-test-path-for-package"
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-test-path-for-package "prj1" "unit")
                     "/home/gralamin/project/unit/prj1/"))
      (should (equal (gral:get-test-path-for-package "prj2" "unit")
                     "/home/gralamin/project/unit/prj2/"))))
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-test-path-for-package "prj1" "unit")
                     "unit/prj1/"))
      (should (equal (gral:get-test-path-for-package "prj2" "unit")
                     "unit/prj2/"))))
)

(ert-deftest test:gral:get-test-path-for-package-non-package ()
  "Tests gral:get-test-path-for-package"
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:get-test-path-for-package "prj3" "unit")
                     "/home/gralamin/project/unit/"))))
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:get-test-path-for-package "prj3" "unit")
                     "unit/"))))
)

(ert-deftest test:gral:get-test-file-name ()
  "Tests gral:get-test-file-name"
  (should (equal (gral:get-test-file-name "foo.py") "test_foo.py"))
  (should (equal (gral:get-test-file-name "/bar/foo.py") "test_foo.py"))
)

(ert-deftest test:gral:get-code-file-name ()
  "Tests gral:get-test-file-name"
  (should (equal (gral:get-code-file-name "test_foo.py") "foo.py"))
  (should (equal (gral:get-code-file-name "/bar/test_foo.py") "foo.py"))
)

(ert-deftest test:gral:file-to-test ()
  "Tests gral:file-to-test"
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:file-to-test "prj1/foo.py" "test/unit")
                     "/home/gralamin/project/test/unit/prj1/test_foo.py"))
      (should (equal (gral:file-to-test "prj2/foo.py" "test/unit")
                     "/home/gralamin/project/test/unit/prj2/test_foo.py"))))
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:file-to-test "prj1/foo.py" "test/unit")
                     "test/unit/prj1/test_foo.py"))
      (should (equal (gral:file-to-test "prj2/foo.py" "test/unit")
                     "test/unit/prj2/test_foo.py"))))
)

(ert-deftest test:gral:file-to-test-unknown-package ()
  "Tests gral:file-to-test"
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:file-to-test "prj3/foo.py" "test/unit")
                     "/home/gralamin/project/test/unit/prj3/test_foo.py"))))
)

(ert-deftest test:gral:test-to-file ()
  "Tests gral:file-to-test"
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:test-to-file "test/unit/prj1/test_foo.py" "test/unit")
                     "/home/gralamin/project/prj1/foo.py"))
      (should (equal (gral:test-to-file "test/unit/prj2/test_foo.py" "test/unit")
                     "/home/gralamin/project/prj2/foo.py"))))
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => nil))
      (should (equal (gral:test-to-file "test/unit/prj1/test_foo.py" "test/unit")
                     "prj1/foo.py"))
      (should (equal (gral:test-to-file "test/unit/prj2/test_foo.py" "test/unit")
                     "prj2/foo.py"))))
)

(ert-deftest test:gral:test-to-file-unknown-package ()
  "Tests gral:file-to-test"
  (let ((gral:prj-packages '("prj1" "prj2")))
    (mocklet ((projectile-project-root => "/home/gralamin/project/"))
      (should (equal (gral:test-to-file "test/unit/prj3/test_foo.py" "test/unit")
                     "/home/gralamin/project/prj3/foo.py"))))
)

(provide 'test-gral-path)
