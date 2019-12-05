(defsystem cl-flyway
  :description "Data migration tool like Flyway for Common Lisp."
  :author "YUE Daian"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:cl-dbi :sxql)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "cl-flyway"))))
  :in-order-to ((test-op (test-op "cl-flyway-tests"))))

(defsystem cl-flyway-tests
  :description "Tests of cl-flyway system"
  :author "YUE Daian"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:cl-flyway :rove)
  :components ((:module "tests"
                :components
                ((:file "packages"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
