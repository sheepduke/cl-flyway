(in-package cl-flyway-tests)

(deftest test-all
  (uiop:with-temporary-file (:pathname filename)
    (mito:connect-toplevel :sqlite3 :database-name filename)
    (let ((action-map (flyway:make-action-map))
          (var-1 1)
          (var-2 2))
      (testing "Migration works properly at the first time."
        (flyway:defaction action-map "1.0"
            "Migration 1.0"
          (setf var-1 100))
        (flyway:setup action-map)
        (ok (= var-1 100)
            "The migration function has been called."))

      (testing "Migration works in normal case."
        (flyway:defaction action-map "1.1"
            "Migration 1.1"
          (setf var-1 200))
        (flyway:defaction action-map "1.2"
            "Migration 1.2"
          (setf var-2 100))
        (flyway:setup action-map)
        (ok (= var-1 200)
            "Function of version 1.1 has been called.")
        (ok (= var-2 100)
            "Function of version 1.2 has been called."))

      (testing "Migration works in corner cases."
        (flyway:defaction action-map "0.9"
            "New migration 0.9"
          (setf var-1 1000))
        (flyway:defaction action-map "1.1"
            "New migration 1.1"
          (setf var-2 1000))
        (ok (= var-1 200)
            "Migration function of smaller version is not called.")
        (ok (= var-2 100)
            "Migration function of equal version is not called.")))
    (mito:disconnect-toplevel)))
