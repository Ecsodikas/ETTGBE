;;;; ettgbe.asd

(asdf:defsystem #:ettgbe
  :description "Ecsodikas' Time Traveling Game Boy Emulator."
  :author "Timo Netzer <ecsodikas@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :serapeum)
  :in-order-to ((asdf:test-op (asdf:test-op :ettgbe/test)))
  :components ((:file "src/package")
               (:file "src/ettgbe")
               (:file "src/cpu")
               (:file "src/instruction")))

(asdf:defsystem #:ettgbe/test
  :description "Tests for Ecsodikas' Time Traveling Game Boy Emulator."
  :author "Timo Netzer <ecsodikas@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :fiveam :run-all-tests))
  :depends-on (:ettgbe
               :fiveam)
  :components ((:file "t/package")
               (:file "t/main")
               (:file "t/cpu")))
