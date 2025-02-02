(in-package #:ecsodikas.ettgbe-test)

(def-suite* cpu
  :in main-suite)

(test registers
  "Tests register manipulation of the cpu."
  (is (= 69 69)))
