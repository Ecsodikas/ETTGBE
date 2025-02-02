(in-package #:ecsodikas.ettgbe)

(defvar *instruction-db* '())

(defmacro definstruction (name byte params &body body)
  `(progn
     (pushnew ',(cons name byte) *instruction-db* :test #'equal)
     (defun ,name ,params
       ,@body)))

(defun half-overflow-p (a b)
  (let ((la (logand a #x0F))
        (lb (logand b #x0F)))
    (> (+ la lb) #x0F)))

(defun overflowing-add (a b)
  (let ((r (+ a b)))
    (values
     (mod r #b11111111)
     (or (> a r) (> b r))
     (half-overflow-p a b))))


;;; #x00
(definstruction nop #x00 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    new-cpu))


                                        ; ADD
(definstruction add-a #x87 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu :ba)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n nil
                   :h ho
                   :c o)))
    new-cpu))

(definstruction add-b #x80 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu :b)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n nil
                   :h ho
                   :c o)))
    new-cpu))

(definstruction add-c #x81 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu :c)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n nil
                   :h ho
                   :c o)))
    new-cpu))

(definstruction add-d #x82 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu :d)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n nil
                   :h ho
                   :c o)))
    new-cpu))

(definstruction add-e #x83 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu :e)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n nil
                   :h ho
                   :c o)))
    new-cpu))

(definstruction add-h #x84 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu :h)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n nil
                   :h ho
                   :c o)))
    new-cpu))

(definstruction add-l #x85 (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu :l)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n nil
                   :h ho
                   :c o)))
    new-cpu))
