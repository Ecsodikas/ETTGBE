(in-package #:ecsodikas.ettgbe)

(defun nop (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    new-cpu))


                                        ; ADD
(defun add (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n NIL
                   :h ho
                   :c o)))
    new-cpu))

(defun adc (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (multiple-value-bind (r o ho) (overflowing-add va vb (if (flags-carry (cpu-flags cpu)) 1 0))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= r 0)
                   :n NIL
                   :h ho
                   :c o)))
    new-cpu))

(defun add-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (multiple-value-bind (r o ho) (overflowing-add va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h ho
                   :c o)))
    new-cpu))

(defun adc-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (multiple-value-bind (r o ho) (overflowing-add va vb (if (flags-carry (cpu-flags cpu)) 1 0))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h ho
                   :c o)))
    new-cpu))
                                        ; SUB

(defun sub (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (multiple-value-bind (r o ho) (underflowing-sub va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n T
                   :h ho
                   :c o)))
    new-cpu))

(defun sbc (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (multiple-value-bind (r o ho) (underflowing-sub va vb (if (flags-carry (cpu-flags cpu)) 1 0))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n T
                   :h ho
                   :c o)))
    new-cpu))

(defun sub-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (multiple-value-bind (r o ho) (underflowing-sub va vb)
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n T
                   :h ho
                   :c o)))
    new-cpu))

(defun sbc-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (multiple-value-bind (r o ho) (underflowing-sub va vb (if (flags-carry (cpu-flags cpu)) 1 0))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n T
                   :h ho
                   :c o)
        ))
    new-cpu))

                                        ; AND

(defun andd (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (let ((r (logand va vb)))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h T
                   :c NIL)))
    new-cpu))


(defun andd-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (let ((r (logand va vb)))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h T
                   :c NIL)))
    new-cpu))

                                        ; XOR

(defun xorr (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (let ((r (logxor va vb)))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h NIL
                   :c NIL)))
    new-cpu))

(defun xorr-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (let ((r (logxor va vb)))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h NIL
                   :c NIL)))
    new-cpu))

                                        ;OR

(defun orr (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (let ((r (logior va vb)))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h NIL
                   :c NIL)))
    new-cpu))


(defun orr-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (let ((r (logior va vb)))
        (set-register new-cpu :a r)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n NIL
                   :h NIL
                   :c NIL)))
    new-cpu))

                                        ;CP
(defun sub (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (load-register new-cpu r)))
      (multiple-value-bind (r o ho) (underflowing-sub va vb)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n T
                   :h ho
                   :c o)))
    new-cpu))

(defun sub-hl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu :a))
          (vb (read-memory new-cpu (load-register new-cpu :hl))))
      (multiple-value-bind (r o ho) (underflowing-sub va vb)
        (set-flags new-cpu
                   :z (= (logand r #xFF) 0)
                   :n T
                   :h ho
                   :c o)))
    new-cpu))
