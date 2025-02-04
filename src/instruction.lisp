(in-package #:ecsodikas.ettgbe)
                                        ; NOP
(defun nop (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu) 1)
    new-cpu))

                                        ; HALT
(defun halt (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (setf (cpu-is-halted new-cpu) T)
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
(defun cp (cpu r)
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

(defun cp-hl (cpu)
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

                                        ; INC

(defun inc (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu r))
          (vb 1))
      (multiple-value-bind (res o ho) (overflowing-add va vb)
        (declare (ignore o))
        (set-register new-cpu r res)
        (set-flags new-cpu
                   :z (= res 0)
                   :n NIL
                   :h ho)))
    new-cpu))


(defun inc16 (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu r))
          (vb 1))
      (multiple-value-bind (res o ho) (overflowing-add va vb 0 #xFFFF)
        (declare (ignore o))
        (set-register new-cpu r res)
        (set-flags new-cpu
                   :z (= res 0)
                   :n NIL
                   :h ho)))
    new-cpu))
                                        ; DEC

(defun dec (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu r))
          (vb 1))
      (multiple-value-bind (res o ho) (underflowing-sub va vb)
        (declare (ignore o))
        (set-register new-cpu r res)
        (set-flags new-cpu
                   :z (= res 0)
                   :n T
                   :h ho)))
    new-cpu))


(defun dec16 (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((va (load-register new-cpu r))
          (vb 1))
      (multiple-value-bind (res o ho) (underflowing-sub va vb 0 #xFFFF)
        (declare (ignore o))
        (set-register new-cpu r res)
        (set-flags new-cpu
                   :z (= res 0)
                   :n T
                   :h ho)))
    new-cpu))

                                        ; CCF
(defun ccf (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (set-flags new-cpu
               :c (not (flags-carry (cpu-flags new-cpu))))
    new-cpu))

                                        ; SCF
(defun scf (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (set-flags new-cpu
               :c T)
    new-cpu))

                                        ; RR

(defun rr (cpu reg)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let* ((r (load-register cpu reg))
           (f (if (flags-carry (cpu-flags new-cpu)) 1 0))
           (res (+
                 (ash r -1)
                 (ash (if (not (= (logand f (ash 1 7)) 0)) 1 0) 7)
                 (ash (logand r 1) 8))))
      (set-flags new-cpu
                 :z NIL
                 :n NIL
                 :h NIL
                 :c (> res #xFF))
      (set-register new-cpu reg (logand #xFF res)))
    new-cpu))

(defun sra (cpu reg)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let* ((r (load-register cpu reg))
           (sign (logand #b10000000 r))
           (f (if (flags-carry (cpu-flags new-cpu)) 1 0))
           (res (+
                 (ash r -1)
                 (ash (if (not (= (logand f (ash 1 7)) 0)) 1 0) 7)
                 (ash (logand r 1) 8))))
      (set-flags new-cpu
                 :z NIL
                 :n NIL
                 :h NIL
                 :c (> res #xFF))
      (set-register new-cpu reg (logior sign (logand #xFF res))))
    new-cpu))

                                        ; RL
(defun rl (cpu reg)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu 2))
    (let* ((r (load-register cpu reg))
           (f (if (flags-carry (cpu-flags new-cpu)) 1 0))
           (res (+
                 (ash r -1)
                 (if (not (= (logand f (ash 1 7)) 0)) 1 0))))
      (set-flags new-cpu
                 :z NIL
                 :n NIL
                 :h NIL
                 :c (> res #xFF))
      (set-register new-cpu reg (logand #xFF res)))
    new-cpu))


(defun sla (cpu reg)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu) 2)
    (let* ((r (load-register cpu reg))
           (sign (logior #b10000000 r))
           (f (if (flags-carry (cpu-flags new-cpu)) 1 0))
           (res (+
                 (ash r -1)
                 (if (not (= (logand f (ash 1 7)) 0)) 1 0))))
      (set-flags new-cpu
                 :z NIL
                 :n NIL
                 :h NIL
                 :c (> res #xFF))
      (set-register new-cpu reg (logior sign (logand #xFF res))))
    new-cpu))

                                        ; RRC

(defun rrc (cpu reg)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let* ((r (load-register cpu reg))
           (res (+
                 (ash r -1)
                 (ash (logand r 1) 7)
                 (ash (logand r 1) 8))))
      (set-flags new-cpu
                 :z (zerop res)
                 :n NIL
                 :h NIL
                 :c (> res #xFF))
      (set-register new-cpu reg (logand #xFF res)))
    new-cpu))


                                        ; RLC
(defun rlc (cpu reg)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let* ((r (load-register cpu reg))
           (res (+
                 (ash r -1)
                 (ash (logand r 1) 7))))
      (set-flags new-cpu
                 :z (zerop res)
                 :n NIL
                 :h NIL
                 :c (> res #xFF))
      (set-register new-cpu reg (logand #xFF res)))
    new-cpu))

                                        ; CPL
(defun cpl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu))
    (let ((r (load-register cpu :a)))
      (set-register cpu :a (logxor r)))
    (set-flags new-cpu
               :n 1
               :h 1)
    new-cpu))

                                        ; BIT
(defun bitt (cpu r p)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu 2))
    (let ((r (load-register cpu r)))
      (set-flags new-cpu
                 :z (= (logand (ash r (- p 1)) 1) 1)
                 :n 0
                 :h 1))
    new-cpu))


                                        ; RESET
(defun reset (cpu r p)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu 2))
    (let ((r (load-register cpu r)))
      (set-register cpu r (clear-bit r p)))
    new-cpu))

                                        ; SET
(defun sett (cpu r p)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu 2))
    (let ((r (load-register cpu r)))
      (set-register cpu r (set-bit r p)))
    new-cpu))

                                        ; SWAP
(defun swap (cpu r)
  (let ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter cpu 2))
    (let* ((rv (load-register cpu r))
           (ln (logand rv #x0F))
           (un (logand rv #xF0)))
      (set-register cpu r (logior (ash ln 4) (ash un -4)))
      (set-flags new-cpu
                 :z (zerop rv)
                 :n NIL
                 :h NIL
                 :c NIL))
    new-cpu))

                                        ; LD

(defun ld-8-reg (cpu target source)
  (let* ((new-cpu (copy-cpu cpu))
         (val (load-register new-cpu source)))
    (incf (cpu-program-counter new-cpu))
    (set-register cpu target val)))

(defun ld-8-d (cpu target value)
  (let* ((new-cpu (copy-cpu cpu)))
    (incf (cpu-program-counter new-cpu))
    (set-register cpu target value)))

(defun ld-16-hl (cpu target)
  (let* ((new-cpu (copy-cpu cpu))
         (val (load-register new-cpu :hl)))
    (incf (cpu-program-counter new-cpu))
    (set-register cpu target val)))

                                        ; JP
                                        ; TODO: Little endianski?

(defun jp-if-zero (cpu addr)
  (let ((new-cpu (copy-cpu cpu)))
    (if (flags-zero (cpu-flags new-cpu))
        (progn
          (setf (cpu-program-counter new-cpu) addr)
          new-cpu)
        (progn
          (incf (cpu-program-counter new-cpu) 3)
          new-cpu))))


(defun jp-if-not-zero (cpu addr)
  (let ((new-cpu (copy-cpu cpu)))
    (if (not (flags-zero (cpu-flags new-cpu)))
        (progn
          (setf (cpu-program-counter new-cpu) addr)
          new-cpu)
        (progn
          (incf (cpu-program-counter new-cpu) 3)
          new-cpu))))

(defun jr-if-zero (cpu offset)
  (let ((new-cpu (copy-cpu cpu)))
    (if (flags-zero (cpu-flags new-cpu))
        (progn
          (incf (cpu-program-counter new-cpu) offset)
          new-cpu)
        (progn
          (incf (cpu-program-counter new-cpu) 3)
          new-cpu))))

(defun jr-if-not-zero (cpu offset)
  (let ((new-cpu (copy-cpu cpu)))
    (if (not (flags-zero (cpu-flags new-cpu)))
        (progn
          (incf (cpu-program-counter new-cpu) offset)
          new-cpu)
        (progn
          (incf (cpu-program-counter new-cpu) 3)
          new-cpu))))

(defun jhl (cpu)
  (let ((new-cpu (copy-cpu cpu)))
    (setf (cpu-program-counter new-cpu) (load-register new-cpu :hl))))

                                        ; STACK
(defun pushh (cpu register)
  (let* ((new-cpu (copy-cpu cpu))
         (val (load-register new-cpu register))
         (msb (ash (logand #xFF00 val) -8))
         (lsb (logand #x00FF val)))
    (decf (cpu-stack-counter new-cpu))
    (write-memory new-cpu (cpu-stack-counter new-cpu) msb)
    (decf (cpu-stack-counter new-cpu))
    (write-memory new-cpu (cpu-stack-counter new-cpu) lsb)
    new-cpu))

(defun popp (cpu register)
  (let* ((new-cpu (copy-cpu cpu))
         (lsb (read-memory new-cpu (cpu-stack-counter new-cpu)))
         (msb (read-memory new-cpu (1+ (cpu-stack-counter new-cpu))))
         (res (logior (ash msb 8) lsb)))
    (incf (cpu-stack-counter new-cpu) 2)
    (set-register new-cpu register res)
    new-cpu))
