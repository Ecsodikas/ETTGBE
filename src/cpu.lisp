(in-package #:ecsodikas.ettgbe)

(defstruct registers
  (a #b00000000 :type unsigned-byte)
  (b #b00000000 :type unsigned-byte)
  (c #b00000000 :type unsigned-byte)
  (d #b00000000 :type unsigned-byte)
  (e #b00000000 :type unsigned-byte)
  (h #b00000000 :type unsigned-byte)
  (l #b00000000 :type unsigned-byte))

(defstruct flags
  (zero)
  (subtraction)
  (half-carry)
  (carry))

(defun set-flags (cpu &key (z nil z-p) (n nil n-p) (h nil h-p) (c nil c-p))
  (let ((new-cpu (copy-cpu cpu)))
    (when z-p
      (setf (flags-zero (cpu-flags new-cpu)) z))
    (when n-p
      (setf (flags-subtraction (cpu-flags new-cpu)) n))
    (when h-p
      (setf (flags-half-carry (cpu-flags new-cpu)) h))
    (when c-p
      (setf (flags-carry (cpu-flags new-cpu)) c))
    new-cpu))

(defun flags->byte (flags)
  (logior
   (ash (if (flags-zero flags) 1 0) 7)
   (ash (if (flags-subtraction flags) 1 0) 6)
   (ash (if (flags-half-carry flags) 1 0) 5)
   (ash (if (flags-carry flags) 1 0) 4)))

(defun byte->flags (byte)
  (make-flags
   :zero (= (logand (ash byte -7) #b00000001) 1)
   :subtraction (= (logand (ash byte -6) #b00000001) 1)
   :half-carry (= (logand (ash byte -5) #b00000001) 1)
   :carry (= (logand (ash byte -4) #b00000001) 1)))

(defun read-memory (cpu short)
  (aref (cpu-memory cpu) short))

(defstruct cpu
  (memory (make-array '(#xFFFF)))
  (program-counter #b00000000)
  (stack-counter #b00000000)
  (flags (make-flags))
  (registers (make-registers)))

(defun execute-instruction (cpu byte)
  (funcall (cdr (assoc byte *instruction-db*)) cpu))

(defun load-16-register (cpu r1 r2)
  (let ((r1 (load-register cpu r1))
        (r2 (load-register cpu r2)))
    (logior (ash r1 8) r2)))

(defun set-16-register (cpu r1 r2 short)
  (let ((h (logand short #xFF00))
        (l (logand short #x00FF)))
    (set-register cpu r1 h)
    (set-register cpu r2 l)))

(defun set-register (cpu r value)
  (cond
;;; 8 bit
    ((eq r :a) (setf (registers-a (cpu-registers cpu)) value))
    ((eq r :b) (setf (registers-b (cpu-registers cpu)) value))
    ((eq r :c) (setf (registers-c (cpu-registers cpu)) value))
    ((eq r :d) (setf (registers-d (cpu-registers cpu)) value))
    ((eq r :e) (setf (registers-e (cpu-registers cpu)) value))
    ((eq r :f) (setf (cpu-flags cpu) (byte->flags value)))
    ((eq r :h) (setf (registers-h (cpu-registers cpu)) value))
    ((eq r :l) (setf (registers-l (cpu-registers cpu)) value))
;;; 16 bit
    ((eq r :af) (set-16-register cpu :a :f value))
    ((eq r :bc) (set-16-register cpu :b :c value))
    ((eq r :de) (set-16-register cpu :d :e value))
    ((eq r :hl) (set-16-register cpu :h :l value))
    ((eq r :sp) (setf (registers-h (cpu-registers cpu)) value))
    ((eq r :pc) (setf (registers-l (cpu-registers cpu)) value))))

(defun load-register (cpu r)
  (cond
;;; 8 bit
    ((eq r :a) (registers-a (cpu-registers cpu)))
    ((eq r :b) (registers-b (cpu-registers cpu)))
    ((eq r :c) (registers-c (cpu-registers cpu)))
    ((eq r :d) (registers-d (cpu-registers cpu)))
    ((eq r :e) (registers-e (cpu-registers cpu)))
    ((eq r :f) (flags->byte (cpu-flags cpu)))
    ((eq r :h) (registers-h (cpu-registers cpu)))
    ((eq r :l) (registers-l (cpu-registers cpu)))
;;; 16 bit
    ((eq r :af) (load-16-register cpu :a :f))
    ((eq r :bc) (load-16-register cpu :b :c))
    ((eq r :de) (load-16-register cpu :d :e))
    ((eq r :hl) (load-16-register cpu :h :l))
    ((eq r :sp) (cpu-stack-counter cpu))
    ((eq r :pc) (cpu-program-counter cpu))))
