(in-package #:ecsodikas.ettgbe)

(defun half-overflow-p (a b)
  (let ((la (logand a #x0F))
        (lb (logand b #x0F)))
    (> (+ la lb) #x0F)))

(defun half-underflow-p (a b)
  (let ((la (logand a #x0F))
        (lb (logand b #x0F)))
    (< (- la lb) 0)))

(defun overflowing-add (a b &optional (c 0) (m #xFF))
  (let ((r (+ a b c)))
    (values
     (mod r m)
     (or (> a r) (> b r))
     (half-overflow-p a b))))

(defun underflowing-sub (a b &optional (c 0) (m #xFF))
  (let ((r (- a b c)))
    (values
     (mod r m)
     (< r 0)
     (half-underflow-p a b))))

(defun clear-bit (num n)
  (logand num (lognot (ash 1 n))))

(defun set-bit (num n)
  (logior num (ash 1 n)))
