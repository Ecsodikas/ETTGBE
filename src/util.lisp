(in-package #:ecsodikas.ettgbe)

(defun half-overflow-p (a b)
  (let ((la (logand a #x0F))
        (lb (logand b #x0F)))
    (> (+ la lb) #x0F)))

(defun half-underflow-p (a b)
  (let ((la (logand a #x0F))
        (lb (logand b #x0F)))
    (< (- la lb) 0)))

(defun overflowing-add (a b &optional (c 0))
  (let ((r (+ a b c)))
    (values
     (mod r #b11111111)
     (or (> a r) (> b r))
     (half-overflow-p a b))))

(defun underflowing-sub (a b &optional (c 0))
  (let ((r (- a b c)))
    (values
     (mod r #b11111111)
     (< r 0)
     (half-underflow-p a b))))
