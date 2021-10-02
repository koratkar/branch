;;; If ODD : 3x + 1
;;; If EVEN: x / 2

(defun collatz (x)
  (cond ((= x 1) (print x))
        ((oddp x) (progn
                     (print x)
                     (collatz (+ (* 3 x) 1))))
        ((evenp x) (progn
                     (print x)
                     (collatz (/ x 2))))))
(collatz 7)
