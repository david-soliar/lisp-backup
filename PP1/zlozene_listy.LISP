(defun a ()
  (let ((a (cons 4 2))
        (b (cons nil 5)))
    (cons a (cons (cons t b) b))))

(defun b ()
  (let ((a (cons t nil)))
    (cons (cons (cons 2 a) (cons 3 a)) 4)))

(defun c ()
  (let ((a (cons 3 4)))
    (cons 1 (cons a (cons a 2)))))
