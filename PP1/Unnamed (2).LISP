;2

#|
(defun s-iter (a n)
  (let ((b (* 10 (floor (/ a 10)))))
    (if (= n 0)
        0
      (+ (- a b)
         (s-iter (/ b 10) (1- n))))))

(defun sum-digits (a)
  (s-iter a (round (1+ (log a 10)))))
|#

(defun sum-digits (a)
  (if (= a 0)
      0
   (+ (rem a 10)
     (sum-digits (floor (/ a 10))))))

;3
(let ((x (cons 'b 'c)))
  (cons x (cons x 'a)))

;4
(defun even-elements (l)
  (cond ((null l)
         nil)
        ((null (cdr l))
         (cons (car l) nil))
        (t 
         (cons (car l)
               (even-elements (cddr l))))))

;5
(defun in-list (x l)
  (cond ((null l)
         nil)
        ((= x (car l))
         t)
        (t 
         (in-list x (cdr l)))))

(defun my-set-intersection (a b)
  (cond ((or (null a) (null b))
         nil)
        ((in-list (car a) b)
         (cons (car a) (my-set-intersection (cdr a) b)))
        (t
         (my-set-intersection (cdr a) b))))

(defun foldr (fun list init)
  (if (null list)
      init
    (funcall fun 
             (car list) 
             (foldr fun (cdr list) init))))

;my-reverse
(defun my-reverse (l)
  (funcall (revx) l nil))

(defun revx ()
  (labels ((rev (l ir)
             (if (null l)
                 ir
               (rev (cdr l) (cons (car l) ir)))))
    #'rev))

;7
(defun seq-to-list (seq n)
  (if (= n 0)
      nil
    (cons (funcall seq (- 20 n)) (seq-to-list seq (1- n)))))

(defun seq-interleave (a b)
  (lambda (n)
    (if (evenp n)
        (funcall a (/ n 2))
      (funcall b (/ (1- n) 2)))))
