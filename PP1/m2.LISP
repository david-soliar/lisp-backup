(defun sum-digits (n)
  (if (= n 0)
      0
    (let ((x (floor (/ n 10))))
      (+ (- n (* 10 x)) (sum-digits x)))))

(defun even-elements (li)
  (e-iter li 0))

(defun e-iter (li n)
  (cond ((null li) nil)
        ((evenp n) (cons (car li) (e-iter (cdr li) (1+ n))))
        (t (e-iter (cdr li) (1+ n)))))

(defun mem (seq i)
  (funcall seq i))

(defun s (l1 l2)
   (lambda (n) (if (evenp n)
                   (mem l1 (/ n 2))
                 (mem l2 (/ (1- n) 2)))))

(defun sortedp (li)
  (cond ((null li) t)
        ((null (cdr li)) t)
        ((<= (car li) (cadr li)) (sortedp (cdr li)))
        (t nil)))

(defun seq-shift (seq k)
  (lambda (n) (funcall seq (+ n k))))

(defun c-iter (list n)
  (if (= n 0)
      0
    (+ (* (car list) (expt 10 (1- n)))
       (c-iter (cdr list) (1- n)))))

(defun c (list)
  (c-iter list (length list)))

(defun my-mapcar (fun list)
  (if (null list)
      nil
    (cons (funcall fun (car list))
          (my-mapcar fun (cdr list)))))

(defun seq-to-list (seq k)
  (labels ((iter (i)
             (if (= i k)
                 nil
               (cons (mem seq i) (iter (1+ i))))))
    (iter 0)))
