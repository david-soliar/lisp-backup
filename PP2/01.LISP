;3
(defun my-string-append (&rest strs)
  (if (null strs)
      ""
    (format nil "~A~A" (car strs) (apply (function my-string-append) (cdr strs)))))

;4
(defun my-car (c)
  (if (consp c)
      (car c)
    (error "~A nie je par" c)))

(defun my-cdr (c)
  (if (consp c)
      (cdr c)
    (error "~A nie je par" c)))

;5
(defun numbers-count-f (&rest n)
  (if (null n)
      0
    (+ (car n) (apply (function numbers-count-f) (cdr n)))))

(defun numbers-count-i (&rest n)
  (setf x 0)
  (dolist (i n)
    (setf x (+ x i)))
  x)

;6
(defun my-count-f (c str)
  (labels ((iter (i)
             (cond ((eql i (length str)) 0)
                   ((eql c (aref str i)) (+ 1 (iter (1+ i))))
                   (t (iter (1+ i))))))
    (iter 0)))

(defun my-count-i (c str)
  (setf x 0)
  (dotimes (i (length str))
    (if (eql c (aref str i))
        (setf x (1+ x))
      nil))
  x)

;7
(defun count-all (str)
  (labels ((not-in-list (c l)
             (cond ((null l) t)
                   ((eql c (caar l)) nil)
                   (t (not-in-list c (cdr l)))))
           (iter (l i)
             (cond ((= i (length str)) l)
                   ((not-in-list (aref str i) l)
                    (iter (append (list (cons (aref str i) (my-count-f (aref str i) str))) l) 
                          (1+ i)))
                   (t (iter l (1+ i))))))
    (iter (list) 0)))

;8
(defun my-reverse (list)
  (setf new-list nil)
  (dolist (x list)
    (setf new-list (cons x new-list)))
  new-list)

;9
(defun foldr (fun list init)
  (if (null list)
      init
    (funcall fun (car list)
             (foldr fun (cdr list) init))))

(defun my-append-f (l1 l2)
  (foldr (quote cons) l1 l2))

(defun my-append-i (l1 l2)
  (setf new-list nil)
  (dolist (x l1)
    (setf new-list (cons x new-list)))
  (dolist (x l2)
    (setf new-list (cons x new-list)))
  (my-reverse new-list))

(defun my-mapcar-f (fun l)
  (if (null l)
      nil
    (cons (funcall fun (car l)) (my-mapcar-f fun (cdr l)))))

(defun my-mapcar-i (fun l)
  (setf new-list nil)
  (dolist (x l)
    (setf new-list (cons (funcall fun x) new-list)))
  new-list)

;10
(defun triangle-row (i n char right)
  (cond ((and (= i 0) (= n 0)) (format t "~%"))
        ((and right (> n 0)) (progn (format t " ") (triangle-row i (1- n) char right)))
        ((and right (> i 0)) (progn (format t "~A" char) (triangle-row (1- i) n char right)))
        ((> i 0) (progn (format t "~A" char) (triangle-row (1- i) n char right)))
        ((> n 0) (progn (format t " ") (triangle-row i (1- n) char right)))))

(defun triangle (n &key (char #\*) align-right-p align-top-p)
  (dotimes (i (1+ n))
    (if align-top-p
        (triangle-row (- n i) i char align-right-p)
      (triangle-row i (- n i) char align-right-p))))
