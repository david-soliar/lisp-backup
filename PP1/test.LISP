;pisomka

;1
(defun funkcia ()
  (function funkcia))

;2
(defun tri (n)
  (if (= n 0)
      nil
    (im:split (im:scaled (im:segment) 0.0001)
              (im:rotated (im:chain (im:segment) 
                              (im:scaled (tri (1- n)) 0.5))
                       (/ pi 6))
              (im:rotated (im:chain (im:segment) 
                              (im:scaled (tri (1- n)) 0.5))
                       (/ pi -6))
              (im:chain (im:segment)
                     (im:scaled (tri (1- n)) 0.5)))))

;3
(defun foldr (fun list init)
  (if (null list)
      init
    (funcall fun (car list) (foldr fun (cdr list) init))))

(defun append-2 (list1 list2)
  (foldr (function cons) list1 list2))

;4
;(cdr (car (cdr (car '((a ((b) c)) ((d (e)) f))))))
;(c)
;nezabudat na nil na konci ked beriem cdr z listu bruh

;5
(defun pascalxy (x y)
  (cond ((= y 0) 1)
        ((= y x) 1)
        (t (+ (pascalxy (1- x) (1- y))
              (pascalxy (1- x) y)))))

(defun pascal (r)
  (labels ((p (c)
             (if (= c r)
                 (cons 1 nil)
               (cons (pascalxy r c)
                     (p (1+ c))))))
    (p 0)))

;6
(defun sortedp (list)
  (cond ((null (cdr list)) t)
        ((<= (car list) (cadr list)) (sortedp (cdr list)))
        (t nil)))

;7
(defun seq-shift (seq k)
  (lambda (n) (if (< (- n k) 0)
                  0
                (funcall seq (- n k)))))

(defun seq-to-list (seq n)
  (labels ((s (i)
             (if (= i n)
                 nil
               (cons (funcall seq i)
                     (s (1+ i))))))
    (s 0)))

(defun my-ident (n)
  n)

(defun power2 (n)
  (* n n))

(defun node-value (root)
  (car root))

(defun node-children (root)
  (cdr root))

;8
(defun tree-find (value root)
  (if (eql (node-value root) value)
      root
    (tree-list-find value (node-children root))))
(defun tree-list-find (value roots)
  (if (null roots)
      nil
    (let ((res (tree-find value (car roots))))
      (if (null res)
          (tree-list-find value (cdr roots))
        res))))

(defun tree-node-path (value root)
  (if (eql (node-value root) value)
      (list root)
    (let ((subpath (tree-list-node-path
                    value
                    (node-children root))))
      (if (null subpath)
          nil
        (cons root subpath)))))

(defun tree-list-node-path (value roots)
  (if (null roots)
      nil
    (let ((subpath (tree-node-path value (car roots))))
      (if (null subpath)
          (tree-list-node-path value (cdr roots))
        subpath))))

(defun tree-cesta (value tree)
  (if (eql value (node-value tree))
      (list tree)
    (let ((sub (tree-cesta-children value (node-children tree))))
      (if (null sub)
          nil
        (cons tree sub)))))

(defun tree-cesta-children (value trees)
  (if (null trees)
      nil
    (let ((sub (tree-cesta value (car trees))))
      (if (null sub)
          (tree-cesta-children value (cdr trees))
        sub))))

(defun tree-cesta-prvky (value tree)
  (if (eql value (node-value tree))
      (list (node-value tree))
    (let ((sub (tree-cesta-children-prvky value (node-children tree))))
      (if (null sub)
          nil
        (cons (node-value tree) sub)))))

(defun tree-cesta-children-prvky (value trees)
  (if (null trees)
      nil
    (let ((sub (tree-cesta-prvky value (car trees))))
      (if (null sub)
          (tree-cesta-children-prvky value (cdr trees))
        sub))))

;1
;(let (((/ 1 0) 1))
;  0)
;(/ 1 0) nie je symbol

;2
(defun sum-digits (n)
  (if (= n 0)
      0
    (+ (rem n 10)
       (sum-digits (floor n 10)))))

;3
(defun tretia-uloha ()
  (let ((x (cons 'b 'c)))
    (cons x (cons x 'a))))

;4
(defun even-elements (list)
  (e-iter list 0))

(defun e-iter (list n)
  (cond ((null list) nil)
        ((evenp n) (cons (car list) (e-iter (cdr list) (1+ n))))
        (t (e-iter (cdr list) (1+ n)))))

;5
(defun my-set-intersection (a b)
  (cond ((null a) 
         nil)
        ((in-list (car a) b)
         (cons (car a) (my-set-intersection (cdr a) b)))
        (t (my-set-intersection (cdr a) b))))

(defun in-list (x list)
  (cond ((null list) nil)
        ((eql x (car list)) t)
        (t (in-list x (cdr list)))))

;odbocka
(defun mergex (a b)
  (cond ((null a) b)
        ((null b) a)
        ((< (car a) (car b)) (cons (car a) (mergex (cdr a) b)))
        (t (cons (car b) (mergex a (cdr b))))))

;6
#|
(defun my-reverse (list)
  (foldr (function cons) list nil))

(defun foldl (fun list init)
  (labels ((f-iter (fun list)
             (let* ((novy (pop-last list))
                    (posledny (get-last list)))
               (if (null novy)
                   posledny
                 (funcall fun posledny (f-iter fun novy)))))
           (get-last (list)
             (if (null (cdr list))
                 (car list)
               (get-last (cdr list))))
           (pop-last (list)
             (if (null (cdr list))
                 nil
               (cons (car list) (pop-last (cdr list))))))
    (funcall fun (f-iter fun list) init)))
|#

(defun my-revappend (list1 list2)
  (if (null list1)
      list2
    (my-revappend (cdr list1)
                  (cons (car list1) list2))))

(defun my-reverse (list)
  (foldl (quote cons) list nil))

;foldl funguje vlastne ako revappend
(defun foldl (fun list init)
  (if (null list)
      init
    (foldl fun
           (cdr list)
           (funcall fun (car list) init))))
