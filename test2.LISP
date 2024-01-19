;8
(defun pascal (y x)
  (cond ((or (< x 0) (> x y)) 0)
        ((= y 0) 1)
        ((or (= x 0) (= x y)) 1)
        (t (+ (pascal (1- y) x)
              (pascal (1- y) (1- x))))))

(defun pascal-row (r)
  (labels ((iter (i)
             (if (> i r)
                 nil
               (cons (pascal r i)
                     (iter (1+ i))))))
    (iter 0)))

(defun tridlicka (n)
  (if (< n 0)
      nil
    (im:split (im:chain (im:segment) (im:rotated (im:segment) pi))
              (im:chain (im:segment) 
                        (im:scaled (tridlicka (1- n)) 0.5))
              (im:rotated (im:chain (im:segment) 
                                    (im:scaled (tridlicka (1- n)) 0.5))
                          (/ pi 6))
              (im:rotated (im:chain (im:segment) 
                                    (im:scaled (tridlicka (1- n)) 0.5))
                          (/ pi -6)))))

(defun my-img (n angle)
  (labels ((iter (i a)
             (if (> i n)
                 nil
               (im:chain (im:rotated (im:segment) a)
                         (im:scaled (iter (1+ i) (+ a angle))
                                    0.9)))))
    (iter 0 0)))