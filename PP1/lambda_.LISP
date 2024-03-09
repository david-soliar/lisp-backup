(funcall (lambda (x y)
           (funcall x y))
         (lambda (z)
           (+ z z))
         20)
;let using lambda
