;;; 6.4. Control features

(define (map f coll)
  (if (null? coll) '()
      (cons (f (car coll)) (map f (cdr coll)))))
