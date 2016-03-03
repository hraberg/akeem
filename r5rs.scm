(define map
  (lambda (f coll)
    (if (null? coll) '()
        (cons (f (car coll)) (map f (cdr coll))))))
