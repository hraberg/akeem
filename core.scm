(define (filter f coll)
  (if (null? coll) '()
      (if (f (car coll)) (cons (car coll) (filter f (cdr coll)))
          (filter f (cdr coll)))))

(define (reduce f init coll)
  (if (null? coll) init
      (reduce f (f init (car coll)) (cdr coll))))
