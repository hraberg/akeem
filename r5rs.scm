;;; 6.2.5. Numerical operations

(define (zero? x)
  (= 0 x))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (odd? n)
  (not (even? n)))

(define (even? n)
  (zero? (modulo n 2)))

(define (max x1 x2)
  (if (< x1 x2)
      x2
      x1))

(define (min x1 x2)
  (if (> x1 x2)
      x2
      x1))

(define (abs x)
  (if (negative? x)
      (- 0 x)
      x))

;;; 6.3.2. Pairs and lists

(define (caar obj)
  (car (car obj)))

(define (cadr obj)
  (car (cdr obj)))

(define (cdar obj)
  (cdr (car obj)))

(define (cddr obj)
  (cdr (cdr obj)))

(define (list? obj)
  (or (null? obj)
      (and (pair? obj) (list? (cdr obj)))))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (- k 1))))

;;; 6.3.6. Vectors

(define (vector->list-aux vector list idx)
  (if (negative? idx)
      list
      (vector->list-aux
       vector (cons (vector-ref vector idx) list) (- idx 1))))

(define (vector->list vector)
  (vector->list-aux
   vector '() (- (vector-length vector) 1)))

;;; 6.4. Control features

(define (map proc list)
  (if (null? list) '()
      (cons (proc (car list)) (map proc (cdr list)))))

(define (for-each proc list)
  (if (null? list) '()
      (begin (proc (car list))
             (for-each proc (cdr list)))))

(define force
  (lambda (object)
    (object)))
