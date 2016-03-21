;;; R7RS

;;; 4.2.1. Conditionals

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))

;;; 6.2.6. Numerical operations

(define (exact-integer? z)
  (and (integer? z) (exact? z)))

(define (square z)
  (* z z))

(define inexact exact->inexact)
(define exact inexact->exact)

;;; 6.4. Pairs and lists

(define (make-list k fill)
  (do ((acc '() (cons fill acc))
       (idx 0 (+ idx 1)))
      ((= idx k) acc)))

(define (list-set! list k obj)
  (set-car! (list-tail list k) obj))

(define (list-copy obj)
  (let* ((length (length obj))
         (acc (make-list length)))
    (do ((from obj (cdr from))
         (to acc (cdr to)))
        ((null? from) acc)
      (set-car! to (car from)))))

;;; 6.5. Symbols

(define (symbol=? symbol1 symbol2)
  (equal? (symbol->string symbol1) (symbol->string symbol2)))

;;; 6.7. Strings

(define (string-upcase string)
  (string-map char-upcase string))

(define (string-downcase string)
  (string-map char-downcase string))

;;; 6.8. Vectors

(define (vector-copy vector)
  (let ((length (vector-length vector)))
    (do ((acc (make-vector length))
         (idx 0 (+ idx 1)))
        ((= idx length) acc)
      (vector-set! acc idx (vector-ref vector idx)))))

(define (string->vector string)
  (let ((length (string-length string)))
    (do ((acc (make-vector length))
         (idx 0 (+ idx 1)))
        ((= idx length) acc)
      (vector-set! acc idx (string-ref string idx)))))

(define (vector->string vector)
  (let ((length (vector-length vector)))
    (do ((acc (make-string length))
         (idx 0 (+ idx 1)))
        ((= idx length) acc)
      (string-set! acc idx (vector-ref vector idx)))))

(define (vector-append vector1 vector2)
  (let* ((length1 (vector-length vector1))
         (length2 (vector-length vector2))
         (acc (make-vector (+ length1 length2))))
    (do ((idx 0 (+ idx 1)))
        ((= idx length1))
      (vector-set! acc idx (vector-ref vector1 idx)))
    (do ((idx 0 (+ idx 1)))
        ((= idx length2))
      (vector-set! acc (+ idx length1) (vector-ref vector2 idx)))
    acc))

;;; 6.10. Control features

(define (string-map proc string)
  (let ((length (string-length string)))
    (do ((acc (make-string length))
         (idx 0 (+ idx 1)))
        ((= idx length) acc)
      (string-set! acc idx (proc (string-ref string idx))))))

(define (vector-map proc vector)
  (let ((length (vector-length vector)))
    (do ((acc (make-vector length))
         (idx 0 (+ idx 1)))
        ((= idx length) acc)
      (vector-set! acc idx (proc (vector-ref vector idx))))))

(define (string-for-each proc string)
  (let ((length (string-length string)))
    (do ((idx 0 (+ idx 1)))
        ((= idx length))
      (proc (string-ref string idx)))))

(define (vector-for-each proc vector)
  (let ((length (vector-length vector)))
    (do ((idx 0 (+ idx 1)))
        ((= idx length))
      (proc (vector-ref vector idx)))))

(define call/cc call-with-current-continuation)
