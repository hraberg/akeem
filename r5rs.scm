;;; 6.4. Control features

(define (map proc list)
  (if (null? list) '()
      (cons (proc (car list)) (map proc (cdr list)))))

(define force
  (lambda (object)
    (object)))
