(define-syntax time
  (syntax-rules ()
    ((time body ...)
     (let ((result (begin body ...)))
       (display result)
       (newline)
       result))))

;;; SRFI 1: List Library

(define (filter pred list)
  (if (null? list) '()
      (if (pred (car list))
          (cons (car list) (filter pred (cdr list)))
          (filter pred (cdr list)))))

(define (reduce f ridentity list)
  (if (null? list) ridentity
      (reduce f (f ridentity (car list)) (cdr list))))
