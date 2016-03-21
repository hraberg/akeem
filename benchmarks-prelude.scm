;;; Racket Benchmarks Prelude

(define-syntax time
  (syntax-rules ()
    ((time body ...)
     (let ((result (begin body ...)))
       (display result)
       (newline)
       result))))
