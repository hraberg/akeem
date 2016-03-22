;;; Racket Benchmarks Prelude

(define (milliseconds z)
  (inexact->exact (* 1000 z)))

(define-syntax time
  (syntax-rules ()
    ((time body ...)
     (let* ((real-start (current-second))
            (cpu-start (current-jiffy))
            (result (begin body ...))
            (real-time (milliseconds (- (current-second) real-start)))
            (cpu-time (milliseconds (/ (- (current-jiffy) cpu-start)
                                       (jiffies-per-second))))
            (gc-start (current-second))
            (gc-time (begin
                       (gc)
                       (milliseconds (- (current-second) gc-start)))))
       (display "cpu time: ")
       (display cpu-time)
       (display " real time: ")
       (display real-time)
       (display " gc time: ")
       (display gc-time)
       (newline)
       (unless (eq? (void) result)
         (display result)
         (newline))
       result))))
