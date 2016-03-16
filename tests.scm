(define (assert x)
  (write x)
  (newline))

;;; 2. Lexical conventions

;;; 2.2. Whitespace and comments

;;; The FACT procedure computes the factorial
;;; of a non-negative integer.
(define fact
  (lambda (n)
    (if (= n 0)
        1 ;Base case: return 1
        (* n (fact (- n 1))))))
(assert (fact 10))

;;; 4. Expressions

;;; 4.1. Primitive expression types

;;; 4.1.1. Variable references
(define x 28)
(assert x)

;;; 4.1.2. Literal expressions
(assert (quote a))
(assert (quote #(a b c)))
(assert (quote (+ 1 2)))

(assert 'a)
(assert '#(a b c))
(assert '())
(assert '(+ 1 2))
(assert '(quote a))
(assert ''a)

(assert '"abc")
(assert "abc")
(assert '145932)
(assert 145932)
(assert '#t)
(assert #t)

;;; 4.1.3. Procedure calls
(assert (+ 3 4))
(assert ((if #f + *) 3 4))

;;; 4.1.4. Procedures
(assert (lambda (x) (+ x x)))
(assert ((lambda (x) (+ x x)) 4))

(define reverse-subtract
  (lambda (x y) (- y x)))
(assert (reverse-subtract 7 10))

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(assert (add4 6))

;;; 4.1.5. Conditionals
(assert (if (> 3 2) 'yes 'no))
(assert (if (> 2 3) 'yes 'no))
(assert (if (> 3 2)
            (- 3 2)
            (+ 3 2)))

;;; 4.1.6. Assignments
(define x 2)
(assert (+ x 1))
(assert (set! x 4))
(assert (+ x 1))

;;; 4.2. Derived expression types

;;; 4.2.1. Conditionals
(assert (cond ((> 3 2) 'greater)
              ((< 3 2) 'less)))
(assert (cond ((> 3 3) 'greater)
              ((< 3 3) 'less)
              (else 'equal)))
(assert (cond ((assv 'b '((a 1) (b 2))) => cadr)
              (else #f)))

(assert (case (* 2 3)
          ((2 3 5 7) 'prime)
          ((1 4 6 8 9) 'composite)))
(assert (case (car '(c d))
          ((a) 'a)
          ((b) 'b)))
(assert (case (car '(c d))
          ((a e i o u) 'vowel)
          ((w y) 'semivowel)
          (else 'consonant)))


(assert (and (= 2 2) (> 2 1)))
(assert (and (= 2 2) (< 2 1)))
(assert (and 1 2 'c '(f g)))
(assert (and))

(assert (or (= 2 2) (> 2 1)))
(assert (or (= 2 2) (< 2 1)))
(assert (or #f #f #f))
(assert (or (memq 'b '(a b c))
            (/ 3 0)))

;;; 4.2.2. Binding constructs
(assert (let ((x 2) (y 3))
          (* x y)))
(assert (let ((x 2) (y 3))
          (let ((x 7)
                (z (+ x y)))
            (* z x))))
(assert (let ((x 2) (y 3))
          (let* ((x 7)
                 (z (+ x y)))
            (* z x))))
;; (assert (letrec ((even?
;;                   (lambda (n)
;;                     (if (zero? n)
;;                         #t
;;                         (odd? (- n 1)))))
;;                  (odd?
;;                   (lambda (n)
;;                     (if (zero? n)
;;                         #f
;;                         (even? (- n 1))))))
;;           (even? 88)))

;;; 4.2.3. Sequencing

(define x 0)
(assert (begin (set! x 5)
               (+ x 1)))
(begin (display "4 plus 1 equals ")
       (display (+ 4 1)))
(newline)

;;; 5. Program structure

;;; 5.1. Programs

;;; 5.2. Definitions

;;; 5.2.1. Top level definitions
(define add3
  (lambda (x) (+ x 3)))
(assert (add3 3))
(define first car)
(assert (first '(1 2)))

;;; 6. Standard procedures

;;; 6.1. Equivalence predicates
(assert (eqv? 'a 'a))
(assert (eqv? 'a 'b))
(assert (eqv? 2 2))
(assert (eqv? '() '()))
(assert (eqv? 100000000 100000000))
(assert (eqv? (cons 1 2) (cons 1 2)))
(assert (eqv? (lambda () 1)
              (lambda () 2)))
(assert (eqv? #f 'nil))
(assert (let ((p (lambda (x) x)))
          (eqv? p p)))

(assert (eqv? "" ""))
(assert (eqv? '#() '#()))
(assert (eqv? (lambda (x) x)
              (lambda (x) x)))
(assert (eqv? (lambda (x) x)
              (lambda (y) y)))

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(assert (let ((g (gen-counter)))
          (eqv? g g)))
;; (assert (eqv? (gen-counter) (gen-counter)))

(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(assert (let ((g (gen-loser)))
          (eqv? g g)))
;; (assert (eqv? (gen-loser) (gen-loser)))


(assert (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
                 (g (lambda () (if (eqv? f g) 'both 'g))))
          (eqv? f g)))

(assert (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                 (g (lambda () (if (eqv? f g) 'g 'both))))
          (eqv? f g)))

(assert (eqv? '(a) '(a)))
(assert (eqv? "a" "a"))
(assert (eqv? '(b) (cdr '(a b))))
(assert (let ((x '(a)))
          (eqv? x x)))

(assert (eq? 'a 'a))
(assert (eq? '(a) '(a)))
;; (assert (eq? (list 'a) (list 'a)))
(assert (eq? "a" "a"))
(assert (eq? "" ""))
(assert (eq? '() '()))
(assert (eq? 2 2))
(assert (eq? #\A #\A))
(assert (eq? car car))
(assert (let ((n (+ 2 3)))
          (eq? n n)))
(assert (let ((x '(a)))
          (eq? x x)))
(assert (let ((x '#()))
          (eq? x x)))
(assert (let ((p (lambda (x) x)))
          (eq? p p)))

(assert (equal? 'a 'a))
(assert (equal? '(a) '(a)))
(assert (equal? '(a (b) c)
                '(a (b) c)))
(assert (equal? "abc" "abc"))
(assert (equal? 2 2))
(assert (equal? (make-vector 5 'a)
                (make-vector 5 'a)))
(assert (equal? (lambda (x) x)
                (lambda (y) y)))

;;; 6.2. Numbers

;;; 6.2.5. Numerical operations
;; (assert (complex? 3+4i))
(assert (complex? 3))
(assert (real? 3))
;; (assert (real? -2.5+0.0i))
;; (assert (real? #e1e10))
;; (assert (rational? 6/10))
;; (assert (rational? 6/3))
;; (assert (integer? 3+0i))
(assert (integer? 3.0))
;; (assert (integer? 8/4))

(assert (max 3 4))
(assert (max 3.9 4))

(assert (+ 3 4))
;; (assert (+ 3))
;; (assert (+))
;; (assert (* 4))
;; (assert (*))

(assert (- 3 4))
;; (assert (- 3 4 5))
;; (assert (- 3))
;; (assert (/ 3 4 5))
;; (aasert (/ 3))

(assert (abs -7))

(assert (let ((n1 2) (n2 4))
          (= n1 (+ (* n2 (quotient n1 n2))
                   (remainder n1 n2)))))

(assert (modulo 13 4))
(assert (remainder 13 4))
(assert (modulo -13 4))
(assert (remainder -13 4))
(assert (modulo 13 -4))
(assert (remainder 13 -4))
(assert (modulo -13 -4))
(assert (remainder -13 -4))
(assert (remainder -13 -4.0))

;; (assert (gcd 32 -36))
;; (assert (gcd))
;; (assert (lcm 32 -36))
;; (assert (lcm 32.0 -36))
;; (assert (lcm))

;; (assert (numerator (/ 6 4)))
;; (assert (denominator (/ 6 4)))
;; (assert (denominator
;;          (exact->inexact (/ 6 4))))

(assert (floor -4.3))
(assert (ceiling -4.3))
(assert (truncate -4.3))
(assert (round -4.3))
(assert (floor 3.5))
(assert (ceiling 3.5))
(assert (truncate 3.5))
(assert (round 3.5))
;; (assert (round 7/2))
(assert (round 7))

;; (assert (rationalize
;;          (inexact->exact .3) 1/10))
;; (assert (rationalize .3 1/10))

;;; 6.2.6. Numerical input and output
(assert (let ((number 20)
              (radix 16))
          (eqv? number
                (string->number (number->string number
                                                radix)
                                radix))))

(assert (string->number "100"))
(assert (string->number "100" 16))
(assert (string->number "1e2"))
;; (assert (string->number "15##"))

;;; 6.3. Other data types

;;; 6.3.1. Booleans
(assert #t)
(assert #f)
(assert '#f)

(assert (not #t))
(assert (not 3))
;; (not (list 3))
(assert (not #f))
(assert (not '()))
;; (not (list)) =⇒ #f
(assert (not 'nil))

(assert (boolean? #f))
(assert (boolean? 0))
(assert (boolean? '()))

;;; 6.3.2. Pairs and lists
(define x '(a b c)) ;; should not be constant
(define y x)
(assert y)
(assert (list? y))
(assert (set-cdr! x 4))
(assert x)
(assert (eqv? x y))
(assert y)
(assert (list? y))
(assert (set-cdr! x x))
;; (assert (list? x))

(assert (pair? '(a . b)))
(assert (pair? '(a b c)))
(assert (pair? '()))
(assert (pair? '#(a b)))

(assert (cons 'a '()))
(assert (cons '(a) '(b c d)))
(assert (cons "a" '(b c)))
(assert (cons 'a 3))
(assert (cons '(a b) 'c))

(assert (car '(a b c)))
(assert (car '((a) b c d)))
(assert (car '(1 . 2)))
;; (assert (car '()))

(assert (cdr '((a) b c d)))
(assert (cdr '(1 . 2)))
;; (assert (cdr '()))

;; (define (f) (list 'not-a-constant-list))
;; (define (g) '(constant-list))
;; (assert (set-car! (f) 3))
;; (assert (set-car! (g) 3))

(assert (list? '(a b c)))
(assert (list? '()))
(assert (list? '(a . b)))
;; (assert (let ((x '(a)))
;;           (set-cdr! x x)
;;           (list? x)))

;; (assert (list 'a (+ 3 4) 'c))
;; (assert (list))

(assert (length '(a b c)))
(assert (length '(a (b) (c d e))))
(assert (length '()))

(assert (append '(x) '(y)))
(assert (append '(a) '(b c d)))
(assert (append '(a (b)) '((c))))

(assert (append '(a b) '(c . d)))
(assert (append '() 'a))

(assert (reverse '(a b c)))
(assert (reverse '(a (b c) d (e (f)))))

(assert (list-ref '(a b c d) 2))
(assert (list-ref '(a b c d)
                  (inexact->exact (round 1.8))))

(assert (memq 'a '(a b c)))
(assert (memq 'b '(a b c)))
(assert (memq 'a '(b c d)))
(assert (memq '(a) '(b (a) c)))
(assert (member '(a)
                '(b (a) c)))
(assert (memq 101 '(100 101 102)))
(assert (memv 101 '(100 101 102)))

(define e '((a 1) (b 2) (c 3)))
(assert (assq 'a e))
(assert (assq 'b e))
(assert (assq 'd e))
(assert (assq '(a) '(((a)) ((b)) ((c)))))
(assert (assoc '(a) '(((a)) ((b)) ((c)))))
(assert (assq 5 '((2 3) (5 7) (11 13))))
(assert (assv 5 '((2 3) (5 7) (11 13))))

;;; 6.3.3. Symbols
(assert (symbol? 'foo))
(assert (symbol? (car '(a b))))
(assert (symbol? "bar"))
(assert (symbol? 'nil))
(assert (symbol? '()))
(assert (symbol? #f))

(assert (symbol->string 'flying-fish))
 ;; (assert (symbol->string 'Martin))
(assert (symbol->string
         (string->symbol "Malvina")))

;; (assert (eq? 'mISSISSIppi 'mississippi))

(assert (string->symbol "mISSISSIppi"))
;; (assert (eq? 'bitBlt (string->symbol "bitBlt")))
(assert (eq? 'JollyWog
             (string->symbol
              (symbol->string 'JollyWog))))
;; (asseert (string=? "K. Harper, M.D."
;;                    (symbol->string
;;                     (string->symbol "K. Harper, M.D."))))

;;; 6.3.4. Characters
(assert (<= (char->integer #\a)
            (char->integer #\a)))
;; (assert (char<=? (integer->char x)
;;                  (integer->char y)))

;;; 6.3.5. Strings
(define (f) (make-string 3 #\*))
;; (define (g) "***")
(assert (string-set! (f) 0 #\?))
;; (assert (string-set! (g) 0 #\?))
;; (assert (string-set! (symbol->string 'immutable)
;;                      0
;;                      #\?))

;;; 6.3.6. Vectors
(assert '#(0 (2 2 2 2) "Anna"))

;; (vector 'a 'b 'c)

(assert (vector-ref '#(1 1 2 3 5 8 13 21)
                    5))
(assert (vector-ref '#(1 1 2 3 5 8 13 21)
                    (let ((i (round (* 2 (acos -1)))))
                      (if (inexact? i)
                          (inexact->exact i)
                          i))))

(assert (let ((vec '#(0 '(2 2 2 2) "Anna")))  ;; should not be constant
          (vector-set! vec 1 '("Sue" "Sue"))
          vec))
;; (assert (vector-set! '#(0 1 2) 1 "doe"))

(assert (vector->list '#(dah dah didah)))
(assert (list->vector '(dididit dah)))

;;; 6.4. Control features
(assert (procedure? car))
(assert (procedure? 'car))
(assert (procedure? (lambda (x) (* x x))))
(assert (procedure? '(lambda (x) (* x x))))
(assert (call-with-current-continuation procedure?))

(assert (apply + '(3 4)))
;; (define compose
;;   (lambda (f g)
;;     (lambda args
;;       (f (apply g args)))))
;; (assert ((compose sqrt *) 12 75))

(assert (map cadr '((a b) (d e) (g h))))

(assert (map (lambda (n) (expt n n))
             '(1 2 3 4 5)))
;; (assert (map + '(1 2 3) '(4 5 6)))
;; (assert (let ((count 0))
;;           (map (lambda (ignored)
;;                  (set! count (+ count 1))
;;                  count)
;;                '(a b))))

(assert (let ((v (make-vector 5)))
          (for-each (lambda (i)
                      (vector-set! v i (* i i)))
                    '(0 1 2 3 4))
          v))

(assert (force (delay (+ 1 2))))
;; (assert (let ((p (delay (+ 1 2))))
;;           (list (force p) (force p))))
;; (define a-stream
;;   (letrec ((next
;;             (lambda (n)
;;               (cons n (delay (next (+ n 1)))))))
;;     (next 0)))
;; (define head car)
;; (define tail
;;   (lambda (stream) (force (cdr stream))))
;; (assert (head (tail (tail a-stream))))

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))
(define x 5)
(assert p)
(assert (force p))
(assert p)
;; (assert (begin (set! x 10)
;;                (force p)))

(assert (eqv? (delay 1) 1))
(assert (pair? (delay (cons 1 2))))

;; (assert (+ (delay (* 3 7)) 13))

;; (assert (call-with-current-continuation
;;          (lambda (exit)
;;            (for-each (lambda (x)
;;                        (if (negative? x)
;;                            (exit x)))
;;                      '(54 0 37 -3 245 19))
;;            #t)))
;; (define list-length
;;   (lambda (obj)
;;     (call-with-current-continuation
;;      (lambda (return)
;;        (letrec ((r
;;                  (lambda (obj)
;;                    (cond ((null? obj) 0)
;;                          ((pair? obj)
;;                           (+ (r (cdr obj)) 1))
;;                          (else (return #f))))))
;;          (r obj))))))
;; (assert (list-length '(1 2 3 4)))
;; (assert (list-length '(a b . c)))

;; (assert (call-with-values (lambda () (values 4 5))
;;           (lambda (a b) b)))
;; (assert (call-with-values * -))

;; (assert (let ((path '())
;;               (c #f))
;;           (let ((add (lambda (s)
;;                        (set! path (cons s path)))))
;;             (dynamic-wind
;;                 (lambda () (add 'connect))
;;                 (lambda ()
;;                   (add (call-with-current-continuation
;;                         (lambda (c0)
;;                           (set! c c0)
;;                           'talk1))))
;;                 (lambda () (add 'disconnect)))
;;             (if (< (length path) 4)
;;                 (c 'talk2)
;;                 (reverse path)))))

;;; 6.5. Eval
(assert (eval '(* 7 3) (scheme-report-environment 5)))
(assert (let ((f (eval '(lambda (f x) (f x x))
                       (null-environment 5))))
          (f + 10)))
