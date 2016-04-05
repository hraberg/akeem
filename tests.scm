(define (assert x)
  (write x)
  (newline))

(define (spec x)
  (display x)
  (newline))

(spec ";;; R7RS")
(spec ";;; 1. Overview of Scheme")
(spec ";;; 1.3. Notation and terminology")
(spec ";;; 1.3.4. Evaluation examples")

(assert (* 5 8))

(assert ";;; 2. Lexical conventions")
(assert ";;; 2.2. Whitespace and comments")

;;; The FACT procedure computes the factorial
;;; of a non-negative integer.
(define fact
  (lambda (n)
    (if (= n 0)
        1 ;Base case: return 1
        (* n (fact (- n 1))))))
(assert (fact 10)) ;; not an actual example

(spec ";;; 4. Expressions")
(spec ";;; 4.1. Primitive expression types")
(spec ";;; 4.1.1. Variable references")
(define x 28)
(assert x)

(spec ";;; 4.1.2. Literal expressions")
(assert (quote a))
(assert (quote #(a b c)))
(assert (quote (+ 1 2)))

(assert 'a)
(assert '#(a b c))
(assert '())
(assert '(+ 1 2))
(assert '(quote a))
(assert ''a)

(assert '145932)
(assert 145932)
(assert '"abc")
(assert "abc")
;; (assert '#)
;; (assert #)
(assert '#(a 10))
(assert #(a 10))
(assert '#u8(64 65))
(assert #u8(64 65))
(assert '#t)
(assert #t)

(spec ";;; 4.1.3. Procedure calls")
(assert (+ 3 4))
(assert ((if #f + *) 3 4))

(spec ";;; 4.1.4. Procedures")
(assert (lambda (x) (+ x x)))
(assert ((lambda (x) (+ x x)) 4))

(define reverse-subtract
  (lambda (x y) (- y x)))
(assert (reverse-subtract 7 10))

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(assert (add4 6))

(assert ((lambda x x) 3 4 5 6))
(assert ((lambda (x y . z) z)
         3 4 5 6))

(spec ";;; 4.1.5. Conditionals")
(assert (if (> 3 2) 'yes 'no))
(assert (if (> 2 3) 'yes 'no))
(assert (if (> 3 2)
            (- 3 2)
            (+ 3 2)))

(spec ";;; 4.1.6. Assignments")
(define x 2)
(assert (+ x 1))
(assert (set! x 4))
(assert (+ x 1))

(spec ";;; 4.2. Derived expression types")
(spec ";;; 4.2.1. Conditionals")
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
          (else => (lambda (x) x))))

(assert (and (= 2 2) (> 2 1)))
(assert (and (= 2 2) (< 2 1)))
(assert (and 1 2 'c '(f g)))
(assert (and))

(assert (or (= 2 2) (> 2 1)))
(assert (or (= 2 2) (< 2 1)))
(assert (or #f #f #f))
(assert (or (memq 'b '(a b c))
            (/ 3 0)))

(assert (when (= 1 1.0)
          (display "1")
          (display "2")
          (newline)))

(assert (unless (= 1 1.0)
          (display "1")
          (display "2")
          (newline)))

(spec ";;; 4.2.2. Binding constructs")
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

(assert (letrec ((even?
                  (lambda (n)
                    (if (zero? n)
                        #t
                        (odd? (- n 1)))))
                 (odd?
                  (lambda (n)
                    (if (zero? n)
                        #f
                        (even? (- n 1))))))
          (even? 88)))

;; (assert (letrec* ((p
;;                    (lambda (x)
;;                      (+ 1 (q (- x 1)))))
;;                   (q
;;                    (lambda (y)
;;                      (if (zero? y)
;;                          0
;;                          (+ 1 (p (- y 1))))))
;;                   (x (p 5))
;;                   (y x))
;;                  y))

;; (assert (let-values (((root rem) (exact-integer-sqrt 32)))
;;           (* root rem)))

;; (assert (let ((a ’a) (b ’b) (x ’x) (y ’y))
;;           (let*-values (((a b) (values x y))
;;                         ((x y) (values a b)))
;;             (list a b x y))))

(spec ";;; 4.2.3. Sequencing")
(define x 0)
(assert (begin (set! x 5)
               (+ x 1)))
(begin (display "4 plus 1 equals ")
       (display (+ 4 1)))
(newline)

(spec ";;; 4.2.4. Iteration")
(assert (do ((vec (make-vector 5))
             (i 0 (+ i 1)))
            ((= i 5) vec)
          (vector-set! vec i i)))

(assert (let ((x '(1 3 5 7 9)))
          (do ((x x (cdr x))
               (sum 0 (+ sum (car x))))
              ((null? x) sum))))

(assert (let loop ((numbers '(3 -2 1 6 -5))
                   (nonneg '())
                   (neg '()))
          (cond ((null? numbers) (list nonneg neg))
                ((>= (car numbers) 0)
                 (loop (cdr numbers)
                       (cons (car numbers) nonneg)
                       neg))
                ((< (car numbers) 0)
                 (loop (cdr numbers)
                       nonneg
                       (cons (car numbers) neg))))))

(spec ";;; 4.2.5. Delayed evaluation")
(assert (force (delay (+ 1 2))))
(assert (let ((p (delay (+ 1 2))))
          (list (force p) (force p))))

(define integers
  (letrec ((next
            (lambda (n)
              (delay (cons n (next (+ n 1)))))))
    (next 0)))
(define head
  (lambda (stream) (car (force stream))))
(define tail
  (lambda (stream) (cdr (force stream))))
(assert (head (tail (tail integers))))

(define (stream-filter p? s)
  (delay-force
   (if (null? (force s))
       (delay '())
       (let ((h (car (force s)))
             (t (cdr (force s))))
         (if (p? h)
             (delay (cons h (stream-filter p? t)))
             (stream-filter p? t))))))
(assert (head (tail (tail (stream-filter odd? integers)))))

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
(assert (begin (set! x 10)
               (force p)))

(assert (eqv? (delay 1) 1))
(assert (pair? (delay (cons 1 2))))

(assert (car
         (list (delay (* 3 7)) 13)))

(spec ";;; 4.2.6. Dynamic bindings")
(define radix
  (make-parameter
   10
   (lambda (x)
     (if (and (exact-integer? x) (<= 2 x 16))
         x
         (error "invalid radix")))))

(define (f n) (number->string n (radix)))

(assert (f 12))
(assert (parameterize ((radix 16)) ;;; should be 2
          (f 12)))
(assert (f 12))

(spec ";;; 4.2.7. Exception handling")
(assert (guard (condition
                ((assq 'a condition) => cdr)
                ((assq 'b condition)))
          (raise (list (cons 'a 42)))))

(assert (guard (condition
                ((assq 'a condition) => cdr)
                ((assq 'b condition)))
          (raise (list (cons 'b 23)))))

(spec ";;; 4.2.8. Quasiquotation")
(assert `(list ,(+ 1 2) 4))
(assert (let ((name 'a)) `(list ,name ',name)))
(assert `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(assert `(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
(assert `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))

;; (assert `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
;; (assert (let ((name1 'x)
;;               (name2 'y))
;;           `(a `(b ,,name1 ,',name2 d) e)))

(assert (quasiquote (list (unquote (+ 1 2)) 4)))
(assert '(quasiquote (list (unquote (+ 1 2)) 4)))

(spec ";;; 4.2.9. Case-lambda")
(define range
  (case-lambda
   ((e) (range 0 e))
   ((b e) (do ((r '() (cons e r))
               (e (- e 1) (- e 1)))
              ((< e b) r)))))

(assert (range 3))
(assert (range 3 5))

(spec ";;; 4.3. Macros")
(spec ";;; 4.3.1. Binding constructs for syntactic keywords")
;; (assert (let-syntax ((when (syntax-rules ()
;;                              ((when test stmt1 stmt2 ...)
;;                               (if test
;;                                   (begin stmt1
;;                                          stmt2 ...))))))
;;           (let ((if #t))
;;             (when if (set! if ’now))
;;             if)))

;; (assert (let ((x ’outer))
;;           (let-syntax ((m (syntax-rules () ((m) x))))
;;             (let ((x ’inner))
;;               (m)))))

;; (assert (letrec-syntax
;;             ((my-or (syntax-rules ()
;;                       ((my-or) #f)
;;                       ((my-or e) e)
;;                       ((my-or e1 e2 ...)
;;                        (let ((temp e1))
;;                          (if temp
;;                              temp
;;                              (my-or e2 ...)))))))
;;           (let ((x #f)
;;                 (y 7)
;;                 (temp 8)
;;                 (let odd?)
;;                 (if even?))
;;             (my-or x
;;                    (let temp)
;;                    (if y)
;;                    y))))

(spec ";;; 4.3.2. Pattern language")

(assert (let ((=> #f))
          (cond (#t => 'ok))))

(spec ";;; 5. Program structure")
(spec ";;; 5.1. Programs")
(spec ";;; 5.3. Variable definitions")
(spec ";;; 5.3.1. Top level definitions")
(define add3
  (lambda (x) (+ x 3)))
(assert (add3 3))
(define first car)
(assert (first '(1 2)))

;; (assert ";;;5.3.2. Internal definitions")
;; (assert (let ((x 5))
;;           (define foo (lambda (y) (bar x y)))
;;           (define bar (lambda (a b) (+ (* a b) a)))
;;           (foo (+ x 3))))

(spec ";;; 5.5. Record-type definitions")
(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(assert (pare? (kons 1 2)))
(assert (pare? (cons 1 2)))
(assert (kar (kons 1 2)))
(assert (kdr (kons 1 2)))
(assert (let ((k (kons 1 2)))
          (set-kar! k 3)
          (kar k)))

(spec ";;; 6. Standard procedures")
(spec ";;; 6.1. Equivalence predicates")
(assert (string=? (symbol->string 'foo)
                  (symbol->string 'foo)))

(assert (string=? (symbol->string 'obj1)
                  (symbol->string 'obj2)))

(assert (eqv? 'a 'a))
(assert (eqv? 'a 'b))
(assert (eqv? 2 2))
(assert (eqv? '() '()))
(assert (eqv? 100000000 100000000))
(assert (eqv? (cons 1 2) (cons 1 2)))
(assert (eqv? (lambda () 1)
              (lambda () 2)))
(assert (let ((p (lambda (x) x)))
          (eqv? p p)))
(assert (eqv? #f 'nil))

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
(assert (eqv? (gen-counter) (gen-counter)))

(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(assert (let ((g (gen-loser)))
          (eqv? g g)))
(assert (eqv? (gen-loser) (gen-loser)))

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
(assert (eq? (list 'a) (list 'a)))
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

(spec ";;; 6.2. Numbers")
(spec ";;; 6.2.6. Numerical operations")
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

(assert (exact-integer? 32))
(assert (exact-integer? 32.0))

(assert (finite? 3))
(assert (finite? (/ 1.0 0.0))) ;; should be +inf.0
;; (assert (finite? 3.0+inf.0i))

(assert (infinite? 3))
(assert (infinite? (/ 1.0 0.0))) ;; should be +inf.0
(assert (infinite? (/ 0.0 0.0))) ;; should be +nan.0
;; (assert (infinite? 3.0+inf.0i))

(assert (nan? (/ 0.0 0.0))) ;; should be +nan.0
(assert (nan? 32))
;; (assert (nan? +nan.0+5.0i))
;; (assert (nan? 1+2i))

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

;;; These are from R5RS, R7RS specifies truncate-remainder, floor/ etc.
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

(assert (gcd 32 -36))
;; (assert (gcd))
(assert (lcm 32 -36))
(assert (lcm 32.0 -36))
;; (assert (lcm))

;; (assert (numerator (/ 6 4)))
;; (assert (denominator (/ 6 4)))
;; (assert (denominator
;;          (inexact (/ 6 4))))

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
;;          (exact .3) 1/10))
;; (assert (rationalize .3 1/10))

(assert (square 42))
(assert (square 2.0))

(assert (exact-integer-sqrt 4))
(assert (exact-integer-sqrt 5))

(spec ";;; 6.2.7. Numerical input and output")
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

(spec ";;; 6.3. Booleans")
(assert #t)
(assert #f)
(assert '#f)

(assert (not #t))
(assert (not 3))
(assert (not (list 3)))
(assert (not #f))
(assert (not '()))
(assert (not (list)))
(assert (not 'nil))

(assert (boolean? #f))
(assert (boolean? 0))
(assert (boolean? '()))

(spec ";;; 6.4. Pairs and lists")
(define x (list 'a 'b 'c))
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

(define (f) (list 'not-a-constant-list))
;; (define (g) '(constant-list))
(assert (set-car! (f) 3))
;; (assert (set-car! (g) 3))

(assert (list? '(a b c)))
(assert (list? '()))
(assert (list? '(a . b)))
;; (assert (let ((x '(a)))
;;           (set-cdr! x x)
;;           (list? x)))

(assert (make-list 2 3))

(assert (list 'a (+ 3 4) 'c))
(assert (list))

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
                  (exact (round 1.8))))

(assert (let ((ls (list 'one 'two 'five!)))
          (list-set! ls 2 'three)
          ls))

(assert (memq 'a '(a b c)))
(assert (memq 'b '(a b c)))
(assert (memq 'a '(b c d)))
(assert (memq (list 'a) '(b (a) c)))
(assert (member (list 'a)
                '(b (a) c)))
(assert (member "B"
                '("a" "b" "c")
                string-ci=?))
(assert (memq 101 '(100 101 102)))
(assert (memv 101 '(100 101 102)))

(define e '((a 1) (b 2) (c 3)))
(assert (assq 'a e))
(assert (assq 'b e))
(assert (assq 'd e))
(assert (assq (list 'a) '(((a)) ((b)) ((c)))))
(assert (assoc (list 'a) '(((a)) ((b)) ((c)))))
(assert (assoc 2.0 '((1 1) (2 4) (3 9)) =))
(assert (assq 5 '((2 3) (5 7) (11 13))))
(assert (assv 5 '((2 3) (5 7) (11 13))))

(define a '(1 8 2 8))
(define b (list-copy a))
(set-car! b 3)
(assert b)
(assert a)

(spec ";;; 6.5. Symbols")
(assert (symbol? 'foo))
(assert (symbol? (car '(a b))))
(assert (symbol? "bar"))
(assert (symbol? 'nil))
(assert (symbol? '()))
(assert (symbol? #f))

(assert (symbol->string 'flying-fish))
(assert (symbol->string 'Martin))
(assert (symbol->string
         (string->symbol "Malvina")))

(assert (string->symbol "mISSISSIppi"))
(assert (eq? 'bitBlt (string->symbol "bitBlt")))
(assert (eq? 'JollyWog
             (string->symbol
              (symbol->string 'JollyWog))))
(assert (string=? "K. Harper, M.D."
                  (symbol->string
                   (string->symbol "K. Harper, M.D."))))

(spec ";;; 6.6. Characters")
(assert (char-ci=? #\A #\a))

(assert (digit-value #\3))
;; (assert (digit-value #\x0664))
;; (assert (digit-value #\x0AE6))
(assert (digit-value #\a)) ;; should be #\x0EA6

(spec ";;; 6.7. Strings")
(assert "The word \"recursion\" has many meanings.")
(assert "Another example:\ntwo lines of text")
(assert "Here's text \
containing just one line")
(assert "\x061; is named GREEK SMALL LETTER ALPHA.") ;; should be \x03B1;


(define (f) (make-string 3 #\*))
;; (define (g) "***")
(assert (string-set! (f) 0 #\?))
;; (assert (string-set! (g) 0 #\?))
;; (assert (string-set! (symbol->string 'immutable)
;;                      0
;;                      #\?))

(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 1 a 0 2)
(assert b)

(spec ";;; 6.8. Vectors")
(assert '#(0 (2 2 2 2) "Anna"))

(assert (vector 'a 'b 'c))

(assert (vector-ref '#(1 1 2 3 5 8 13 21)
                    5))
(assert (vector-ref '#(1 1 2 3 5 8 13 21)
                    (let ((i (round (* 2 (acos -1)))))
                      (if (inexact? i)
                          (exact i)
                          i))))

(assert (let ((vec (vector 0 '(2 2 2 2) "Anna")))
          (vector-set! vec 1 '("Sue" "Sue"))
          vec))
;; (assert (vector-set! '#(0 1 2) 1 "doe"))

(assert (vector->list '#(dah dah didah)))
(assert (vector->list '#(dah dah didah) 1 2))
(assert (list->vector '(dididit dah)))

(assert (string->vector "ABC"))
(assert (vector->string
         #(#\1 #\2 #\3)))

(define a #(1 8 2 8))
(define b (vector-copy a))
(vector-set! b 0 3)
(assert b)

(define c (vector-copy b 1 3))
(assert c)

(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 1 a 0 2)
(assert b)

(assert (vector-append #(a b c) #(d e f)))

(define a (vector 1 2 3 4 5))
(vector-fill! a 'smash 2 4)
(assert a)

(spec ";;; 6.9. Bytevectors")
(assert (make-bytevector 2 12))

(assert (bytevector 1 3 5 1 3 5))
(assert (bytevector))

(assert (bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21) 5))

(assert (let ((bv (bytevector 1 2 3 4)))
          (bytevector-u8-set! bv 1 3)
          bv))

(define a #u8(1 2 3 4 5))
(assert (bytevector-copy a 2 4))

(define a (bytevector 1 2 3 4 5))
(define b (bytevector 10 20 30 40 50))
(bytevector-copy! b 1 a 0 2)
(assert b)

(assert (bytevector-append #u8(0 1 2) #u8(3 4 5)))

(assert (utf8->string #u8(#x41)))
(assert (string->utf8 "A")) ;; should be λ

(spec ";;; 6.10. Control features")
(assert (procedure? car))
(assert (procedure? 'car))
(assert (procedure? (lambda (x) (* x x))))
(assert (procedure? '(lambda (x) (* x x))))
(assert (call-with-current-continuation procedure?))

(assert (apply + '(3 4)))
(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))
(assert ((compose sqrt *) 12 75))

(assert (map cadr '((a b) (d e) (g h))))

(assert (map (lambda (n) (expt n n))
             '(1 2 3 4 5)))
;; (assert (map + '(1 2 3) '(4 5 6)))
;; (assert (let ((count 0))
;;           (map (lambda (ignored)
;;                  (set! count (+ count 1))
;;                  count)
;;                '(a b))))

(assert (string-map
         (lambda (c) (integer->char (+ 1 (char->integer c))))
         "HAL"))

(assert (vector-map cadr '#((a b) (d e) (g h))))

(assert (vector-map (lambda (n) (expt n n))
                    '#(1 2 3 4 5)))

(assert (let ((v (make-vector 5)))
          (for-each (lambda (i)
                      (vector-set! v i (* i i)))
                    '(0 1 2 3 4))
          v))

(assert (let ((v (list '()))) ;; should be '()
          (string-for-each
           (lambda (c) (set-car! v (cons (char->integer c) (car v))))
           "abcde")
          (car v)))

(assert (let ((v (make-list 5)))
          (vector-for-each
           (lambda (i) (list-set! v i (* i i)))
           '#(0 1 2 3 4))
          v))

;; (assert (+ (delay (* 3 7)) 13))

(assert (call-with-current-continuation
         (lambda (exit)
           (for-each (lambda (x)
                       (if (negative? x)
                           (exit x)))
                     '(54 0 37 -3 245 19))
           #t)))

(define list-length
  (lambda (obj)
    (call-with-current-continuation
     (lambda (return)
       (letrec ((r
                 (lambda (obj)
                   (cond ((null? obj) 0)
                         ((pair? obj)
                          (+ (r (cdr obj)) 1))
                         (else (return #f))))))
         (r obj))))))
(assert (list-length '(1 2 3 4)))
(assert (list-length '(a b . c)))

(assert (call-with-values (lambda () (values 4 5))
          (lambda (a b) b)))
;; (assert (call-with-values * -))

(assert (let ((path (list '())) ;; these should not be boxed
              (c (list #f)))
          (let ((add (lambda (s)
                       (set-car! path (cons s (car path))))))
            (dynamic-wind
              (lambda () (add 'connect))
              (lambda ()
                (add (call-with-current-continuation
                      (lambda (c0)
                        (set-car! c c0)
                        'talk1))))
              (lambda () (add 'disconnect)))
            (if (< (length (car path)) 4)
                ((car c) 'talk2)
                (reverse (car path))))))

(spec ";;; 6.11. Exceptions")
(assert (call-with-current-continuation
         (lambda (k)
           (with-exception-handler
            (lambda (x)
              (display "condition: ")
              (write x)
              (newline)
              (k 'exception))
            (lambda ()
              (+ 1 (raise 'an-error)))))))

;; (with-exception-handler
;;  (lambda (x)
;;    (display "something went wrong\n"))
;;  (lambda ()
;;    (+ 1 (raise 'an-error))))

(assert (with-exception-handler
         (lambda (con)
           (cond
            ((string? con)
             (display con))
            (else
             (display "a warning has been issued\n")))
           42)
         (lambda ()
           (+ (raise-continuable "should be a number\n")
              23))))

(spec ";;; 6.12. Environments and evaluation")
(assert (eval '(* 7 3) (environment '(scheme base))))
(assert (let ((f (eval '(lambda (f x) (f x x))
                       (environment '(scheme base)))))
          (f + 10)))


(spec ";;; Example")

(define (integrate-system system-derivative
                          initial-state
                          h)
  (let ((next (runge-kutta-4 system-derivative h)))
    (letrec ((states (list initial-state))) ;; should be one line without set-cdr!
      (set-cdr! states
                (delay (map-streams next
                                    states)))
      states)))

(define (runge-kutta-4 f h)
  (let ((*h (scale-vector h))
        (*2 (scale-vector 2))
        (*1/2 (scale-vector (/ 1 2)))
        (*1/6 (scale-vector (/ 1 6))))
    (lambda (y)
      ;; y is a system state
      (let* ((k0 (*h (f y)))
             (k1 (*h (f (add-vectors y (*1/2 k0)))))
             (k2 (*h (f (add-vectors y (*1/2 k1)))))
             (k3 (*h (f (add-vectors y k2)))))
        (add-vectors y
                     (*1/6 (add-vectors (add-vectors k0 ;; should be single add-vectors
                                                     (*2 k1))
                                        (add-vectors (*2 k2)
                                                     k3))))))))

(define (elementwise f)
  (lambda vectors
    (generate-vector
     (vector-length (car vectors))
     (lambda (i)
       (apply f
              (map (lambda (v) (vector-ref v i))
                   vectors))))))

(define (generate-vector size proc)
  (let ((ans (make-vector size)))
    (letrec ((loop
              (lambda (i)
                (cond ((= i size) ans)
                      (else
                       (vector-set! ans i (proc i))
                       (loop (+ i 1)))))))
      (loop 0))))

(define add-vectors (elementwise +))

(define (scale-vector s)
  (elementwise (lambda (x) (* x s))))

(define (map-streams f s)
  (cons (f (head s))
        (delay (map-streams f (tail s)))))

(define head car)
(define (tail stream)
  (force (cdr stream)))

(define (damped-oscillator R L C)
  (lambda (state)
    (let ((Vc (vector-ref state 0))
          (Il (vector-ref state 1)))
      (vector (- 0 (+ (/ Vc (* R C)) (/ Il C)))
              (/ Vc L)))))

(define the-states
  (integrate-system
   (damped-oscillator 10000 1000 .001)
   '#(1 0)
   .01))

(assert (head the-states))
(assert (head (tail the-states)))
(assert (head (tail (tail the-states))))
