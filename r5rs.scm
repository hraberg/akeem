;;; 4.3. Macros

;;; 4.3.2. Pattern language

(set! equal? (lambda (obj1 obj2)
               (if (pair? obj1)
                   (if (pair? obj2)
                       (if (equal? (car obj1) (car obj2))
                           (equal? (cdr obj1) (cdr obj2))
                           #f)
                       #f)
                   (eqv? obj1 obj2))))

(set! memv
      (lambda (obj list)
        (let loop ((list list))
          (if (null? list)
              #f
              (if (eqv? (car list) obj)
                  list
                  (loop (cdr list)))))))

(set! syntax-pattern-variable?
      (lambda (literals pattern)
        (if (symbol? pattern)
            (not (memv pattern (cons '... literals)))
            #f)))

(set! match-syntax-rule
      (lambda (literals pattern form match idxs env)
        (if (null? pattern)
            (if (null? form)
                match
                #f)
            (let ((first-pattern (car pattern))
                  (rest-pattern (cdr pattern)))
              (if (pair? first-pattern)
                  (if (null? form)
                      #f
                      (if (pair? (car form))
                          (let ((default-match
                                  (lambda ()
                                    (let ((match (match-syntax-rule literals first-pattern (car form) match idxs env)))
                                      (if match
                                          (match-syntax-rule literals rest-pattern (cdr form) match idxs env)
                                          #f)))))
                            (if (null? rest-pattern)
                                (default-match)
                                (if (eq? '... (car rest-pattern))
                                    (let loop ((form form)
                                               (match match)
                                               (idx 0))
                                      (if (null? form)
                                          match
                                          (let ((match (match-syntax-rule literals first-pattern (car form) match (cons idx idxs) env)))
                                            (if match
                                                (loop (cdr form) match (+ 1 idx))
                                                #f))))
                                    (default-match))))
                          #f))
                  (if (syntax-pattern-variable? literals first-pattern)
                      (if (null? rest-pattern)
                          (match-syntax-rule literals rest-pattern (cdr form)
                                             (cons (cons (car form) (cons first-pattern idxs)) match) idxs env)
                          (if (eq? '... (car rest-pattern))
                              (if (null? form)
                                  (cons (cons 'transcribe-failure (cons first-pattern (cons 0 idxs))) match)
                                  (let loop ((form form)
                                             (match match)
                                             (idx 0))
                                    (if (null? form)
                                        match
                                        (loop (cdr form)
                                              (cons (cons (car form) (cons first-pattern (cons idx idxs))) match)
                                              (+ 1 idx)))))
                              (match-syntax-rule literals rest-pattern (cdr form)
                                                 (cons (cons (car form) (cons first-pattern idxs)) match) idxs env)))
                      (if (equal? first-pattern (car form))
                          (if (memv first-pattern env)
                              #f
                              (match-syntax-rule literals rest-pattern (cdr form) match idxs env))
                          #f)))))))

(set! syntax-template-pattern-variable?
      (lambda (match template)
        (let loop ((match match))
          (if (null? match)
              #f
              (if (eqv? (car (cdr (car match))) template)
                  #t
                  (loop (cdr match)))))))

(set! transcribe-syntax-template
      (lambda (match template-idxs)
        (let loop ((match match))
          (if (null? match)
              'transcribe-failure
              (if (equal? (cdr (car match)) template-idxs)
                  (car (car match))
                  (loop (cdr match)))))))

(set! transcribe-syntax-rule
      (lambda (match template idxs)
        (if (not (pair? template))
            (if (syntax-template-pattern-variable? match template)
                (transcribe-syntax-template match (cons template idxs))
                template)
            (let* ((first-template (car template))
                   (rest-template (cdr template))
                   (default-transcribe
                     (lambda ()
                       (let ((first-new-transcribed (transcribe-syntax-rule match first-template idxs))
                             (rest-new-transcribed (transcribe-syntax-rule match rest-template idxs)))
                         (if (eq? 'transcribe-failure first-new-transcribed)
                             'transcribe-failure
                             (if (eq? 'transcribe-failure rest-new-transcribed)
                                 'transcribe-failure
                                 (cons first-new-transcribed rest-new-transcribed)))))))
              (if (null? rest-template)
                  (default-transcribe)
                  (if (eq? '...  (car rest-template))
                      (append
                       (let loop ((transcribed '())
                                  (new-idx 0))
                         (let ((new-transcribed (transcribe-syntax-rule match first-template (cons new-idx idxs))))
                           (if (eq? 'transcribe-failure new-transcribed)
                               transcribed
                               (loop (append transcribed (cons new-transcribed '()))
                                     (+ new-idx 1)))))
                       (transcribe-syntax-rule match (cdr rest-template) idxs))
                      (default-transcribe)))))))

(set! transform-syntax-rules
      (lambda (literals syntax-rules form env)
        (if (null? syntax-rules)
            '(begin)
            (let* ((syntax-rule (car syntax-rules))
                   (pattern (cdr (car syntax-rule)))
                   (template (cdr syntax-rule))
                   (match (match-syntax-rule literals pattern form '() '() env)))
              (if match
                  (cons 'begin (transcribe-syntax-rule (reverse match) template '()))
                  (transform-syntax-rules literals (cdr syntax-rules) form env))))))

(set! transform-syntax
      (lambda (transformer-spec form env)
        (let ((literals (car (cdr transformer-spec)))
              (syntax-rules (cdr (cdr transformer-spec))))
          (transform-syntax-rules literals syntax-rules (cdr form) env))))

(define-syntax syntax-rules
  (lambda (transformer-spec)
    (cons 'lambda (cons (cons 'form (cons 'env '()))
                        (cons (cons 'transform-syntax
                                    (cons (cons 'quote (cons transformer-spec '()))
                                          (cons 'form (cons 'env '()))))
                              '())))))

;;; 4.2. Derived expression types

;;; 4.2.1. Conditionals

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

;;; 4.2.4. Iteration

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (let loop ((var init) ...)
       (if test
           (begin
             (if #f #f)
             expr ...)
           (begin
             command
             ...
             (loop (do "step" var step ...)
                   ...)))))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

;;; 4.2.5. Delayed evaluation

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (make-promise (lambda () expression)))))

;;; 4.2.6. Quasiquotation

;; Based on https://github.com/mishoo/SLip/blob/master/lisp/compiler.lisp#L25
(define-syntax quasiquote
  (lambda (qq-template)
    (letrec ((qq (lambda (x)
                   (cond ((pair? x)
                          (case (car x)
                            ((unquote) (cadr x))
                            ((quasiquote) (qq (qq (cadr x))))
                            (else
                             (if (and (pair? (car x))
                                      (eq? 'unquote-splicing (caar x)))
                                 (cons 'append (cons (cadar x) (cons (qq (cdr x)) '())))
                                 (cons 'cons (cons (qq (car x)) (cons (qq (cdr x)) '())))))))
                         ((vector? x)
                          (cons 'list->vector (cons (qq (vector->list x)) '())))
                         (else (cons 'quote (cons x '())))))))
      (qq (cadr qq-template)))))

;;; 5.2. Definitions

(define-syntax define
  (syntax-rules ()
    ((define (variable formals ...) body ...)
     (define variable (lambda (formals ...) body ...)))
    ((define variable expression)
     (set! variable expression))))

;;; 6.1. Equivalence predicates

(define (equal? obj1 obj2)
  (cond ((and (pair? obj1) (pair? obj2))
         (and (equal? (car obj1) (car obj2))
              (equal? (cdr obj1) (cdr obj2))))
        ((and (string? obj1) (string? obj2))
         (string=? obj1 obj2))
        ((and (vector? obj1) (vector? obj2))
         (equal? (vector->list obj1) (vector->list obj2)))
        (else (eqv? obj1 obj2))))

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

(define (min-max-aux test x1 x2)
  (if (test x1 x2)
      (if (inexact? x1)
          (exact->inexact x2)
          x2)
      (if (inexact? x2)
          (exact->inexact x1)
          x1)))

(define (max x1 x2)
  (min-max-aux < x1 x2))

(define (min x1 x2)
  (min-max-aux > x1 x2))

(define (abs x)
  (if (negative? x)
      (- 0 x)
      x))

(define (gcd n1 n2)
  (if (zero? n2)
      (abs n1)
      (gcd n2 (modulo n1 n2))))

(define (lcm n1 n2)
  (/ (abs (* n1 n2))
     (gcd n1 n2)))

;;; 6.3.1. Booleans

(define (boolean? obj)
  (or (eq? #t obj) (eq? #f obj)))

;;; 6.3.2. Pairs and lists

(define (caar obj)
  (car (car obj)))

(define (cadr obj)
  (car (cdr obj)))

(define (cdar obj)
  (cdr (car obj)))

(define (cddr obj)
  (cdr (cdr obj)))

(define (caaar obj)
  (car (caar obj)))

(define (caadr obj)
  (car (cadr obj)))

(define (cadar obj)
  (car (cdar obj)))

(define (caddr obj)
  (car (cddr obj)))

(define (cdaar obj)
  (cdr (caar obj)))

(define (cdadr obj)
  (cdr (cadr obj)))

(define (cddar obj)
  (cdr (cdar obj)))

(define (cdddr obj)
  (cdr (cddr obj)))

(define (caaaar obj)
  (car (caaar obj)))

(define (caaadr obj)
  (car (caadr obj)))

(define (caadar obj)
  (car (cadar obj)))

(define (caaddr obj)
  (car (caddr obj)))

(define (cadaar obj)
  (car (cdaar obj)))

(define (cadadr obj)
  (car (cdadr obj)))

(define (caddar obj)
  (car (cddar obj)))

(define (cadddr obj)
  (car (cdddr obj)))

(define (cdaaar obj)
  (cdr (caaar obj)))

(define (cdaadr obj)
  (cdr (caadr obj)))

(define (cdadar obj)
  (cdr (cadar obj)))

(define (cdaddr obj)
  (cdr (caddr obj)))

(define (cddaar obj)
  (cdr (cdaar obj)))

(define (cddadr obj)
  (cdr (cdadr obj)))

(define (cdddar obj)
  (cdr (cddar obj)))

(define (cddddr obj)
  (cdr (cdddr obj)))

(define (list? obj)
  (let loop ((obj obj))
    (cond ((null? obj) #t)
          ((pair? obj) (loop (cdr obj)))
          (else #f))))

(define (list-tail list k)
  (let loop ((list list)
             (k k))
    (if (zero? k)
        list
        (loop (cdr list) (- k 1)))))

(define (list-ref list k)
  (car (list-tail list k)))

(define (member-aux comparator obj list)
  (let loop ((list list))
    (cond ((null? list) #f)
          ((comparator obj (car list)) list)
          (else (loop (cdr list))))))

(define (memq obj list)
  (member-aux eq? obj list))

(define (memv obj list)
  (member-aux eqv? obj list))

(define (member obj list)
  (member-aux equal? obj list))

(define (assoc-aux comparator obj alist)
  (let loop ((alist alist))
    (cond ((null? alist) #f)
          ((comparator obj (caar alist)) (car alist))
          (else (loop (cdr alist))))))

(define (assq obj alist)
  (assoc-aux eq? obj alist))

(define (assv obj alist)
  (assoc-aux eqv? obj alist))

(define (assoc obj alist)
  (assoc-aux equal? obj alist))

;;; 6.3.4. Characters

(define (char=? char1 char2)
  (= (char->integer char1) (char->integer char2)))

(define (char<? char1 char2)
  (< (char->integer char1) (char->integer char2)))

(define (char>? char1 char2)
  (> (char->integer char1) (char->integer char2)))

(define (char<=? char1 char2)
  (<= (char->integer char1) (char->integer char2)))

(define (char>=? char1 char2)
  (>= (char->integer char1) (char->integer char2)))

(define (char-ci=? char1 char2)
  (= (char->integer (char-downcase char1)) (char->integer (char-downcase char2))))

(define (char-ci<? char1 char2)
  (< (char->integer (char-downcase char1)) (char->integer (char-downcase char2))))

(define (char-ci>? char1 char2)
  (> (char->integer (char-downcase char1)) (char->integer (char-downcase char2))))

(define (char-ci<=? char1 char2)
  (<= (char->integer (char-downcase char1)) (char->integer (char-downcase char2))))

(define (char-ci>=? char1 char2)
  (>= (char->integer (char-downcase char1)) (char->integer (char-downcase char2))))

;;; 6.3.5. Strings

(define (string->list string)
  (do ((list '() (cons (string-ref string idx) list))
       (idx (- (string-length string) 1) (- idx 1)))
      ((negative? idx) list)))

(define (substring string start end)
  (do ((copy (make-string (- end start)))
       (idx start (+ idx 1)))
      ((= idx end) copy)
    (string-set! copy (- idx start) (string-ref string idx))))

(define (string-append string1 string2)
  (list->string (append (string->list string1) (string->list string2))))

(define (list->string list)
  (do ((list list (cdr list))
       (string (make-string (length list)))
       (idx 0 (+ idx 1)))
      ((null? list) string)
    (string-set! string idx (car list))))

(define (string-copy string)
  (list->string (string->list string)))

(define (string-fill! string fill)
  (do ((idx (- (string-length string) 1) (- idx 1)))
      ((negative? idx) string)
    (string-set! string idx fill)))

;;; 6.3.6. Vectors

(define (vector->list vector)
  (do ((list '() (cons (vector-ref vector idx) list))
       (idx (- (vector-length vector) 1) (- idx 1)))
      ((negative? idx) list)))

(define (vector-fill! vector fill)
  (do ((idx (- (vector-length vector) 1) (- idx 1)))
      ((negative? idx) vector)
    (vector-set! vector idx fill)))

;;; 6.4. Control features

(define (map proc list)
  (do ((list list (cdr list))
       (acc '() (cons (proc (car list)) acc)))
      ((null? list) (reverse acc))))

(define (for-each proc list)
  (do ((list list (cdr list)))
      ((null? list) acc)
    (proc (car list))))

(define force
  (lambda (object)
    (object)))

(define make-promise
  (lambda (proc)
    (let ((result (make-vector 2 #f)))
      (lambda ()
        (if (vector-ref result 0)
            (vector-ref result 1)
            (let ((x (proc)))
              (if (vector-ref result 0)
                  (vector-ref result 1)
                  (begin (vector-set! result 0 #t)
                         (vector-set! result 1 x)
                         (vector-ref result 1)))))))))

(define (dynamic-wind before thunk after)
  (before)
  (let ((return (thunk)))
    (after)
    return))
