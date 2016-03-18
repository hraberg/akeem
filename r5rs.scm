;;; 4.3.2. Pattern language

(define (syntax-literal? literals pattern)
  (if (null? literals)
      #f
      (if (eq? (car literals) pattern)
          #t
          (syntax-literal? (cdr literals) pattern))))

(define (syntax-pattern-variable? literals pattern)
  (if (symbol? pattern)
      (if (eq? '... pattern)
          #f
          (not (syntax-literal? literals pattern)))
      #f))

(define (match-syntax-rule literals pattern form match)
  (if (null? pattern)
      (if (null? form)
          match
          #f)
      (if (null? form)
          #f
          (let* ((first-pattern (car pattern))
                 (rest-pattern (cdr pattern))
                 (first-form (car form))
                 (rest-form (cdr form)))
            (if (pair? first-pattern)
                (let* ((match (match-syntax-rule literals first-pattern first-form match)))
                  (if match
                      (match-syntax-rule literals rest-pattern rest-form match)
                      #f))
                (if (syntax-pattern-variable? literals first-pattern)
                    (if (null? rest-pattern)
                        (match-syntax-rule literals rest-pattern rest-form
                                           (cons (cons first-pattern first-form) match))
                        (if (eq? '... (car rest-pattern))
                            (cons (cons first-pattern form) match)
                            (match-syntax-rule literals rest-pattern rest-form
                                               (cons (cons first-pattern first-form) match))))
                    (if (eq? first-pattern first-form)
                        (match-syntax-rule literals rest-pattern rest-form match)
                        #f)))))))

(define (syntax-transcribe match template)
  (if (null? match)
      template
      (if (eq? (car (car match)) template)
          (cdr (car match))
          (syntax-transcribe (cdr match) template))))

(define (transcribe-syntax-rule match template)
  (if (null? template)
      '()
      (let* ((first-template (car template))
             (rest-template (cdr template)))
        (if (pair? first-template)
            (cons (transcribe-syntax-rule match first-template)
                  (transcribe-syntax-rule match rest-template))
            (let* ((transcribed (syntax-transcribe match first-template)))
              (if (null? rest-template)
                  (cons transcribed '())
                  (if (eq? '...  (car rest-template))
                      transcribed
                      (cons transcribed (transcribe-syntax-rule match rest-template)))))))))

(define (transform-syntax-rules literals syntax-rules form)
  (if (null? syntax-rules)
      '(begin)
      (let* ((syntax-rule (car syntax-rules))
             (pattern (cdr (car syntax-rule)))
             (template (cdr syntax-rule))
             (match (match-syntax-rule literals pattern form '())))
        (if match
            (cons 'begin (transcribe-syntax-rule match template))
            (transform-syntax-rules literals (cdr syntax-rules) form)))))

(define (transform-syntax transformer-spec form)
  (let* ((literals (car (cdr transformer-spec)))
         (syntax-rules (cdr (cdr transformer-spec)))
         (expansion (transform-syntax-rules literals syntax-rules (cdr form))))
    expansion))

(define-syntax syntax-rules
  (lambda (transformer-spec)
    (cons 'lambda (cons (cons 'form '())
                        (cons (cons 'transform-syntax
                                    (cons (cons 'quote (cons transformer-spec '()))
                                          (cons 'form '())))
                              '())))))

;;; 7.3. Derived expression types

;; (define-syntax cond
;;   (syntax-rules (else =>)
;;     ((cond (else result1 result2 ...))
;;      (begin result1 result2 ...))
;;     ((cond (test => result))
;;      (let ((temp test))
;;        (if temp (result temp))))
;;     ((cond (test => result) clause1 clause2 ...)
;;      (let ((temp test))
;;        (if temp
;;            (result temp)
;;            (cond clause1 clause2 ...))))
;;     ((cond (test)) test)
;;     ((cond (test) clause1 clause2 ...)
;;      (let ((temp test))
;;        (if temp
;;            temp
;;            (cond clause1 clause2 ...))))
;;     ((cond (test result1 result2 ...))
;;      (if test (begin result1 result2 ...)))
;;     ((cond (test result1 result2 ...)
;;            clause1 clause2 ...)
;;      (if test
;;          (begin result1 result2 ...)
;;          (cond clause1 clause2 ...)))))

;; (define-syntax case
;;   (syntax-rules (else)
;;     ((case (key ...)
;;        clauses ...)
;;      (let ((atom-key (key ...)))
;;        (case atom-key clauses ...)))
;;     ((case key
;;        (else result1 result2 ...))
;;      (begin result1 result2 ...))
;;     ((case key
;;        ((atoms ...) result1 result2 ...))
;;      (if (memv key '(atoms ...))
;;          (begin result1 result2 ...)))
;;     ((case key
;;        ((atoms ...) result1 result2 ...)
;;        clause clauses ...)
;;      (if (memv key '(atoms ...))
;;          (begin result1 result2 ...)
;;          (case key clause clauses ...)))))

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

;;; 4.2.6. Quasiquotation

;; Based on https://github.com/mishoo/SLip/blob/master/lisp/compiler.lisp#L25
(define (qq x)
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
        (else (cons 'quote (cons x '())))))

(define-syntax quasiquote
  (lambda (qq-template)
    (qq (cadr qq-template))))

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
  (do ((list list (cdr list))
       (k k (- k 1)))
      ((zero? k) list)))

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
