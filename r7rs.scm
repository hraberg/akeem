;;; R7RS

;;; 4. Expressions

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
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
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

(define-syntax cond-expand
  (syntax-rules (and or not else)
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
      (req1
       (cond-expand
        ((and req2 ...) body ...)
        more-clauses ...))
      more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
      (req1
       (begin body ...))
      (else
       (cond-expand
        ((or req2 ...) body ...)
        more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
      (req
       (cond-expand more-clauses ...))
      (else body ...)))
    ((cond-expand (feature-id body ...))
     (if (memv 'feature-id (features))
         (begin body ...)))
    ((cond-expand (feature-id body ...) more-clauses ...)
     (if (memv 'feature-id (features))
         (begin body ...)
         (cond-expand more-clauses ...)))))

;;; 4.2.2. Binding constructs

(define-syntax r7rs-let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

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

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-promise #f (lambda () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise #t expression)))))

(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((promise* ((promise-value promise))))
        (unless (promise-done? promise)
          (promise-update! promise* promise))
        (force promise))))

(define promise-update!
  (lambda (new old)
    (set-promise-done! old (promise-done? new))
    (set-promise-value! old (promise-value new))
    (set-promise-value! new (promise-value old))
    (set-promise-done! new (promise-done? old))))

;;; 4.2.6. Dynamic bindings

(define (make-parameter init . o)
  (let* ((converter
          (if (pair? o) (car o) (lambda (x) x)))
         (value (cons (converter init) '())))
    (lambda args
      (cond
       ((null? args)
        (car value))
       ((eq? (car args) '<param-set!>)
        (set-car! value (cadr args)))
       ((eq? (car args) '<param-convert>)
        converter)
       ((eq? 1 (length args))
        (set-car! value (converter (car args))))
       (else
        (error "bad parameter syntax"))))))

(define-syntax parameterize
  (lambda (form env)
    (let ((bindings (cadr form))
          (body (cddr form)))
      `(let ((old (list ,@(map (lambda (p)
                                 `(cons (,(car p)) ,(car p)))
                               bindings))))
         (dynamic-wind
           (lambda ()
             ,@(map (lambda (p)
                      `(,(car p) '<param-set!> ((,(car p) '<param-convert>) ,(cadr p))))
                    bindings))
           (lambda ()
             ,@body)
           (lambda ()
             (for-each (lambda (p)
                         ((cdr p) '<param-set!> (car p)))
                       old)))))))

;;; 4.2.7. Exception handling

(define-syntax guard
  (lambda (form env)
    (let ((condition (caadr form))
          (guard (cdadr form))
          (body (cddr form)))
      `(call/cc (lambda (continue)
                  (with-exception-handler
                   (lambda (,condition)
                     (let ((result (cond ,@guard)))
                       (if result
                           (continue result)
                           (raise ,condition))))
                   (lambda ()
                     ,@body)))))))

;;; 4.2.9. Case-lambda

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 ...) ...)
     (lambda args
       (let ((len (length args)))
         (case-lambda len (params body0 ...) ...))))
    ((case-lambda len)
     (error "no matching clause"))
    ((case-lambda len ((p ...) . body) . rest)
     (if (= len (length '(p ...)))
         (apply (lambda (p ...)
                  . body)
                args)
         (case-lambda len . rest)))
    ((case-lambda len ((p ... . tail) . body)
                  . rest)
     (if (>= len (length '(p ...)))
         (apply
          (lambda (p ... . tail)
            . body)
          args)
         (case-lambda len . rest)))))

;;; 5.5. Record-type definitions

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type name
       (constructor fields ...)
        pred
        (field-name accessor modifier ...) ...)

     (define (constructor fields ...)
       (make-record
        (let ((arguments (list (cons 'fields fields) ...)))
          (map (lambda (x)
                 (let ((v (assoc x arguments)))
                   (if v (cdr v) #f)))
               '(field-name ...)))
        'name))

     (define (pred obj)
       (eq? 'name (class-of obj)))

     (let ((fs '(field-name ...))
           (type 'name))
       (define-record-type "field" fs type (field-name accessor modifier ...)) ...))

    ((define-record-type "field" fields type (field-name accessor))
     (let ((field-idx (- (length fields) (length (memq 'field-name fields)))))
       (define (accessor record)
         (if (eq? type (class-of record))
             (record-ref record field-idx)
             (error (string-append "Not a " (symbol->string type) ":") record)))))

    ((define-record-type "field" fields type (field-name accessor modifier))
     (define-record-type "field" fields type (field-name accessor))
     (let ((field-idx (- (length fields) (length (memq 'field-name fields)))))
       (define (modifier record value)
         (if (eq? type (class-of record))
             (record-set! record field-idx value)
             (error (string-append "Not a " (symbol->string type) ":") record)))))))

;;; 6. Standard procedures

;;; 6.1. Equivalence predicates

(define (pair=? pair1 pair2)
  (let loop ((pair1 pair1)
             (pair2 pair2))
    (if (and (pair? pair1) (pair? pair2))
        (and (equal? (car pair1) (car pair2))
             (loop (cdr pair1) (cdr pair2)))
        (equal? pair1 pair2))))

(define (vector=? vector1 vector2)
  (and (= (vector-length vector1) (vector-length vector2))
       (let loop ((idx (- (vector-length vector1) 1)))
         (cond ((negative? idx) #t)
               ((equal? (vector-ref vector1 idx)
                        (vector-ref vector2 idx))
                (loop (- idx 1)))
               (else #f)))))

(define (bytevector=? bytevector1 bytevector2)
  (and (= (bytevector-length bytevector1) (bytevector-length bytevector2))
       (let loop ((idx (- (bytevector-length bytevector1) 1)))
         (cond ((negative? idx) #t)
               ((eq? (bytevector-u8-ref bytevector1 idx)
                     (bytevector-u8-ref bytevector2 idx))
                (loop (- idx 1)))
               (else #f)))))

(define (equal? obj1 obj2)
  (cond ((and (pair? obj1) (pair? obj2))
         (pair=? obj1 obj2))
        ((and (string? obj1) (string? obj2))
         (string=? obj1 obj2))
        ((and (vector? obj1) (vector? obj2))
         (vector=? obj1 obj2))
        ((and (bytevector? obj1) (bytevector? obj2))
         (bytevector=? obj1 obj2))
        (else (eqv? obj1 obj2))))

;;; 6.2. Numbers

;;; 6.2.6. Numerical operations

(define (exact-integer? z)
  (and (integer? z) (exact? z)))

(define (finite? z)
  (and (number? z)
       (not (or (nan? z) (infinite? z)))))

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
          (inexact x2)
          x2)
      (if (inexact? x2)
          (inexact x1)
          x1)))

(define (max x1 x2)
  (min-max-aux < x1 x2))

(define (min x1 x2)
  (min-max-aux > x1 x2))

(define (abs x)
  (if (negative? x)
      (- 0 x)
      x))

(define (floor/ n1 n2)
  (let* ((nq (floor (/ n1 n2)))
         (nr (- n1 (* nq n2))))
    (if (and (exact? n1) (exact? n2))
        (cons (exact nq) (exact nr))
        (cons (inexact nq) (inexact nr)))))

(define (floor-quotient n1 n2)
  (floor (quotient n1 n2)))

(define floor-remainder modulo)

(define (truncate/ n1 n2)
  (let* ((nq (truncate (/ n1 n2)))
         (nr (- n1 (* nq n2))))
    (if (and (exact? n1) (exact? n2))
        (cons (exact nq) (exact nr))
        (cons (inexact nq) (inexact nr)))))

(define truncate-quotient quotient)

(define truncate-remainder remainder)

(define gcd
  (case-lambda
   (() 0)
   ((n1 n2)
    (if (zero? n2)
        (abs n1)
        (gcd n2 (modulo n1 n2))))))

(define lcm
  (case-lambda
   (() 1)
   ((n1 n2)
    (/ (abs (* n1 n2))
       (gcd n1 n2)))))

(define atan1 atan)

(define atan
  (case-lambda
   ((z)
    (atan1 z))
   ((y x)
    (atan2 y x))))

(define (square z)
  (* z z))

(define (exact-integer-sqrt k)
  (let* ((s (exact (sqrt k)))
         (r (exact (- k (* s s)))))
    (cons s r)))

;;; 6.3. Booleans

(define (boolean? obj)
  (or (eq? #t obj) (eq? #f obj)))

(define boolean=? eqv?)

;;; 6.4. Pairs and lists

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

(define make-list
  (case-lambda
   ((k)
    (make-list k (if #f #f)))
   ((k fill)
    (do ((acc '() (cons fill acc))
         (idx 0 (+ idx 1)))
        ((= idx k) acc)))))

(define (list . obj)
  obj)

(define append-internal append)

(define (append . lists)
  (cond ((null? lists)
         '())
        ((null? (cdr lists))
         (car lists))
        (else
         (append-internal (car lists)
                          (apply append (cdr lists))))))

(define (list-tail list k)
  (let loop ((list list)
             (k k))
    (if (zero? k)
        list
        (loop (cdr list) (- k 1)))))

(define (list-ref list k)
  (car (list-tail list k)))

(define (list-set! list k obj)
  (set-car! (list-tail list k) obj))

(define member
  (case-lambda
   ((obj list)
    (member obj list equal?))
   ((obj list compare)
    (let loop ((list list))
      (cond ((null? list) #f)
            ((compare obj (car list)) list)
            (else (loop (cdr list))))))))

(define (memq obj list)
  (member obj list eq?))

(define (memv obj list)
  (member obj list eqv?))

(define (assq obj alist)
  (assoc obj alist eq?))

(define (assv obj alist)
  (assoc obj alist eqv?))

(define assoc
  (case-lambda
   ((obj list)
    (assoc obj list equal?))
   ((obj alist compare)
    (let loop ((alist alist))
      (cond ((null? alist) #f)
            ((compare obj (caar alist)) (car alist))
            (else (loop (cdr alist))))))))

(define (list-copy obj)
  (map (lambda (x) x) obj))

;;; 6.5. Symbols

(define (symbol=? symbol1 symbol2)
  (equal? (symbol->string symbol1) (symbol->string symbol2)))

;;; 6.6. Characters

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

(define (digit-value char)
  (if (char-numeric? char)
      (- (char->integer char) (char->integer #\0))
      #f))

(define char-foldcase char-downcase)

;;; 6.7. Strings

(define (string . char)
  (list->string char))

(define (string-upcase string)
  (string-map char-upcase string))

(define (string-downcase string)
  (string-map char-downcase string))

(define (string-foldcase string)
  (string-map char-foldcase string))

(define (substring string start end)
  (do ((copy (make-string (- end start)))
       (idx start (+ idx 1)))
      ((= idx end) copy)
    (string-set! copy (- idx start) (string-ref string idx))))

(define (string-append . strings)
  (if (null? strings)
      ""
      (let* ((string1 (car strings))
             (string2 (apply string-append (cdr strings)))
             (length1 (string-length string1))
             (length2 (string-length string2))
             (acc (make-string (+ length1 length2))))
        (do ((idx 0 (+ idx 1)))
            ((= idx length1))
          (string-set! acc idx (string-ref string1 idx)))
        (do ((idx 0 (+ idx 1)))
            ((= idx length2))
          (string-set! acc (+ idx length1) (string-ref string2 idx)))
        acc)))

(define string->list
  (case-lambda
   ((string)
    (string->list string 0))
   ((string start)
    (string->list string start (string-length string)))
   ((string start end)
    (do ((list '() (cons (string-ref string idx) list))
         (idx (- end 1) (- idx 1)))
        ((< idx start) list)))))

(define (list->string list)
  (do ((list list (cdr list))
       (string (make-string (length list)))
       (idx 0 (+ idx 1)))
      ((null? list) string)
    (string-set! string idx (car list))))

(define string-copy
  (case-lambda
   ((string)
    (string-copy string 0))
   ((string start)
    (string-copy string start (string-length string)))
   ((string start end)
    (string-copy! (make-string (- end start) #\space) 0 string start end))))

(define string-copy!
  (case-lambda
   ((to at from)
    (string-copy! to at from 0))
   ((to at from start)
    (string-copy! to at start (string-length string)))
   ((to at from start end)
    (do ((idx start (+ idx 1)))
        ((= idx end) to)
      (string-set! to (- (+ idx at) start) (string-ref from idx))))))

(define string-fill!
  (case-lambda
   ((string fill)
    (string-fill! string fill 0))
   ((string fill start)
    (string-fill! string fill start (string-length string)))
   ((string fill start end)
    (do ((idx start (+ idx 1)))
        ((= idx end) string)
      (string-set! string idx fill)))))

;;; 6.8. Vectors

(define make-vector-internal make-vector)

(define make-vector
  (case-lambda
   ((k)
    (make-vector k (if #f #f)))
   ((k fill)
    (make-vector-internal k fill))))

(define (vector . obj)
  (list->vector obj))

(define vector->list
  (case-lambda
   ((vector)
    (vector->list vector 0))
   ((vector start)
    (vector->list vector start (vector-length vector)))
   ((vector start end)
    (do ((list '() (cons (vector-ref vector idx) list))
         (idx (- end 1) (- idx 1)))
        ((< idx start) list)))))

(define vector-copy
  (case-lambda
   ((vector)
    (vector-copy vector 0))
   ((vector start)
    (vector-copy vector start (vector-length vector)))
   ((vector start end)
    (vector-copy! (make-vector (- end start) 0) 0 vector start end))))

(define vector-copy!
  (case-lambda
   ((to at from)
    (vector-copy! to at from 0))
   ((to at from start)
    (vector-copy! to at start (vector-length vector)))
   ((to at from start end)
    (do ((idx start (+ idx 1)))
        ((= idx end) to)
      (vector-set! to (- (+ idx at) start) (vector-ref from idx))))))

(define vector->string
  (case-lambda
   ((vector)
    (vector->string vector 0))
   ((vector start)
    (vector->string vector start (vector-length vector)))
   ((vector start end)
    (do ((acc (make-string (- end start) #\space))
         (idx start (+ idx 1)))
        ((= idx end) acc)
      (string-set! acc (- idx start) (vector-ref vector idx))))))

(define string->vector
  (case-lambda
   ((string)
    (string->vector string 0))
   ((string start)
    (string->vector string start (string-length string)))
   ((string start end)
    (do ((acc (make-vector (- end start)))
         (idx start (+ idx 1)))
        ((= idx end) acc)
      (vector-set! acc (- idx start) (string-ref string idx))))))

(define (vector-append . vectors)
  (if (null? vectors)
      #()
      (let* ((vector1 (car vectors))
             (vector2 (apply vector-append (cdr vectors)))
             (length1 (vector-length vector1))
             (length2 (vector-length vector2))
             (acc (make-vector (+ length1 length2))))
        (do ((idx 0 (+ idx 1)))
            ((= idx length1))
          (vector-set! acc idx (vector-ref vector1 idx)))
        (do ((idx 0 (+ idx 1)))
            ((= idx length2))
          (vector-set! acc (+ idx length1) (vector-ref vector2 idx)))
        acc)))

(define vector-fill!
  (case-lambda
   ((vector fill)
    (vector-fill! vector fill 0))
   ((vector fill start)
    (vector-fill! vector fill start (vector-length vector)))
   ((vector fill start end)
    (do ((idx start (+ idx 1)))
        ((= idx end) vector)
      (vector-set! vector idx fill)))))

;;; 6.9. Bytevectors

(define (bytevector . byte)
  (list->bytevector byte))

(define bytevector-copy
  (case-lambda
   ((bytevector)
    (bytevector-copy bytevector 0))
   ((bytevector start)
    (bytevector-copy bytevector start (bytevector-length bytevector)))
   ((bytevector start end)
    (bytevector-copy! (make-bytevector (- end start) 0) 0 bytevector start end))))

(define bytevector-copy!
  (case-lambda
   ((to at from)
    (bytevector-copy! to at from 0))
   ((to at from start)
    (bytevector-copy! to at start (bytevector-length bytevector)))
   ((to at from start end)
    (do ((idx start (+ idx 1)))
        ((= idx end) to)
      (bytevector-u8-set! to (- (+ idx at) start) (bytevector-u8-ref from idx))))))

(define (bytevector-append . bytevectors)
  (if (null? bytevectors)
      (bytevector)
      (let* ((bytevector1 (car bytevectors))
             (bytevector2 (apply bytevector-append (cdr bytevectors)))
             (length1 (bytevector-length bytevector1))
             (length2 (bytevector-length bytevector2))
             (acc (make-bytevector (+ length1 length2))))
        (do ((idx 0 (+ idx 1)))
            ((= idx length1))
          (bytevector-u8-set! acc idx (bytevector-u8-ref bytevector1 idx)))
        (do ((idx 0 (+ idx 1)))
            ((= idx length2))
          (bytevector-u8-set! acc (+ idx length1) (bytevector-u8-ref bytevector2 idx)))
        acc)))

(define utf8->string
  (case-lambda
   ((bytevector)
    (utf8->string bytevector 0))
   ((bytevector start)
    (utf8->string bytevector start (bytevector-length bytevector)))
   ((bytevector start end)
    (do ((acc (make-string (- end start)))
         (idx start (+ idx 1)))
        ((= idx end) acc)
      (string-set! acc (- idx start) (integer->char (bytevector-u8-ref bytevector idx)))))))

(define string->utf8
  (case-lambda
   ((string)
    (string->utf8 string 0))
   ((string start)
    (string->utf8 string start (string-length string)))
   ((string start end)
    (do ((acc (make-bytevector (- end start)))
         (idx start (+ idx 1)))
        ((= idx end) acc)
      (bytevector-u8-set! acc (- idx start) (char->integer (string-ref string idx)))))))

;;; 6.10. Control features

(define apply-internal apply)

(define (apply proc . args)
  (let* ((args (reverse args))
         (flat-args (car args)))
    (if (list? flat-args)
        (do ((args (cdr args) (cdr args))
             (flat-args flat-args (cons (car args) flat-args)))
            ((null? args) (apply-internal proc flat-args)))
        (error "Not a list:" flat-args))))

(define (map proc list)
  (let* ((length (length list))
         (acc (make-list length)))
    (do ((from list (cdr from))
         (to acc (cdr to)))
        ((null? from) acc)
      (set-car! to (proc (car from))))))

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

(define (for-each proc list)
  (do ((list list (cdr list)))
      ((null? list))
    (proc (car list))))

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

(define-record-type promise
  (make-promise done? value)
  promise?
  (done? promise-done? set-promise-done!)
  (value promise-value set-promise-value!))

(define (values . things)
  (lambda (cont) (apply cont things)))

(define (call-with-values producer consumer)
  (let ((producer (producer)))
    ((if (procedure? producer)
         producer
         (values producer)) consumer)))

(define dynamic-extent-stack (make-parameter '()))

(define (dynamic-wind before thunk after)
  (before)
  (let ((old-dynamic-extent-stack (dynamic-extent-stack))
        (new-dynamic-extent-stack (dynamic-extent-stack '<param-set!>
                                                        ((dynamic-extent-stack '<param-convert>)
                                                         (cons before (dynamic-extent-stack)))))
        (return (thunk)))
    (dynamic-extent-stack '<param-set!> old-dynamic-extent-stack)
    (after)
    return))

;;; 6.11. Exceptions

(define (default-exception-handler error)
  (if (error-object? error)
      (begin
        (display (error-object-message error)
                 (current-error-port))
        (for-each (lambda (irritant)
                    (display #\space (current-error-port))
                    (display irritant (current-error-port)))
                  (error-object-irritants error)))
      (display error (current-error-port)))
  (newline (current-error-port)))

(define exception-handler-stack (make-parameter (list default-exception-handler)))

(define exception-handler-continuation (make-parameter (lambda (x))))

(define (parent-exception-handler)
  (let ((stack (exception-handler-stack)))
    (if (null? stack)
        stack
        (cdr stack))))

(define (with-exception-handler handler thunk)
  (parameterize ((exception-handler-stack
                  (cons (lambda (obj)
                          (parameterize ((exception-handler-stack (parent-exception-handler)))
                            ((exception-handler-continuation) (handler obj))
                            (raise obj)))
                        (exception-handler-stack))))
    (thunk)))

(define (raise-continuable obj)
  (call/cc (lambda (continue)
             (parameterize ((exception-handler-continuation continue))
               (raise obj)))))

(define-record-type error-object
  (make-error-object message irritants)
  error-object?
  (message error-object-message)
  (irritants error-object-irritants))

(define (error message . obj)
  (raise (make-error-object message obj)))

;; 6.12. Environments and evaluation

(define (environment . list)
  (interaction-environment))

;; 6.13. Input and output

;; 6.13.1. Ports

(define (call-with-input-file string proc)
  (call-with-port (open-input-file string) proc))

(define (call-with-output-file string proc)
  (call-with-port (open-output-file string) proc))

(define (port? obj)
  (or (input-port? obj) (output-port? obj)))

(define textual-port? port?)
(define binary-port? port?)

(define input-port-open? input-port?)
(define output-port-open? output-port?)

(define current-input-port (make-parameter (current-input-port)))
(define current-output-port (make-parameter (current-output-port)))
(define current-error-port (make-parameter (current-error-port)))

(define (with-input-from-file string thunk)
  (let ((in (open-input-file string)))
    (parameterize ((current-input-port in))
      (dynamic-wind
        (lambda ())
        thunk
        (lambda ()
          (close-input-port in))))))

(define (with-output-to-file string thunk)
  (let ((out (open-output-file string)))
    (parameterize ((current-output-port out))
      (dynamic-wind
        (lambda ())
        thunk
        (lambda ()
          (close-output-port out))))))

(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)

;;; 6.13.2. Input

(define read-line
  (case-lambda
   (()
    (read-line (current-input-port)))
   ((port)
    (do ((acc '() (cons char acc))
         (char (read-char port) (read-char port)))
        ((or (eq? #\newline char)
             (eq? #\return char)
             (eof-object? char)) (list->string (reverse acc)))))))

(define read-string
  (case-lambda
   ((k)
    (read-string k (current-input-port)))
   ((k port)
    (utf8->string (read-bytevector k port) 0 k))))

(define read-bytevector
  (case-lambda
   ((k)
    (read-bytevector k (current-input-port)))
   ((k port)
    (let ((bytevector (make-bytevector k 0)))
      (read-bytevector! bytevector port 0 k)
      bytevector))))

(define read-bytevector!
  (case-lambda
   ((bytevector)
    (read-bytevector! bytevector (current-input-port)))
   ((bytevector port)
    (read-bytevector! bytevector port 0))
   ((bytevector port start)
    (read-bytevector! bytevector port start (bytevector-length bytevector)))
   ((bytevector port start end)
    (do ((idx start (+ idx 1))
         (byte (read-u8 port) (read-u8 port)))
        ((or (= idx end) (eof-object? byte)) (if (= idx start)
                                                 (eof-object)
                                                 idx))
      (bytevector-u8-set! bytevector idx byte)))))

;;; 6.13.3. Output

(define write-string
  (case-lambda
   ((string)
    (write-string string (current-output-port)))
   ((string port)
    (write-string string port 0))
   ((string port start)
    (write-string string port start (string-length string)))
   ((string port start end)
    (write-bytevector (string->utf8 string start end) port start end))))

(define write-bytevector
  (case-lambda
   ((bytevector)
    (write-bytevector bytevector (current-output-port)))
   ((bytevector port)
    (write-bytevector bytevector port 0))
   ((bytevector port start)
    (write-bytevector bytevector port start (bytevector-length bytevector)))
   ((bytevector port start end)
    (do ((idx start (+ idx 1)))
        ((= idx end) (- end start))
      (write-u8 (bytevector-u8-ref bytevector idx) port)))))

;;; 6.14. System interface

(define (get-environment-variable name)
  (cond ((assoc name (get-environment-variables)) => cdr)
        (else #f)))

(define (features)
  '(r7rs exact-closed ieee-float posix unix gnu-linux x86-64 little-endian akeem
         srfi-0 srfi-9 srfi-23 srfi-34 srfi-39 srfi-87 srfi-98))
