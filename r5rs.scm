;;; 6.1. Equivalence predicates

(define (equal? obj1 obj2)
  (eq? obj1 obj2))

;;; 6.2.5. Numerical operations

(define complex? number?)
(define real? number?)

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

(define (max x1 x2)
  (if (< x1 x2)
      x2
      x1))

(define (min x1 x2)
  (max x2 x1))

(define (abs x)
  (if (negative? x)
      (- 0 x)
      x))

;;; 6.3.1. Booleans

(define (not obj)
  (if (eq? obj #f)
      #t
      #f))

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
  (car (car (car obj))))

(define (caadr obj)
  (car (car (cdr obj))))

(define (cadar obj)
  (car (cdr (car obj))))

(define (caddr obj)
  (car (cdr (cdr obj))))

(define (cdaar obj)
  (cdr (car (car obj))))

(define (cdadr obj)
  (cdr (car (cdr obj))))

(define (cddar obj)
  (cdr (cdr (car obj))))

(define (cdddr obj)
  (cdr (cdr (cdr obj))))

(define (caaaar obj)
  (car (car (car (car obj)))))

(define (caaadr obj)
  (car (car (car (cdr obj)))))

(define (caadar obj)
  (car (car (cdr (car obj)))))

(define (caaddr obj)
  (car (car (cdr (cdr obj)))))

(define (cadaar obj)
  (car (cdr (car (car obj)))))

(define (cadadr obj)
  (car (cdr (car (cdr obj)))))

(define (caddar obj)
  (car (cdr (cdr (car obj)))))

(define (cadddr obj)
  (car (cdr (cdr (cdr obj)))))

(define (cdaaar obj)
  (cdr (car (car (car obj)))))

(define (cdaadr obj)
  (cdr (car (car (cdr obj)))))

(define (cdadar obj)
  (cdr (car (cdr (car obj)))))

(define (cdaddr obj)
  (cdr (car (cdr (cdr obj)))))

(define (cddaar obj)
  (cdr (cdr (car (car obj)))))

(define (cddadr obj)
  (cdr (cdr (car (cdr obj)))))

(define (cdddar obj)
  (cdr (cdr (cdr (car obj)))))

(define (cddddr obj)
  (cdr (cdr (cdr (cdr obj)))))

(define (null? obj)
  (eq? '() obj))

(define (list? obj)
  (or (null? obj)
      (and (pair? obj) (list? (cdr obj)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (car (list-tail list k)))

(define (member-aux comparator obj list)
  (if (null? list) #f
      (if (comparator obj (car list))
          list
          (member-aux comparator obj (cdr list)))))

(define (memq obj list)
  (member-aux eq? obj list))

(define (memv obj list)
  (member-aux eqv? obj list))

(define (member obj list)
  (member-aux equal? obj list))

(define (assoc-aux comparator obj alist)
  (if (null? alist) #f
      (if (comparator obj (caar alist))
          (car alist)
          (assoc-aux comparator obj (cdr alist)))))

(define (assq obj alist)
  (assoc-aux eq? obj alist))

(define (assv obj alist)
  (assoc-aux eqv? obj alist))

(define (assoc obj alist)
  (assoc-aux equal? obj alist))

;;; 6.3.5. Strings

(define (string->list-aux string list idx)
  (if (negative? idx)
      list
      (string->list-aux
       string (cons (string-ref string idx) list) (- idx 1))))

(define (string->list string)
  (string->list-aux
   string '() (- (string-length string) 1)))

(define (substring-aux string copy idx start end)
  (if (= idx end)
      copy
      (substring-aux
       string
       (begin
         (string-set! copy (- idx start) (string-ref string idx))
         copy)
       (+ idx 1)
       start
       end)))

(define (substring string start end)
  (substring-aux
   string (make-string (- end start)) start start end))

(define (string-append string1 string2)
  (list->string (append (string->list string1) (string->list string2))))

(define (list->string-aux string list idx)
  (if (null? list)
      string
      (list->string-aux
       (begin
         (string-set! string idx (car list))
         string)
       (cdr list)
       (+ 1 idx))))

(define (list->string list)
  (list->string-aux
   (make-string (length list)) list 0))

(define (string-copy string)
  (list->string (string->list string)))

(define (string-fill!-aux string fill idx)
  (if (negative? idx)
      string
      (string-fill!-aux
       (begin
         (string-set! string idx fill)
         string)
       fill
       (- idx 1))))

(define (string-fill! string fill)
  (string-fill!-aux
   string fill (- (string-length string) 1)))

;;; 6.3.6. Vectors

(define (vector->list-aux vector list idx)
  (if (negative? idx)
      list
      (vector->list-aux
       vector (cons (vector-ref vector idx) list) (- idx 1))))

(define (vector->list vector)
  (vector->list-aux
   vector '() (- (vector-length vector) 1)))

(define (vector-fill!-aux vector fill idx)
  (if (negative? idx)
      vector
      (vector-fill!-aux
       (begin
         (vector-set! vector idx fill)
         vector)
       fill
       (- idx 1))))

(define (vector-fill! vector fill)
  (vector-fill!-aux
   vector fill (- (vector-length vector) 1)))

;;; 6.4. Control features

(define (map proc list)
  (if (null? list) '()
      (cons (proc (car list)) (map proc (cdr list)))))

(define (for-each proc list)
  (if (null? list) '()
      (begin (proc (car list))
             (for-each proc (cdr list)))))

(define force
  (lambda (object)
    (object)))
