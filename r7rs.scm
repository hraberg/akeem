;;; R7RS

;;; 4.2.1. Conditionals

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

;;; 6.1. Equivalence predicates

(let ((vector=? (lambda (vector1 vector2)
                  (if (= (vector-length vector1) (vector-length vector2))
                      (let loop ((idx (- (vector-length vector1) 1)))
                        (cond ((negative? idx) #t)
                              ((equal? (vector-ref vector1)
                                       (vector-ref vector2))
                               (loop (- idx 1)))
                              (else #f)))
                      #f)))
      (bytevector=? (lambda (bytevector1 bytevector2)
                      (if (= (bytevector-length bytevector1) (bytevector-length bytevector2))
                          (let loop ((idx (- (bytevector-length bytevector1) 1)))
                            (cond ((negative? idx) #t)
                                  ((equal? (bytevector-u8-ref bytevector1)
                                           (bytevector-u8-ref bytevector2))
                                   (loop (- idx 1)))
                                  (else #f)))
                          #f))))
  (define (equal? obj1 obj2)
    (cond ((and (pair? obj1) (pair? obj2))
           (and (equal? (car obj1) (car obj2))
                (equal? (cdr obj1) (cdr obj2))))
          ((and (string? obj1) (string? obj2))
           (string=? obj1 obj2))
          ((and (vector? obj1) (vector? obj2))
           (vector=? obj1 obj2))
          ((and (bytevector? obj1) (bytevector? obj2))
           (bytevector=? obj1 obj2))
          (else (eqv? obj1 obj2)))))

;;; 6.2.6. Numerical operations

(define (exact-integer? z)
  (and (integer? z) (exact? z)))

(define (square z)
  (* z z))

(define inexact exact->inexact)
(define exact inexact->exact)

(define (finite? z)
  (and (number? z)
       (not (or (nan? z) (infinite? z)))))

(define (exact-integer-sqrt k)
  (let* ((s (exact (sqrt k))))
    (cons s (exact (- k (* s s))))))

;;; 6.4. Pairs and lists

(define (make-list k fill)
  (do ((acc '() (cons fill acc))
       (idx 0 (+ idx 1)))
      ((= idx k) acc)))

(define (list-set! list k obj)
  (set-car! (list-tail list k) obj))

(define (list-copy obj)
  (map (lambda (x) x) obj))

;;; 6.5. Symbols

(define (symbol=? symbol1 symbol2)
  (equal? (symbol->string symbol1) (symbol->string symbol2)))

;;; 6.6. Characters

(define (digit-value char)
  (if (char-numeric? char)
      (- (char->integer char) (char->integer #\0))
      #f))

(define char-foldcase char-downcase)

;;; 6.7. Strings

(define (string-upcase string)
  (string-map char-upcase string))

(define (string-downcase string)
  (string-map char-downcase string))

(define (string-foldcase string)
  (string-map char-foldcase string))

(define (string-copy! to at from start end)
  (do ((idx start (+ idx 1)))
      ((= idx end) to)
    (string-set! to (- (+ idx at) start) (string-ref from idx))))

;;; 6.8. Vectors

(define (vector-copy vector start end)
  (vector-copy! (make-vector (- end start) 0) 0 vector start end))

(define (vector-copy! to at from start end)
  (do ((idx start (+ idx 1)))
      ((= idx end) to)
    (vector-set! to (- (+ idx at) start) (vector-ref from idx))))

(define (string->vector string)
  (let ((length (string-length string)))
    (do ((acc (make-vector length))
         (idx 0 (+ idx 1)))
        ((= idx length) acc)
      (vector-set! acc idx (string-ref string idx)))))

(define (vector->string vector)
  (let ((length (vector-length vector)))
    (do ((acc (make-string length))
         (idx 0 (+ idx 1)))
        ((= idx length) acc)
      (string-set! acc idx (vector-ref vector idx)))))

(define (vector-append vector1 vector2)
  (let* ((length1 (vector-length vector1))
         (length2 (vector-length vector2))
         (acc (make-vector (+ length1 length2))))
    (do ((idx 0 (+ idx 1)))
        ((= idx length1))
      (vector-set! acc idx (vector-ref vector1 idx)))
    (do ((idx 0 (+ idx 1)))
        ((= idx length2))
      (vector-set! acc (+ idx length1) (vector-ref vector2 idx)))
    acc))

;;; 6.9. Bytevectors

(define (bytevector-copy bytevector start end)
  (bytevector-copy! (make-bytevector (- end start) 0) 0 bytevector start end))

(define (bytevector-copy! to at from start end)
  (do ((idx start (+ idx 1)))
      ((= idx end) to)
    (bytevector-u8-set! to (- (+ idx at) start) (bytevector-u8-ref from idx))))

(define (bytevector-append bytevector1 bytevector2)
  (let* ((length1 (bytevector-length bytevector1))
         (length2 (bytevector-length bytevector2))
         (acc (make-bytevector (+ length1 length2))))
    (do ((idx 0 (+ idx 1)))
        ((= idx length1))
      (bytevector-u8-set! acc idx (bytevector-u8-ref bytevector1 idx)))
    (do ((idx 0 (+ idx 1)))
        ((= idx length2))
      (bytevector-u8-set! acc (+ idx length1) (bytevector-u8-ref bytevector2 idx)))
    acc))

(define (utf8->string bytevector start end)
  (do ((acc (make-string (- end start)))
       (idx start (+ idx 1)))
      ((= idx end) acc)
    (string-set! acc (- idx start) (bytevector-u8-ref bytevector idx))))

(define (string->utf8 string start end)
  (do ((acc (make-bytevector (- end start)))
       (idx start (+ idx 1)))
      ((= idx end) acc)
    (bytevector-u8-set! acc (- idx start) (string-ref string idx))))

;;; 6.10. Control features

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

;;; 6.13.2. Input

(define (read-line port)
  (do ((acc '() (cons char acc))
       (char (read-char port) (read-char port)))
      ((or (eq? #\newline char)
           (eq? #\return char)
           (eof-object? char)) (list->string (reverse acc)))))

(define (read-string k port)
  (utf8->string (read-bytevector k port) 0 k))

(define (read-bytevector k port)
  (let ((bytevector (make-bytevector k 0)))
    (read-bytevector! bytevector port 0 k)
    bytevector))

(define (read-bytevector! bytevector port start end)
  (do ((idx start (+ idx 1))
       (byte (read-u8 port) (read-u8 port)))
      ((or (= idx end) (eof-object? byte)) (if (= idx start)
                                               (eof-object)
                                               idx))
    (bytevector-u8-set! bytevector idx byte)))

;;; 6.13.3. Output

(define (write-string string port start end)
  (write-bytevector (string->utf8 string start end) port start end))

(define (write-bytevector bytevector port start end)
  (do ((idx start (+ idx 1)))
      ((= idx end) (- end start))
    (write-u8 (bytevector-u8-ref bytevector idx) port)))

;;; 6.14. System interface

(define (get-environment-variable name)
  (cond ((assoc name (get-environment-variables)) => cdr)
        (else #f)))

(define (features)
  '(r7rs exact-closed ieee-float posix gnu-linux x86-64 little-endian))
