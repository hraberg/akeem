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
        (if (null? list)
            #f
            (if (eqv? (car list) obj)
                list
                (memv obj (cdr list))))))

(set! syntax-pattern-variable?
      (lambda (literals pattern)
        (if (symbol? pattern)
            (not (memv pattern (cons '... literals)))
            #f)))

(set! collect-syntax-variables
      (lambda (literals pattern match idxs)
        (if (pair? pattern)
            (collect-syntax-variables literals (car pattern)
                                      (collect-syntax-variables literals (cdr pattern) match idxs)
                                      idxs)
            (if (syntax-pattern-variable? literals pattern)
                (cons (cons 'transcribe-failure (cons pattern (cons 0 idxs))) match)
                match))))

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
                      (begin
                        (if (not (null? rest-pattern))
                            (if (eq? '... (car rest-pattern))
                                (collect-syntax-variables literals first-pattern match idxs)
                                #f)
                            #f))
                      (letrec ((default-match
                                     (lambda ()
                                       (let ((match (match-syntax-rule literals first-pattern (car form) match idxs env)))
                                         (if match
                                             (match-syntax-rule literals rest-pattern (cdr form) match idxs env)
                                             #f)))))
                        (let ((pair-or-null-match
                               (lambda ()
                                 (if (null? rest-pattern)
                                     (default-match)
                                     (if (eq? '... (car rest-pattern))
                                         (letrec ((loop
                                                   (lambda (form match idx)
                                                     (if (null? form)
                                                         match
                                                         (let ((match (match-syntax-rule literals first-pattern (car form) match (cons idx idxs) env)))
                                                           (if match
                                                               (loop (cdr form) match (+ 1 idx))
                                                               #f))))))
                                           (loop form match 0))
                                         (default-match))))))
                          (if (pair? (car form))
                              (pair-or-null-match)
                              (if (null? (car form))
                                  (pair-or-null-match)
                                  #f)))))
                  (if (syntax-pattern-variable? literals first-pattern)
                      (if (null? rest-pattern)
                          (match-syntax-rule literals rest-pattern (cdr form)
                                             (cons (cons (car form) (cons first-pattern idxs)) match) idxs env)
                          (if (eq? '... (car rest-pattern))
                              (letrec ((loop (lambda (form match idx)
                                               (if (null? form)
                                                   (cons (cons 'transcribe-failure (cons first-pattern (cons idx idxs))) match)
                                                   (loop (cdr form)
                                                         (cons (cons (car form) (cons first-pattern (cons idx idxs))) match)
                                                         (+ 1 idx))))))
                                (loop form match 0))
                              (match-syntax-rule literals rest-pattern (cdr form)
                                                 (cons (cons (car form) (cons first-pattern idxs)) match) idxs env)))
                      (if (equal? first-pattern (car form))
                          (if (memv first-pattern env)
                              #f
                              (match-syntax-rule literals rest-pattern (cdr form) match idxs env))
                          #f)))))))

(set! syntax-template-pattern-variable?
      (lambda (match template)
        (if (null? match)
            #f
            (if (eqv? (car (cdr (car match))) template)
                #t
                (syntax-template-pattern-variable? (cdr match) template)))))

(set! transcribe-syntax-template
      (lambda (match template-idxs)
        (if (null? match)
            'transcribe-failure
            (if (equal? (cdr (car match)) template-idxs)
                (car (car match))
                (transcribe-syntax-template (cdr match) template-idxs)))))

(set! transcribe-syntax-rule
      (lambda (match template idxs)
        (if (not (pair? template))
            (if (syntax-template-pattern-variable? match template)
                (transcribe-syntax-template match (cons template idxs))
                template)
            (let ((first-template (car template))
                  (rest-template (cdr template)))
              (letrec ((default-transcribe
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
                         (letrec ((loop (lambda (transcribed new-idx)
                                          (let ((new-transcribed (transcribe-syntax-rule match first-template (cons new-idx idxs))))
                                            (if (eq? 'transcribe-failure new-transcribed)
                                                transcribed
                                                (loop (append transcribed (cons new-transcribed '()))
                                                      (+ new-idx 1)))))))
                           (loop '() 0))
                         (transcribe-syntax-rule match (cdr rest-template) idxs))
                        (default-transcribe))))))))

(set! transform-syntax-rules
      (lambda (literals syntax-rules form env)
        (if (null? syntax-rules)
            '(begin)
            (let ((pattern (cdr (car (car syntax-rules))))
                  (template (cdr (car syntax-rules))))
              (let ((match (match-syntax-rule literals pattern form '() '() env)))
                (if match
                    (cons 'begin (transcribe-syntax-rule (reverse match) template '()))
                    (transform-syntax-rules literals (cdr syntax-rules) form env)))))))

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
