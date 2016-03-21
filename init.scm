(define (repl)
  (display "Welcome to Akeem Scheme.")
  (newline)

  (let loop ()
    (display "> " (current-output-port))
    (let ((input (read)))
      (unless (eof-object? input)
        (let ((value (eval input (interaction-environment))))
          (unless (eq? (void) value)
            (write value (current-output-port))
            (newline (current-output-port)))
          (loop))))))

(let ((args (cdr (command-line))))
  (if (or (null? args))
      (repl)
      (for-each load args)))
