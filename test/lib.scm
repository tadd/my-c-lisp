(define tests ())
(define n-failure 0)
(define n-success 0)

(define (describe name f)
  (set! tests (cons (cons name f) tests)))
;;(define context describe)

(define (display* . args)
  (if (not (null? args))
      (begin
        (display (car args))
        (apply display* (cdr args)))))

(define (msg-proc-1 msg)
  (lambda (x)
    (display* "<" x "> to " msg)))

(define (msg-proc-2 msg)
  (lambda (x y)
    ((msg-proc-1 msg) x)
    (display* " <" y ">")))

(define fail-message-procs
  (list
   (cons > (msg-proc-2 "be >"))
   (cons equal? (msg-proc-2 "be equal?"))
   (cons eq? (msg-proc-2 "be eq?"))
   (cons null? (msg-proc-1 "be null?"))))

(define (assq o alist) ;; XXX: Implement me in C
  (cond
   ((null? alist) ())
   ((eq? o (car (car alist))) (car alist))
   (else (assq o (cdr alist)))))

(define (succeed)
  (set! n-success (+ n-success 1)))

(define (fail proc args)
  (set! n-failure (+ n-failure 1))
  (display* "Failed in " test-name ": Expected ")
  (fail-message proc args)
  (newline))

(define (fail-message proc args)
  (let ((f (assq proc fail-message-procs)))
    (apply (cdr f) args)))

(define (expect . args)
  (let ((proc (car args))
        (pargs (cdr args)))
    (if (apply proc pargs)
        (succeed)
        (fail proc pargs))))

(define (expect-t x)
  (expect eq? #t x))

(define (expect-f x)
  (expect eq? #f x))

(define test-name ())

(define (test-run)
  (define (test-run-inner l)
    (if (not (null? l))
        (begin
          (test-run-inner (cdr l))
          (test-run-single (car l)))))
  (test-run-inner tests)
  (test-summarize))

(define (test-run-single pair)
  (let ((name (car pair))
        (func (cdr pair)))
    (set! test-name name)
    (func)))

(define (test-summarize)
  (display* "Test summary: "
            n-success " succeeded, "
            n-failure " failed.")
  (newline))
