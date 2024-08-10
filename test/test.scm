(load "./lib.scm")

(describe "letrec" (lambda ()
  (define retval
    (letrec ((myeven?
              (lambda (n)
                (if (= n 0)
                    #t
                    (myodd? (- n 1)))))
             (myodd?
              (lambda (n)
                (if (= n 0)
                    #f
                    (myeven? (- n 1))))))
      (myeven? 88)))
  (expect equal? retval #t)))

(describe "call/cc" (lambda ()
  ;; Based on "yin-yang" in Kawa's test suite under the MIT license:
  ;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
  (define r
    (let ((x ())
          (y 0)
          (id (lambda (x) x)))
      (call/cc
       (lambda (escape)
         (let* ((in ((lambda (foo)
                       (set! x (cons y x))
                       (if (= y 5)
                           (escape x)
                           (begin
                             (set! y 0)
                             foo)))
                     (call/cc id)))
                (yo ((lambda (foo)
                       (set! y (+ y 1))
                       foo)
                     (call/cc id))))
           (in yo))))))
  (expect equal? r (list 5 4 3 2 1 0))))

(describe "eq" (lambda ()
  (expect-t (eq? #t #t))
  (expect-t (eq? #f #f))
  (expect-f (eq? #t #f))
  (expect-t (eq? 1 1))
  (expect-f (eq? 1 -1))
  (expect-t (eq? () ()))
  (expect-f (eq? () (list 1)))
  (expect-t (let ((x (list 1)))
              (eq? x x)))
  (expect-t (let ((p (lambda (x) x)))
              (eq? p p)))

  (expect-f (eq? (list 1) (list 1)))
  (expect-f (eq? (list 1 (list 2)) (list 1 (list 2))))))

(describe "equal" (lambda ()
  (expect-t (equal? #t #t))
  (expect-t (equal? #f #f))
  (expect-f (equal? #t #f))
  (expect-t (equal? 1 1))
  (expect-f (equal? 1 -1))
  (expect-t (equal? () ()))
  (expect-f (equal? () (list 1)))
  (expect-t (let ((x (list 1)))
              (equal? x x)))
  (expect-t (let ((p (lambda (x) x)))
              (equal? p p)))

  (expect-t (equal? (list 1) (list 1)))
  (expect-t (equal? (list 1 (list 2)) (list 1 (list 2))))

  (expect-t (equal? "abc" "abc"))
  (expect-t (equal? "\"" "\""))
  (expect-f (equal? "abc" "abd"))
  (expect-f (equal? "abc\"" "\""))))

(test-run)
