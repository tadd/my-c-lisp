;; All of above call/cc tests were from Kawa's test suite under the MIT license

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
;; Cause: maybe just a bug?
(describe "call/cc defines in retlec body" (lambda ()
  (define (f)
    (let ((cont #f))
      (letrec ((x (call/cc (lambda (c) (set! cont c) 0)))
               (y (call/cc (lambda (c) (set! cont c) 0))))
        (if cont
            (let ((c cont))
              (set! cont #f)
              (set! x 1)
              (set! y 1)
              (c 0))
            (+ x y)))))
  (expect eq? (f) 0)))

;; Cause: bug?
(describe "call/cc retlec and set!" (lambda ()
  (define (f)
    (letrec ((x (call/cc
		 (lambda (c)
		   (list #t c)))))
      (if (car x)
	  ((cadr x) (list #f (lambda () x)))
	  (eq? x ((cadr x))))))
  (expect-t (f))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/sva35362.scm
;; Cause: named let
(describe "call/cc unused" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let l ()
         (l))))
    #t)
  (expect-t (f))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/unreach1.scm
;; Cause: named let
(describe "call/cc unreach 1" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let l ()
         (return #f)
         (l))))
    #t)
  (expect-t (f))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/unreach2.scm
;; Cause: named let
(describe "call/cc unreach 2" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let ((a 1))
         (let loop ((a a))
           (let ((finish (lambda (a) (return #f))))
             (finish a)
             (let ((a 2))
               (loop a))))))))
  (expect-f (f))))
