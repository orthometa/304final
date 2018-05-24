(load "pmatch.ss")

;; Use for the first two questions
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

(define dropf
  (lambda (f)
    (lambda (ls)
      (cond
        ((or (null? ls) (f (car ls))) ls)
        (else ((dropf f) (cdr ls)))))))

(define (list-index-ofv? x ls)
  (cond
    ((eqv? x (car ls)) 0)
    (else (add1 (list-index-ofv? x (cdr ls))))))

(define lex
  (lambda (exp acc)
    (pmatch exp
      [,y (guard (symbol? y)) `(var ,(list-index-ofv? y acc))]
      [,c (guard (or (boolean? c) (number? c))) `(const ,c)]
      [(sub1 ,nexp) `(sub1 ,(lex nexp acc))]
      [(if ,test ,conseq ,alt) `(if ,(lex test acc) ,(lex conseq acc) ,(lex alt acc))]
      [(let ((,x ,e)) ,body) `(let ,(lex e acc) ,(lex body (cons x acc)))]
      [(lambda (,x) ,body) `(lambda ,(lex body (cons x acc)))]
      [(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))])))


