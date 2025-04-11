#lang racket

(require
 latex-utils/scribble/math
 latex-utils/scribble/utils)

(provide
 infix-ops-set!
 infix-ops-remove!
 prefix-ops-set!
 prefix-ops-remove!
 consts-set!
 consts-remove!
 delimited-ops-set!
 delimited-ops-remove!
 cat-ops-set!
 cat-ops-remove!
 macros-set!
 macros-remove!
 Q
 math
 displaymath
 array
 calc
 sep-by
 latex-eval
 macros-set!
 macros-remove!
 displaymath)

(define infix-ops
  (make-hash
   (list
    (cons (quote =) "=")
    (cons (quote =/=) "\\neq")
    (cons (quote and) "\\land")
    (cons (quote or) "\\lor")
    (cons (quote mid) "\\mid")
    (cons (quote ->) "\\rightarrow")
    (cons (quote :) "{:}"))))

(define (infix-ops-set! sym str)
  (hash-set! infix-ops sym str))

(define (infix-ops-remove! sym)
  (hash-remove! infix-ops sym))

(define prefix-ops
  (make-hash
   (list
    (cons (quote plus) "\\text{plus}")
    (cons (quote times) "\\text{times}")
    (cons (quote S) "\\text{S}")
    (cons (quote Cons) "\\text{Cons}")
    (cons (quote not) "\\lnot")
    (cons (quote rev) "\\text{rev}")
    (cons (quote snoc) "\\text{snoc}")
    (cons (quote revapp) "\\text{revapp}"))))

(define (prefix-ops-set! sym str)
  (hash-set! prefix-ops sym str))

(define (prefix-ops-remove! sym str)
  (hash-remove! prefix-ops sym))

(define consts
  (make-hash
   (list
    (cons (quote Z) "\\text{Z}")
    (cons (quote Nil) "\\text{Nil}")
    (cons (quote Nat) "\\text{Nat}")
    (cons (quote Lst) "\\text{Lst}")
    (cons (quote __) ""))))

(define (consts-set! sym str)
  (hash-set! consts sym str))

(define (consts-remove! sym)
  (hash-remove! consts sym))

(define delimited-ops
  (make-hash
   (list
    (cons (quote hs-list) (cons "[" "]")))))

(define (delimited-ops-set! sym lt rt)
  (hash-set! delimited-ops sym (cons lt rt)))

(define (delimited-ops-remove! sym)
  (hash-remove! delimited-ops sym))

(define cat-ops
  (make-hash
   (list
    (cons (quote reverse) "\\texttt{reverse}")
    (cons (quote reverseq) "\\texttt{reverse'}")
    (cons (quote revIter) "\\texttt{revIter}"))))

(define (cat-ops-set! sym str)
  (hash-set! cat-ops sym str))

(define (cat-ops-remove! sym str)
  (hash-remove! cat-ops sym))

(define macros
  (make-hash))

(define (macros-set! sym handler)
  (hash-set! macros sym handler))

(define (macros-remove! sym)
  (hash-remove! macros sym))

(define (latex-eval expression)
  (match expression
    [(list (and quantifier (or (quote forall) (quote exists)))
           bound-variables
           body)

     (define quantifier-string
       (match quantifier
         [(quote forall) "\\forall"] 
         [(quote exists) "\\exists"]))
     (define bound-variables-string
       (with-output-to-string
         (thunk
          (for/fold ([first? #t])
                    ([bv bound-variables]) ;; bound variable
            (unless first?
              (display ","))
            (match bv
              [(list var srt) ;; 'variable' 'sort'
               (display var)
               (display "{:}")
               (display (latex-eval srt))]
              [var ;; 'variable'
               (display (latex-eval var))])
            #f))))
     (define body-string (latex-eval body))
     (string-append
      quantifier-string "\\, "
      bound-variables-string ".~"
      body-string)]


    ;; [(list '_ expression subscript)

    ;;  (string-append "{" (latex-eval expression) "}_{" (latex-eval subscript) "}")]
    

    [(cons operator arguments)
     #:when (hash-has-key? infix-ops operator)

     (define separator (string-append " " (hash-ref infix-ops operator) " "))
     (define accumulator (open-output-string))
     (for/fold ([first? #t])
               ([argument arguments])
       (unless first?
         (display separator accumulator))
       (display (latex-eval argument) accumulator)
       #f)
     (get-output-string accumulator)]

    
    [(cons operator arguments)
     #:when (hash-has-key? prefix-ops operator)

     (define accumulator (open-output-string))
     (display (hash-ref prefix-ops operator) accumulator)
     (display #\( accumulator)
     (for/fold ([first? #t])
               ([argument arguments])
       (unless first?
         (display "," accumulator))
       (display (latex-eval argument) accumulator)
       #f)
     (display #\) accumulator)
     (get-output-string accumulator)]


    [(cons operator arguments)
     #:when (hash-has-key? delimited-ops operator)

     (match-define (cons lt rt)
       (hash-ref delimited-ops operator))
     (with-output-to-string
       (thunk
        (display lt)
        (for/fold ([first? #t])
                  ([argument arguments])
          (unless first?
            (display ","))
          (display (latex-eval argument))
          #f)
        (display rt)))]


    [(cons operator arguments)
     #:when (hash-has-key? cat-ops operator)

     (with-output-to-string
       (thunk
        (display (hash-ref cat-ops operator))
        (for ([argument arguments])
          (display "\\ ")
          (display (latex-eval argument)))))]

    [(cons operator arguments)
     #:when (hash-has-key? macros operator)
     ((hash-ref macros operator) (map latex-eval arguments))]


    [(? symbol?)
     #:when (hash-has-key? macros expression)
     ((hash-ref macros expression) null)]


    [(cons '$ (cons operator arguments))

     (define accumulator (open-output-string))
     (display (latex-eval operator) accumulator)
     (display #\( accumulator)
     (for/fold ([first? #t])
               ([argument arguments])
       (unless first?
         (display "," accumulator))
       (display (latex-eval argument) accumulator)
       #f)
     (display #\) accumulator)
     (get-output-string accumulator)]


    [(cons operator arguments)
     #:when (hash-has-key? macros operator)

     ((hash-ref macros operator) (map latex-eval arguments))]


    [(? symbol?)
     #:when (hash-has-key? macros expression)

     ((hash-ref macros expression) null)]


    [(? symbol?)
     #:when (hash-has-key? consts expression)

     (hash-ref consts expression)]


    [(? symbol?)

     (symbol->string expression)]


    [(? number?)

     (number->string expression)]


    [(? string?)
     expression]

    
    [else "UNIMPLEMENTED"]))

(define (array style rows)
  (env "array"
       #:opt (list (bracket "t") (curlies style))
       (string-join 
        (for/list ([cells rows])
          (string-join cells "&"))
        "\\\\")))

(define (calc op . terms)
  ;; precondition: (not (null? terms))
  (array
   "rl"
   (cons
    (list "" (first terms))
    (for/list ([term (rest terms)])
      (list op term)))))

(define (sep-by sep . elts) ;; 'separator', 'elements'
  (with-output-to-string
    (thunk
     (for/fold ([first? #t])
               ([elt elts])
       (unless first?
         (displayln sep))
       (display elt)
       #f))))

(define-syntax-rule (Q expression)
  (latex-eval (quote expression)))

(define-syntax-rule (math expression)
  (m (latex-eval (quote expression))))

(define-syntax-rule (displaymath expression)
  (mp (latex-eval (quote expression))))
