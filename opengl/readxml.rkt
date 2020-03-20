#lang racket
(require sxml)


(define (read-spec)
  (define in (open-input-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/api/gl.xml"))
  (define spec (ssax:xml->sxml in '()))
  (close-input-port in)
  spec
)

(define output (open-output-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/results.rkt" #:exists 'replace))

(displayln "#lang racket" output)
(define export-list '())
;; tests to learn what's going on
(match '(a c) ((list a b) (print b)))
(match '(value "onion") [(list 'value onion) (print onion)])
(write '(@ foo))
; end tests

(define (find-registry spec)
  (match spec
    [(cons '*TOP*
           (cons (cons '*PI* pi)
                 (list(cons 'registry stuff )))) stuff
                                                      ]
    [ _ (cons 'nope: spec)]
    ))

(define (process-enum-header header)
  (println 'header)
  )

(define (rehexify v)
  (if (and (string? v) ( >= (string-length v) 2) (equal? (substring v 0 2) "0x"))
      (string-append "#x" (substring v 2))
      v
))
(define (process-enum-item item)
  (match item
      [ (list 'enum (cons '@ xmlattributes))
  ;; There ought to be a way to do this with a match, but I haven't found it.
              #;(match xmlattributes [(list-no-order (list 'value v) (list 'name s)(list 'alias alias) ...
                                            ;(or (list 'vendor _) (list 'comment _ ) (list 'type _ ) (list 'api _)) ...
                                            )
                                 '() ;; placeholder for generating enum translation
                                 ]           )
              
        (define names '())
        (define values '())
        (define aliases '())
          
        (for ([attr xmlattributes])
          (match attr
            [(list 'value v) (set! values (cons v values))]
            [(list 'name n) (set! names (cons n names))]
            [(list 'alias a) (set! aliases (cons a aliases))]
            [(cons 'comment _) '()]  ; TODO: write comments
            [(cons 'type _) '()] ; TODO: Do I need to process types in enumerations?
            [(cons 'api _) '()] ; TODO:  What do I do with API?
            [ other (displayln (list 'other 'enum 'attribute other))]
            ;; TODO: Do I have to do anything with type or api?
            )
          )
        (if (not (equal? (length names) 1)) (displayln (cons "too many names in enum" names)) '())
        (if (not (equal? (length names) 1)) (displayln (cons "too many values i enum" names values)) '())
        (for ([name names] [value values])
          (displayln (list 'define name (rehexify value)) output )
          (set! export-list (cons name export-list))
          (for ([a aliases])
            (display (list 'define a (rehexify value)) output)
            (displayln (string-append "; alias for " name) output)
            (set! export-list (cons a export-list))
            ))
        ]
      [ _ (println (cons 'unknown-enum-item item))]
    )
  )

(define (process-enum-list enum-list)
  (if (pair? enum-list)
      (begin
        (process-enum-item (car enum-list))
        (process-enum-list (cdr enum-list))
        )
      (cons 'enum-list-tail enum-list)
      )
  )
  
(define (process-enums enums)
  (match enums [(cons header enum-list)
                (process-enum-header header)
                (process-enum-list enum-list)]
    [ _ (print enums) ]
    ))

(define (process-regitem item)
  (match item
    [(cons 'enums rest) (process-enums rest)]
    [ _ (print (car item))]
    ))

(define (process-registry reg)
  (match reg
    [(cons item rest)
     (process-regitem item)
     (process-registry rest)]
    ['() '()]
    ))

(process-registry
 (find-registry
  (read-spec)
))

(displayln (cons 'provide export-list) output)

(close-output-port output)
