#lang typed/racket
(require/typed sxml [ssax:xml->sxml (-> Input-Port Any Any)] )
;(require/typed sxml [ssax:xml->sxml (-> Any Any Any)] )

;(require srfi/13) Don't do this.  It has the wrong ersion of string-trim)

; Debugging flags

#| This program reads the xml specfile that has been downloaded from Khronos
   and writes an (as yet incomplete) binding for openGL on Racket.
   Whenever it encounters something not yet implemented, ot merely anomalous,
   it write a comment into the output describing the problem.
   These comments are a means of tracking what still has to be done.
   It is also intended to be able to compare its output with the output
   of the older Racket binding, possibly by diff,
   and possibly by applying diff to the sorted output of the two programs.
   This is another quality check.
|#

; Debugging flags

(define do-enums #t) 
  ; whether to produce enums. value during production: #t
  ; Set this fo false when debugging other stuff
  ; to prevent being inundated with huge numbers of enums.

(define suppress-messages #f)
  ; whether to suppress messages entirely
  ; This may be useful when comparing generated code with a previous version.

(define messages-in-generated-code #f)
(define separate-trace #t) ; whether trace output goes into a separate file

; --- message conventions

#|
  This program complains whenever it receives unexpected input.
  This usualy arises because the program is incomplete, or was
  because the author misunderstood the structure of the xml input file.
  Such messages should be embedded in the output as comments.
  They should always contain the word "TODO" or "LATER"
  "TODO" is used for things under active development.
  "LATER" is used for matters intentionally deferred for later.
     to avoid diverting my attention by a plethora of currently irrelevnt messages.
  -- hendrik
|#

(define output : Output-Port (open-output-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/generated/api.inc" #:exists 'replace))
#;(define output (current-output-port))
;  regular output file.
; TODO: use call-with-output-port

(define trace (if separate-trace
                  (open-output-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/traceout" #:exists 'replace)
                  output)
  )

(define anomaly : Output-Port
  (if suppress-messages (open-output-nowhere)
      (if messages-in-generated-code output
          (current-output-port))))

; the file to which to report anomalies
; NOTE: the distinction between output and anomaly is currently not well respected

;(fprintf output "#lang racket~n") -- to be included, not required
(define export-list '())

(define (unique [list : (Listof Any)] [message : String] .
                [messageargs : Any *])
  ; return the only element of the list, or '() if there is none.
  ; Produce message if not just one. 
  (if (equal? 1 (length list)) (car list)
     (begin
       (apply fprintf anomaly message messageargs)
       (if (null? list) list (car list))
       )
     )
)


(define (atmostone [list : (Listof Any)] [message : String] . [messageargs : Any *])
  ; return the first element of the list, or '() if there is none.
  ; Produce message if there are more than one element in the list.
     (if (null? list) '()
         (begin
           (when ( < 1 (length list))
               (apply fprintf anomaly message messageargs)
               )
           (car list)
           )
    )
)
; Global variables used to collect information

(: enums (HashTable String String))
(define enums (make-hash))
(define type-map (make-hash)) ; TODO: what is this?
(define-type XML (U String (Listof Any))) ; pro tem type to identify pieces of XML.  Don't want to have a serious
    ; XML type unless I can get sxml to be a typed module -- huge waste of time.
    ; TODO: expand this to whatever I think I've implemented and nothing else.

;; Here is code lifted verbatim from the original opengl42

; It would be really cool if we could do strict checking of
; the enums. And in fact the info appears to be available in enum.spec
; and I wrote the code to do it. But I quickly found out that enum.spec
; is too buggy to be useful for that purpose. 
; Eveything a C compiler doesn't check is likely to be unreliable in that file...

(define strongly-typed-enums #f)

;; These data structures look like stuff parsed from the original specfiles.
;; I'll provide types and perhaps use them instead of the ones I was starting to build,
;; on the off chance that he figured out more than I did.

;; TODO:  Most of the 'Any's in teh following shouls be more specific types.

(define-struct param-spec
               ([name : String] [type : Any] [mode : Any] [shape : Any] [size : Integer]))

;; original:
#;(define-struct function-spec
               ([name]
                (params #:mutable)
                (return #:mutable)
                (version #:mutable)
                (category #:mutable)
                (alias #:mutable)
                (deprecated #:mutable)
                ))

(define-struct function-spec
               ([name : String] ;; TODO: This isn't mutable in the oiginal,
                (params : Any) ;; presumably the params entities from xml
                (return : Any) ;; presumably the return type from xml 
                (version : Any);; TODO : What is this/
                (category : Any) ;; TODO: what is this?
                (alias : Any) ;; Presumably the alias comment.
                (deprecated : Any)) ;; where should this come from?
  #:mutable ;; TODO: can only make everything mutable, instaed of field by field,
  )

#;(define-struct constant-spec
               (name
                (value #:mutable)))

(define-struct constant-spec
               ([name : String] ;; the name of the ocnstant
                [value : Any])) ;; teh value.  Probably doesn't need to  Any.


#;(define-struct enum-spec
               (name
                 type
                 (constants #:mutable))) ; The enumerations.  Is this maybe a list of names?

(define-struct enum-spec
               ([name : String]
                 [type : Any] ;; The type (in what notation?) This si presumably the type that we can't check because openGL seems to cross type boundaries all the time.
                 [constants : Any] ; The enumeratees.  Is this maybe a list of names?  Or name, value pairs?
                 ))
#;(define-struct mode-dependent-type (in out))


  ;; Operations on types


#| This program has to deal with types in three different forms:
  * The original XML form, as a bunch of attributes and the like
    in a param ane elsewhere
  * as a Racket ctype 
  * as a Racket predicate
    The original function 'cleanup-type-for-doc produced this from the ctype
    in the original nonxml version of the binding generator
  Here are the functions that do this traslation.
|#

(define-type TType (U Symbol (Listof Any)  mode-dependent-type broken-type)) ;; TODO: make this more precise.
(define-struct broken-type ([detail : Any])) ; used to represent erroneous types.
(define-struct mode-dependent-type ([in : TType] [out : TType]))

(define-type CContract (U Symbol (Listof Any) broken-contract)) ; symbolis representation for a contract ;; TODO: make this more precise
(define-struct broken-contract ([detail : Any])) ; used to represent contracts for erroneous types.
;; This contanis two types, one to be used for an input argument to a functino, the other for an output arguent.


(: type-to-vector (HashTable TType TType))

(define type-to-vector
  (make-hash
    (cast '(
      (_int8 . _s8vector)
      (_uint16 . _u16vectorbbbb)
      (_int16 . _s16vector)
      (_uint32 . _u32vector)
      (_int32 . _s32vector)
      (_uint64 . _u64vector)
      (_int64 . _s64vector)
      (_float . _f32vector)
      (_double* . _f64vector))
          (Listof (Pair TType TType)))
          ))

(: vector-to-contract (HashTable TType CContract))

(define vector-to-contract
  (make-hash
   (cast '(
           (_bytes . bytes?)
           (_s8vector . s8vector?)
           (_u16vector . u16vector?)
           (_s16vector . s16vector?)
           (_u32vector . u32vector?)
           (_s32vector . s32vector?)
           (_u64vector . u64vector?)
           (_s64vector . s64vector?)
           (_f32vector . f32vector?)
           (_f64vector . f64vector?))
         (Listof (Pair TType CContract))
         )
   ))


( hash-ref vector-to-contract '_bytes (lambda () 'other))

; The old pointer-to function
#|(define (pointer-to type . args)
  (if (and (equal? args '(1)) (not (eq? type '_void)))
    (mode-dependent-type
      `(_ptr i ,type) `(_ptr o ,type))
    (case type
      ((_void) '_pointer/intptr)
      ((_byte _uint8) (mode-dependent-type 
                        '_string*/utf-8 
                        (if (null? args)
                          '_bytes
                          `(_bytes o ,@ args))))
      (else
        (let ((vt (hash-ref type-to-vector type #f)))
          (if vt
            (mode-dependent-type
              `(,vt i)
              (if (null? args) 
                vt
                `(,vt o ,@ args))) 
            (mode-dependent-type
              `(_vector i ,type)
              (if (null? args) 
                '_pointer
                `(_vector o ,type ,@ args)))))))))
|#
; : (-> (Listof Any) String Any * Any)

( : pointer-to (-> TType Any * TType))

(define (pointer-to type . [args : Any *])
  (if (and (equal? args '(1)) (not (eq? type '_void)))
    (mode-dependent-type
      `(_ptr i ,type) `(_ptr o ,type))
    (case type
      ((_void) '_pointer/intptr)
      ((_byte _uint8) (mode-dependent-type 
                        '_string*/utf-8 
                        (if (null? args)
                          '_bytes
                          `(_bytes o ,@ args))))
      (else
        (let ((vt (hash-ref type-to-vector type #f)))
          (if vt
            (mode-dependent-type
              `(,vt i)
              (if (null? args) 
                vt
                `(,vt o ,@ args))) 
            (mode-dependent-type
              `(_vector i ,type)
              (if (null? args) 
                '_pointer
                `(_vector o ,type ,@ args)))))))))



#|
; Original version from the old spec reader, kept for reference..
; It takes combinations of notations that are no longer available.
; It is kept here for reference while writing the new one.
#;(define basic-type-map
  (make-hash
    (list
      (cons "GLshort" '_int16)
      (cons "GLvoid" '_void)
      (cons "const GLubyte *" (pointer-to '_uint8))
      (cons "GLsync" '_GLsync)
      (cons "GLhandleARB" '_uint32)
      (cons "GLboolean" '_bool)
      (cons "struct _cl_event *" '_pointer)
      (cons "GLint64EXT" '_int64)
      (cons "GLsizeiptrARB" '_intptr)
      ;    (cons "GLDEBUGPROCARB" _GLDEBUGPROCARB)
      (cons "GLenum" '_int32)
      (cons "GLint" '_int32)
      (cons "GLclampd" '_double*)
      (cons "GLvoid*" (pointer-to '_void))
      (cons "GLhalfNV"'_uint16) ; A 16-bit floating point number. You get the bits, good luck. ;-)
      ;    (cons "_GLfuncptr" __GLfuncptr)
      (cons "GLubyte" '_uint8)
      ;    (cons "GLvdpauSurfaceNV" _GLvdpauSurfaceNV)
      (cons "GLcharARB*" (pointer-to '_byte))
      (cons "GLdouble*" (pointer-to '_double*))
      (cons "struct _cl_context *" '_pointer)
      (cons "GLcharARB" '_byte)
      (cons "GLfloat" '_float)
      (cons "GLuint64" '_uint64)
      (cons "GLbyte" '_int8)
      (cons "GLbitfield" '_uint32)
      (cons "GLuint64EXT" '_uint64)
      (cons "GLchar*" (pointer-to '_byte))
      (cons "GLchar* const" (pointer-to '_byte))
      (cons "GLsizeiptr" '_intptr)
      (cons "GLchar" '_byte)
      ;    (cons "GLUquadric*" _GLUquadric*)
      (cons "GLdouble" '_double*)
      (cons "GLintptr" '_intptr)
      ;    (cons "GLUtesselator*" _GLUtesselator*)
      (cons "GLsizei" '_int32)
      (cons "GLvoid* const" (pointer-to '_void))
      ;    (cons "GLDEBUGPROCAMD" _GLDEBUGPROCAMD)
      (cons "GLboolean*" (pointer-to '_bool))
      (cons "GLint64" '_int64)
      (cons "GLintptrARB" '_intptr)
      ;    (cons "GLUnurbs*" _GLUnurbs*)
      (cons "GLuint" '_uint32)
      (cons "GLclampf" '_float)
      (cons "GLushort" '_uint16)
      (cons "GLfloat*" (pointer-to '_float)))))
|#

; This function is from the non-XML version of this program.
; TODO: remove notations that cannot occur in the XML verison.
; TODO: remove the names with stars in them and replace them with something suitable, once I figure out what that is.

(define basic-type-map : (HashTable String TType)
  (make-hash
    (list
      (cons "GLshort" '_int16)
      (cons "GLvoid" '_void)
      (cons "const GLubyte *" (pointer-to '_uint8)) ;; TODO: put this back
      (cons "GLsync" '_GLsync)
      (cons "GLhandleARB" '_uint32)
      (cons "GLboolean" '_bool)
      (cons "struct _cl_event *" '_pointer)
      (cons "GLint64EXT" '_int64)
      (cons "GLsizeiptrARB" '_intptr)
      ;    (cons "GLDEBUGPROCARB" _GLDEBUGPROCARB)
      (cons "GLenum" '_int32)
      (cons "GLint" '_int32)
      (cons "GLclampd" '_double*)
      (cons "GLvoid*" (pointer-to '_void))
      (cons "GLhalfNV"'_uint16) ; A 16-bit floating point number. You get the bits, good luck. ;-)
      ;    (cons "_GLfuncptr" __GLfuncptr)
      (cons "GLubyte" '_uint8)
      ;    (cons "GLvdpauSurfaceNV" _GLvdpauSurfaceNV)
      (cons "GLcharARB*" (pointer-to '_byte))
      (cons "GLdouble*" (pointer-to '_double*))
      (cons "struct _cl_context *" '_pointer)
      (cons "GLcharARB" '_byte)
      (cons "GLfloat" '_float)
      (cons "GLuint64" '_uint64)
      (cons "GLbyte" '_int8)
      (cons "GLbitfield" '_uint32)
      (cons "GLuint64EXT" '_uint64)
      (cons "GLchar*" (pointer-to '_byte))
      (cons "GLchar* const" (pointer-to '_byte))
      (cons "GLsizeiptr" '_intptr)
      (cons "GLchar" '_byte)
      ;    (cons "GLUquadric*" _GLUquadric*)
      (cons "GLdouble" '_double*)
      (cons "GLintptr" '_intptr)
      ;    (cons "GLUtesselator*" _GLUtesselator*)
      (cons "GLsizei" '_int32)
      (cons "GLvoid* const" (pointer-to '_void))
      ;    (cons "GLDEBUGPROCAMD" _GLDEBUGPROCAMD)
      (cons "GLboolean*" (pointer-to '_bool))
      (cons "GLint64" '_int64)
      (cons "GLintptrARB" '_intptr)
      ;    (cons "GLUnurbs*" _GLUnurbs*)
      (cons "GLuint" '_uint32)
      (cons "GLclampf" '_float)
      (cons "GLushort" '_uint16)
      (cons "GLfloat*" (pointer-to '_float)))
                ))



;; more verbatim code


;; and more
#|
; Original version, kept for reference.
(define (cleanup-type-for-doc type)
  (cond
    ((enum-spec? type)
     (if strongly-typed-enums
       (string->symbol (format "gl~a?" (enum-spec-name type)))
       (cleanup-type-for-doc (base-to-ffi-type (enum-spec-type type)))))
    ((list? type)
     (let ((head (car type)))
       (case head
         ((_ptr) (cleanup-type-for-doc (list-ref type 2)))
         ((_vector) `(vectorof ,(cleanup-type-for-doc (list-ref type 2))))
         (else
           (hash-ref vector-to-contract head type)))))
    ((symbol? type)
     (case type
       ((_void) 'void?)
       ((_int8) '(integer-in -128 127))
       ((_uint8) '(integer-in 0 255))
       ((_int16) '(integer-in -32768 32767))
       ((_uint16) '(integer-in 0 65535))
       ((_int32 _intptr _int64) 'exact-integer?)
       ((_uint32 _uint64) 'exact-nonnegative-integer?)
       ((_GLsync) 'GLsync?)
       ((_float) 'flonum?)
       ((_double*) 'real?)
       ((_bool) 'boolean?)
       ((_pointer) 'cpointer?)
       ((_pointer/intptr) 'gl-pointer?)
       ((_string*/utf-8) '(or/c string? bytes?))
       (else 
         (hash-ref vector-to-contract type type))))
    (else type)))
|#
;; end verbatim code

(: cleanup-type-for-doc (-> TType CContract))

(define (cleanup-type-for-doc [type : TType]) ;TODO: ctype, not tye -- name change
  (cond
    ; TODO: First, exceptions.  These show up in this version, but not in the original.
    ; TODO: Find out why.
    #;( (equal? type '())
      (fprintf anomaly "; TODO: Null type ~s~n" type) '())
    ( (equal? type "GLenum") 'exact-integer?)
    ( (equal? type "GLfloat") 'flonum?)
    ( (equal? type "GLuint") 'exact-nonnegative-integer?)
    ( (equal? type '_string*/utf-8) '(or/c string? bytes?)) 
    ( (equal? type '_void) 'any) ;TODO:  why oh why this?
        ; nonxml version says to return any when type says return void
        ; Am I being compatible with a bug?
    ; end of exceptions    
    #;((enum-spec? type) ; TODO: put this back
     (if strongly-typed-enums
       (string->symbol (format "gl~a?" (enum-spec-name type)))
       (cleanup-type-for-doc (base-to-ffi-type (enum-spec-type type)))))
    ((list? type) ; TODO: trouble if type is '()
     (let ((head (car type)))
       (case head
         ((_ptr) (cleanup-type-for-doc (cast (list-ref type 2) TType))) ;; Can I eliminate the cast?
         ((_vector) `(vectorof ,(cleanup-type-for-doc (cast (list-ref type 2) TType)))) ;; Can I eliminate the cast?
         (else
           (hash-ref vector-to-contract (cast head TType)
                     (lambda() (fprintf anomaly "undocumentable type ~s~n" type)
                       (cast (broken-contract type) CContract)))
           ))))
    ((symbol? type)
     (case type
       ((_void) 'void?)
       ((_int8) '(integer-in -128 127))
       ((_uint8) '(integer-in 0 255))
       ((_int16) '(integer-in -32768 32767))
       ((_uint16) '(integer-in 0 65535))
       ((_int32 _intptr _int64) 'exact-integer?)
       ((_uint32 _uint64) 'exact-nonnegative-integer?)
       ((_GLsync) 'GLsync?)
       ((_float) 'flonum?)
       ((_double*) 'real?)
       ((_bool) 'boolean?)
       ((_pointer) 'cpointer?)
       ((_pointer/intptr) 'gl-pointer?)
       ((_string*/utf-8) '(or/c string? bytes?))
       (else 
         (hash-ref vector-to-contract type (lambda() type)))))
    (else (cast type CContract))))

'after-cleanup-type-for-doc-definition

(define racket-contract cleanup-type-for-doc)

;; more verbatim code


;; and more



#| original untyped version
(define (base-to-ffi-type type)
  (cond
    ((hash-ref enums type #f) 
     =>
     (lambda (t2) t2))
    ((hash-ref type-map type #f)
     =>
     (lambda (t2) (hash-ref basic-type-map t2 t2)))
    (else type)))
|#


;; typed version
(define (base-to-ffi-type [type : String])
  (cond
    ((hash-ref enums type #f)
     =>
     (lambda (t2) t2))
    ((hash-ref type-map type #f)
     =>
     (lambda (t2) (hash-ref basic-type-map t2 (lambda () t2))))
    (else type)
    ))

'after-defining-base-to-ffi-type

(: racket-type (-> XML TType))

(define (racket-type [xmltype : XML]) ; translate the simplest type names.
  (if (string? xmltype)
      (hash-ref basic-type-map xmltype
                (lambda () (fprintf anomaly "no basic type for ~s~n" xmltype) (broken-type xmltype))
                )
      (broken-type xmltype)
      )
  )

#|  TODO: convert these or replace when I know their specs from usage.

; This function too is taken from the original nonXML version.
; It too will need to be cleaned up for the xml version.

(define (racket-predicate ctype) ; TODO TODO TODO TODO
  (cond ; First, exceptions that cleanup-type-for-doc doesn't handle yet.
    ( (equal? ctype '())
      (fprintf anomaly "; TODO: Null type ~s~n" ctype) '())
    ( (equal? ctype "GLenum") 'exact-integer?)
    ( (equal? ctype "GLfloat") 'flonum?)
    ( (equal? ctype "void ") 'any) ;TODO:  why oh why this?
        ; nonxml version says to return any when type says return void
        ; Am I being compatible with a bug?
    (#t (cleanup-type-for-doc ctype))
    )
  #;(define rtype (racket-type xmltype))
  #;(cond ((eq? rtype '_int32) 'exact-integer?)
          ((eq? rtype '_uint32) 'exact-nonnegative-integer?)
          ((eq? rtype '_float) 'flonum?)
          ((eq? rtype '_void) 'any)
          (#t rtype)
          )
  )
; --- processing starts here.
|#

(define (process-comment stuff) (fprintf anomaly "; TODO: process comments.~n"))

(define (process-types stuff) (fprintf anomaly "; TODO: process types~n"))

(define (process-groups stuff) (fprintf anomaly "; TODO: process groups~n"))

(define (process-enum-header header)
  (fprintf anomaly "; LAtER: enum-header ~s~n" header)
  )

(define (rehexify [v : String])
  (if (and (string? v) ( >= (string-length v) 2) (equal? (substring v 0 2) "0x"))
      (string-append "#x" (substring v 2))
      v
))

(define (strip-white [s : String]) (string-trim s " " #:repeat? #t))

( : process-enum-item (-> XML Void))
'bar
(define (process-enum-item [item : XML])
  (ann
   (match item
     [ (list 'enum (cons '@ qxmlattributes))
       (define xmlattributes : (Listof Any) (cast qxmlattributes (Listof Any)))
       ;; There ought to be a way to do this with a match, but I haven't found it.
       #;(match xmlattributes [(list-no-order (list 'value v) (list 'name s)(list 'alias alias) ...
                                              ;(or (list 'vendor _) (list 'comment _ ) (list 'type _ ) (list 'api _)) ...
                                              )
                               '() ;; placeholder for generating enum translation
                               ]           )              
       (define names : (Listof Any) '())
       (define values : (Listof String) '())
       (define aliases : (Listof Any) '())
       (define apis : (Listof Any) '())
       (ann    
        (for ([attr : Any xmlattributes])
          
          (match attr
            [(list 'value v) (set! values (cons (cast v String) values))]
            [(list 'name n) (set! names (cons n names))]
            [(list 'alias a) (set! aliases (cons a aliases))]
            [(cons 'comment _) (fprintf anomaly "; LATER: write comments~n")]
            [(cons 'type _)
             (fprintf anomaly "; LATER: Do I need to process types in enumerations?~n")]
            [(list 'api api)
             (set! apis (cons (strip-white (cast api String)) apis))
             (fprintf anomaly "; LATER:  What do I do with API? ~s from ~s apis ~s~n"
                      api attr apis)]
            [ other (fprintf anomaly "; LATER: other enum attribute ~s~n" other)]
            
            )
          )
        Void)
       (unique names "; TODO: not just one name in enum ~s~n" names)
       (unique values "; TODO: not just one value in enum ~s ~s~n" names values)
       (define api (atmostone apis "; TODO: not just one api. ~s~n" apis))
       (fprintf anomaly ";;;;;api is ~s.~n" api)
       (ann (unless (member api '("gles2"))
              #| This is an opengl binding, not a gles binding
         opengl and gles have different definitions for some symbols,
         but Khronos lists all of them in the opengl registry.
         |#
              (for ([name names] [value values])
                ; and there should be only one of each, so only one iteration.
                (fprintf output "(define ~a ~a)~n" name (rehexify value))
                (fprintf output "~s~n" (list 'provide name))
                (for ([a aliases])
                  (fprintf output "; alias for ~s~n" a)
                  #;(fprintf output "~a~n" (list 'provide a))
                  ))
              )
            Void)
       ]
     [(cons 'unused rest) (ann (fprintf anomaly "; LATER: unused ~s~n" item) Void)]
     [ _ (ann (fprintf anomaly "; unknown-enum-item ~s~n" item) Void)]
     )
   Void)
  )

'foo

(: process-enum-list ( -> (Listof XML) Any))

(define (process-enum-list [enum-list : (Listof XML)])
  (if (pair? enum-list)
      (begin
        (process-enum-item (car enum-list))
        (process-enum-list (cdr enum-list))
        )
      (cons 'enum-list-tail enum-list)
      )
  )

(: process-enums ( -> (Listof XML) Any))

(define (process-enums enums)
  (match enums [(cons header enum-list)
                (process-enum-header header)
                (process-enum-list enum-list)]
    [ _ (fprintf anomaly "strange argument to enums. ~s~n" enums) ]
    ))

; Rename types


(struct prototype ([ptype : Any] [name : Symbol] [group : Any]))

( : process-proto [ -> XML prototype])

(define (process-proto [proto : XML])
  ;; I wonder why Khronos considers the param not to be part of the proto?
  ;; They split the identifier's type into two separate XML elements.
  (define ptypes '()) ; should be only one
  (define names '()) ; should be only one
  (define groups '()) ; should be at most one
  (for [(p proto)]
    (match p
      ["void " (set! ptypes (cons '_void ptypes))]
      ; TODO: Is thre an important distinction between void here and a ptype element?
      [(cons '@ qattributes)
       (define attributes (cast qattributes (Listof XML)))
       (for ([a attributes])
         (match a
           [(list 'group g) (set! groups (cons g groups))]
           [ _ 
             (fprintf anomaly "; TODO: What to do with attribute ~s of prototype ~s~n"
                      a proto)]
           )
         )]
      [(list 'name s) (set! names (cons s names))]
      [(list 'ptype s) (set! ptypes (cons s ptypes))
                       (fprintf anomaly "NOTE: ptype directly in proto")
                       ] ; TODO: This seems never to occur.
      [(cons glx rest) (fprintf anomaly "; LATER: what to do with glx? ~s~n" p)]
      [ _ (fprintf anomaly "; TODO: unknown component ~s of prototype ~s~n" p proto)]
      )
    )
    (define name (string->symbol (strip-white
      (cast (unique names ":TODO: too many names ~s in protoype ~s~n" names proto) String))))
    (define ptype
      (cast (unique ptypes ":TODO: too many ptypes ~s in protoype ~s~n" ptypes proto) XML))
  #;(fprintf anomaly "process-proto sees groups ~s~n" groups)
  (define group
    (atmostone groups "; TODO: not just one group ~s in prototype" groups proto))
  (prototype (racket-type ptype) name group) ; Do we want racket-type here?
  )



; example
#;(define (tuple-print tuple port mode)
  (when mode (write-string "<" port))
  (let ([l (tuple-ref tuple)]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))])])
    (unless (zero? (vector-length l))
      (recur (vector-ref l 0) port)
      (for-each (lambda (e)
                  (write-string ", " port)
                  (recur e port))
                (cdr (vector->list l)))))
  (when mode (write-string ">" port)))

 
#;(struct tuple (ref)
        #:methods gen:custom-write
        [(define write-proc tuple-print)])

(define (parameter-print [p : parameter] [port : Output-Port])
  (fprintf port "parameter{~s ~s ~s ~s ~s}"
           (parameter-type p) (parameter-name p) (parameter-len p) (parameter-const? p) (parameter-stars p)
           )
  )

; A parameter structure records the result of parsing an xml param entity.
; It still beeds to be translated into both the racket and the ctype notations.

(struct parameter
 
  ([type : XML] [name : String] [len : String] [const? : Boolean] [stars : Integer])
  ; #:methods gen:custom-write [(define write-proc parameter-print)]
  )

( : primitive-ctype ( -> String TType ))
(define (primitive-ctype [t2 : String]) ; TODO TODO TODO 
  (hash-ref basic-type-map t2 (lambda () (broken-type t2)))
  )

(: parse-param ( -> (Listof XML) parameter)) 
(define (parse-param (param : (Listof XML)))
  ; parse a param entity and produce a struct parameter object
  (define types '())
  (define names '())
  (define lengths '())
  (define const? : Boolean #f)
  (define stars 0)
  (for [(p param)]
    (match p
      [(list '@ (list 'group g))
       (fprintf anomaly
                "; LATER: figure out what these groups in param's are for.~n")]
      [(list 'ptype t) (set! types (cons t types))]
      [(list 'name ( ? string? n)) (set! names (cons (string->symbol (strip-white n)) names))]
      [(or (list '@ (list 'len len))
           (list '@ (list 'len len) (list 'group _))) ; TODO: what's group?
           (set! lengths (cons len lengths))
           (fprintf anomaly "length ~s in param ~s~n" p param)
           ]
      ["const " (set! const? #t)
                #;(fprintf anomaly "const ~s in param ~s~n" p param)] ; TODO: is this really an anomaly here?
      [" *" (set! stars ( + 1 stars))
            (fprintf anomaly "star ~s in param ~s~n" p param)]
      [_ (fprintf anomaly "; TODO: strange param specifier ~s~n" p)]
    ))
  (define type (cast (unique types
                       "; TODO: not just one type ~s in param ~s~n" types param) XML))
  (define name (cast (unique names
                       "; TODO: not just one name ~s in param ~s~n" names param) String))
  (define len (cast (atmostone lengths
                         "; TODO: not just one length ~s in param ~s~n" names param) String) )
  (parameter type name len const? stars)
    #;(list name ': (racket-type type))
  )



(define (param-to-contract) (void))

(: param->ctype ( -> parameter (List Boolean String ': TType)))

(define (param->ctype param) ; TODO TODO TODO Should this be parameter->ctype?
  #;(fprintf trace "DEBUG: param->ctype ~s~n" param)
  (match param
    [ (parameter (or '_byte '_char 'char "GLchar") name (or "COMPSIZE(name)" '()) #t 1)
      (list #f name ': '_string*/utf-8 )]
    [ (parameter "GLuint" name "n" #t 1)
      (list #f name ': '(_u32vector i))]
    [ (parameter "GLboolean" name "n" #f 1)
      (list #t name ': '(_vector o _bool n))] ; the #t indicates that this parameter is used for output.
    [ (parameter t "residences" l c s) ; debugging catchall; should not be used
      (list #f "residences" ': (broken-type (cons "param->ctype" parameter)))]
    [ (parameter t name (not '()) c? (not 0)) (list #f name ': '_cptr) ]
    [ (parameter ( ? string? type) name len const? stars)
      (fprintf trace "DEBUG: param->ctype ~s ~s ~s ~s ~s ~n"
               type name len const? stars)
      (define t (primitive-ctype type))
      (fprintf trace "DEBUGt t ~s~n" t)
#|
     (when (equal? t "GLenum") (set! t '_int32))
     (when (equal? t "GLsizei") (set! t '_int32))
     (when (equal? t "GLuint") (set! t '_uint32))
     (when (equal? t "GLbyte") (set! t '_uint8))
     (when (equal? t "GLfloat") (set! t '_float))
|#
      #;(if (and (equal? stars 1)
              (or (equal? t '_byte)
                  (equal? t '_char) (equal? t 'char) (equal? t "GLchar") ; ><<><>
                  )
              (equal? len "COMPSIZE(name)") const?)
         #| This could be match ( (parameter (or '_byte '_char 'char "GLchar) n "COMPSIZE(name)" #t 1) |#
         (set! t '_string*/utf-8)
         (when (or (not (equal? stars 0)) (not (null? len)))
             (set! t '_cptr)
             )
         )
      (list #f name ': t)
      ]
    [ _ (fprintf anomaly "Unrecognised param ~s~n" param)
        (list #f "broken" ': (broken-type param))]
    )
  )
#;(define (param->ctype param)
  (match param
    [(parameter type name len const? stars)
     #;(fprintf anomaly
      "anomaly: unpacket parameter is type ~s name ~s len ~s const? ~s stars ~s\n"
      type name len const? stars)
     (when (equal? stars 1)
         (cond
           ((and (equal? type '_byte)
              (equal? len "COMPSIZE(name)") const?)
            (set! type '_string*/utf-8))
           ((and (equal? type '_uint))
            (set! type '(_u32vector i) )
            )
           ( (not (null? len))
             (set! type '_cptr)
             )
           )
         )
         
     (list name ': type)
     ]
    [ _ (fprintf anomaly "TODO: What kind of parameter is ~s?~n" param) param]
    )
  )

(: params->ctypes ( -> (Listof parameter) (Listof (List Boolean String ': TType))))
(define (params->ctypes params) ; params contains parameters in ???reverse order
  ; Return a list of the ctypes of the parameters, also in reverse order.
  #;(fprintf trace "DEBUG: params->ctypes ~s~n" params) ; <><>><
  (cond
    ((null? params) '())
    [(pair? params)
     (define p (param->ctype (car params)))
     (cons p (params->ctypes (cdr params)))
     ]
    (#t (fprintf anomaly "; What is ~s doing in my parameter list?~n" params) )
    ))

(: get-output-ctypes ( -> (Listof (List Boolean String ': TType)) (Listof (List String ': TType))))
(define (get-output-ctypes params) ; params is a list of parameters in ???reverse order each in the (#t n : t) or (n : t) form
  ; return a list of the output parameters, also in reerseorder.
  ; Output parameters are parameters which point to memory
  ; where the opengl function is expected to return results.
; TODO: How is this indicaated in the XML?  I am guessing here.
  (cond
    ((null? params) '())
    ((pair? params)
     (if (equal? (caar params) '#t)
         (cons (cdar params) (get-output-ctypes (cdr params)))
         (get-output-ctypes (cdr params))
     ))
    (#t (fprintf anomaly "; What is ~s doing in my parameter list?~n" params) )
    ))

(define (process-command [command : (Listof XML)])
  (fprintf output "~n") ; TODO this line is temporary code
  (define name : (Option Symbol) #f) ; TODO: check there's only one
  (define resulttype '()) ; TODO: check there's only one
  (define group : (Option XML) #f)
  (define proto : (Option prototype) #f); TODO: check there's only one
  (define params : (Listof parameter)'()) ; list of parameters in reverse order
  (define glxtype : (Option String) #f)
  (define opcode : (Option String) #f)
  (for ([item command])
    (match item
      #;[(list 'proto t (list 'name n))
  `       (set! resulttype
`             (if (and (string? t) (equal? "void" (strip-white t))) '_void t))
`         `
`         (set! name (string->symbol (strip-white n)))
       ]
      [(cons 'proto rest)
       (set! proto (process-proto (cast rest (Listof XML))))
       (match proto
         [(prototype t n g) ; formerly list n ': t)
          (when name (fprintf anomaly "Too many names ~s ~s~n" name n))
          (set! name n)
          (when resulttype (fprintf anomaly "Too many result types ~s ~s`n" resulttype t))
 `        (set! resulttype
                (if (and (string? t) (equal? "void" (strip-white t))) '_void t))
          (when group (fprintf anomaly "Too many groupss ~s ~s`n" group g))
 `        (set! group g) #;(fprintf anomaly "proto had group ~s\n" g)
          ]
         [ _ (fprintf anomaly "; TODO: strange proto in command: ~s~n" command)]
         )]         
      [(cons 'param rest) (set! params (cons (parse-param (cast rest (Listof XML))) params))]
      [(list 'glx (list '@ (list 'type (? string? t)) (list 'opcode (? string? o))))
       (set! glxtype t)
       (set! opcode o)
       (fprintf anomaly "; LATER: whatever do i do with item ~s in command ~s~n" item name)]
      [(list 'alias (list '@ (list 'name name)))
       (fprintf output "; alias for ~a~n" name)]
      [(list 'vecequiv (list '@ (list 'name name)))
       (fprintf anomaly "; LATER vecequiv ~s~n" item)]
       
      [ _ (fprintf anomaly "; TODO: unknown command item ~s~n" item) ]
      ))

  (when (null? name)
      #;(fprintf anomaly "; TODO: no name in command definition~n")
    (fprintf anomaly "; TODO: best try:~n #;")
    )

  (fprintf output "TODO: debug: Parameter list is ~s from command ~s~n" params command)
  (fprintf trace "; DEBUGG ~s~n" (map parameter-type params))
  (define args (params->ctypes params))
  (define results : (Listof (List String ': TType)) (get-output-ctypes args))
  (fprintf output "got results ~s from args ~s~n" results args)
;  (define rev-regular-type (cons '-> (map (lambda ([a :(List Boolean String ': TType)]) (cdr a)) args))) ; <><><><>
  (define rev-regular-type (cons '-> (map (ann car (-> (List Boolean String ': TType) Boolean)) args))) ; <><><><>
  (define rev-type
    (if (null? results)
        (cons resulttype rev-regular-type)
        (cons (cons 'values (cons 'result (map (lambda ([a : (List String ': TType)]) (car a)) (cast results (Listof (List String ': TType))))))
              (cons '-> (cons (list 'result ': resulttype) rev-regular-type)))
        ))
  (fprintf output "!!!!! rev-type ~s~n" rev-type)
  (fprintf output "~s~n"
           (list 'define-gl name
                 (- (length params) (length results)) ; This is wroneg.  I suspect is has to be altered when there are output parameters, but I don't really know.
                   (reverse rev-type) ; the ctype of the function
                   (cons '->> ; this part is still very wwrong and currently generates gibberish.
                         (reverse (cons (racket-contract resulttype)
                                             (map racket-contract ; <><><>
                                                  #;(map parameter-type params)
                                                  (map (ann cadddr(-> (List Boolean String ': TType) TType) ) args)
                                                  )
                                             )))
                   (if (equal? group "ErrorCode") 'void 'check-gl-error)
                   ; TODO: when to generate check-gl-error?
                   ; glGetError has void instead of check-gl-error in this spot
                   ;    in the old binding.
                   ; many functions have check-gl-error
                   ; Presumably this is to know when to test for errors on function return
                   ; an connect with Rackets error handling.
                   ; Maybe ... group="ErrorCode" as a prototype attribute should tell me to generate void instead of check-gl-error.
                   )
           )
  (unless (or (null? opcode) (null? glxtype))
      (fprintf anomaly "; LATER: what to do with type ~s opcode ~s~n"
               glxtype opcode))
  )

(define (process-commands [commands : (Listof XML)])
  (for [(command commands)]
    (match command
      [(list '@ (list 'namespace namespace))
       (fprintf anomaly "; LATER: namespace for commands is ~s~n" namespace)
       ]
      [(cons 'command commanddef)
       (process-command (cast commanddef (Listof XML)))]
      [(cons (cons 'namespace spacedef) rest)
       (fprintf anomaly "; TODO: namespace~n")]
      [(cons (cons '@ stuff) rest)
       (fprintf anomaly "; TODO: at-item in command list ~s~n" stuff)]
      ['() '()]
      [ _ (fprintf anomaly "; TODO: unrecognised command~s~n" command)]
      ))
  )

(define (process-regitem (item : (Pair Symbol (Listof XML))))
  (match item
    [(cons 'comment rest) (process-comment rest)]
    [(cons 'types rest) (process-types rest)]
    [(cons 'groups rest) (process-groups rest)]
    [(cons 'enums rest) (if do-enums (process-enums rest) void)]
    [(cons 'commands rest) (process-commands rest)]
    [ _ (fprintf anomaly "; TODO: strange registry item ~s~n" item)]
    ))

(define (process-registry (reg : Any))
  (for [(item (cast reg (Listof (Pair Symbol (Listof XML)))))]
    (process-regitem item)
    )
  )

(define (find-registry (spec : Any))
  (match spec
    [(cons '*TOP*
           (cons (cons '*PI* pi)
                 (list(cons 'registry stuff ))))
     stuff]
    [ _ (cons 'nope: spec)]
    ))

(define (read-spec)
  (define in (open-input-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/api/gl.xml"))
  (printf "in traced: ~s~n" in)
  (define spec (ssax:xml->sxml in '()))
  (close-input-port in)
  spec
)

(process-registry
 (find-registry
  (read-spec)
))


(close-output-port output)
(println "done")
