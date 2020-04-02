#lang racket
(require sxml)

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

(define messages-in-generated-code #t)

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

(define output (open-output-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/generated/api.inc" #:exists 'replace))
;  regular output file.
; TODO: use call-with-output-port

(define anomaly
  (if suppress-messages (open-output-nowhere)
      (if messages-in-generated-code output
          (current-output-port))))

; the file to which to report anomalies
; NOTE: the distinction between output and anomaly is currently not well respected

;(fprintf output "#lang racket~n") -- to be included, not required
(define export-list '())

(define (unique list message . messageargs)
  ; return the only element of the list, or '() if there is none.
  ; Produce message if not just one. 
  (if (equal? 1 (length list)) (car list)
     (begin
       (apply fprintf (cons anomaly (cons message messageargs)))
       (if (null? list) list (car list))
       )
     )
)

(define (atmostone list message . messageargs)
  ; return the first element of the list, or '() if there is none.
  ; Produce message if there are more than one element in the list.
     (if (null? list) '()
         (begin
           (when ( < 1 (length list))
               (apply fprintf (cons anomaly (cons message messageargs)))
               )
           (car list)
           )
    )
)
; Global variables used to collect information

(define enums (make-hash))
(define type-map (make-hash)) ; TODO: what is this?

;; Here is code lifted verbatim from the original opengl42

; It would be really cool if we could do strict checking of
; the enums. And in fact the info appears to be available in enum.spec
; and I wrote the code to do it. But I quickly found out that enum.spec
; is too buggy to be useful for that purpose. 
; Eveything a C compiler doesn't check is likely to be unreliable in that file...

(define strongly-typed-enums #f)

(define-struct param-spec
               (name type mode shape size))

(define-struct function-spec
               (name
                (params #:mutable)
                (return #:mutable)
                (version #:mutable)
                (category #:mutable)
                (alias #:mutable)
                (deprecated #:mutable)))

(define-struct constant-spec
               (name
                (value #:mutable)))

(define-struct enum-spec
               (name
                 type
                 (constants #:mutable)))

(define-struct mode-dependent-type (in out))

(define type-to-vector
  (make-hash
    '(
      (_int8 . _s8vector)
      (_uint16 . _u16vector)
      (_int16 . _s16vector)
      (_uint32 . _u32vector)
      (_int32 . _s32vector)
      (_uint64 . _u64vector)
      (_int64 . _s64vector)
      (_float . _f32vector)
      (_double* . _f64vector))))

(define vector-to-contract
  (make-hash
    '(
      (_bytes . bytes?)
      (_s8vector . s8vector?)
      (_u16vector . u16vector?)
      (_s16vector . s16vector?)
      (_u32vector . u32vector?)
      (_s32vector . s32vector?)
      (_u64vector . u64vector?)
      (_s64vector . s64vector?)
      (_f32vector . f32vector?)
      (_f64vector . f64vector?))))


(define (pointer-to type . args)
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

#|; This is the basc type map from the old spec reader.
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

;; more verbatim code

#| original version
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

;; my version so far
(define (base-to-ffi-type type)
  (cond
    #;((hash-ref enums type #f) 
     =>
     (lambda (t2) t2))
    #;((hash-ref type-map type #f)
     =>
     (lambda (t2) (hash-ref basic-type-map t2 t2)))
    ((hash-ref basic-type-map type type) => (lambda (t2) t2))
    (else type)
    ))

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

; TODO:  There are too many of these.  Clean up and refactor

; This functino is from the non-XML version of this program.
; TODO: remove notations that cannot occur in the XML verison.  
(define basic-type-map
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
  
(define (racket-type xmltype) ; translate the simplest type names.
  (or (hash-ref basic-type-map xmltype #f) xmltype) ; this one seems to work.
  )

; This function too is taken from the original nonXML version.
; It too will need to be cleaned up for the xml version.
(define (cleanup-type-for-doc type) ;TODO: ctype, not tye -- name change
  (cond
    ; TODO: First, exceptions.  These show up in this version, but not in the original.
    ; TODO: Find out why.
    ( (equal? type '())
      (fprintf anomaly "; TODO: Null type ~s~n" type) '())
    ( (equal? type "GLenum") 'exact-integer?)
    ( (equal? type "GLfloat") 'flonum?)
    ( (equal? type "GLuint") 'exact-nonnegative-integer?)
    ( (equal? type '_string*/utf-8) '(or/c string? bytes?)) 
    ( (equal? type '_void) 'any) ;TODO:  why oh why this?
        ; nonxml version says to return any when type says return void
        ; Am I being compatible with a bug?
    ; end of exceptions    
    ((enum-spec? type)
     (if strongly-typed-enums
       (string->symbol (format "gl~a?" (enum-spec-name type)))
       (cleanup-type-for-doc (base-to-ffi-type (enum-spec-type type)))))
    ((list? type) ; TODO: touble if type is '()
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

(define (racket-predicate ctype)
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

(define (process-comment stuff) (fprintf anomaly "; TODO: process comments.~n"))

(define (process-types stuff) (fprintf anomaly "; TODO: process types~n"))

(define (process-groups stuff) (fprintf anomaly "; TODO: process groups~n"))

(define (process-enum-header header)
  (fprintf anomaly "; LAtER: enum-header ~s~n" header)
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
      (define apis '())
          
      (for ([attr xmlattributes])
        (match attr
          [(list 'value v) (set! values (cons v values))]
          [(list 'name n) (set! names (cons n names))]
          [(list 'alias a) (set! aliases (cons a aliases))]
          [(cons 'comment _) (fprintf anomaly "; LATER: write comments~n")]
          [(cons 'type _)
           (fprintf anomaly "; LATER: Do I need to process types in enumerations?~n")]
          [(list 'api api)
           (set! apis (cons (strip-white api) apis))
           (fprintf anomaly "; LATER:  What do I do with API? ~s from ~s apis ~s~n"
                    api attr apis)]
          [ other (fprintf anomaly "; LATER: other enum attribute ~s~n" other)]
          )
        )
      (unique names "; TODO: not just one name in enum ~s~n" names)
      (unique values "; TODO: not just one value in enum ~s ~s~n" names values)
      (define api (atmostone apis "; TODO: not just one api. ~s~n" apis))
      (fprintf anomaly ";;;;;api is ~s.~n" api)
      (unless (member api '("gles2"))
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
      ]
    [(cons 'unused rest) (fprintf anomaly "; LATER: unused ~s~n" item)]
    [ _ (fprintf anomaly "; unknown-enum-item ~s~n" item)]
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
    [ _ (fprintf "strange argument to enums. ~s~n" enums) ]
    ))

; Rename types

(struct prototype (ptype name group))

(define (process-proto proto)
  ;; I wonder why Khronos considers the parameter not to be part of the proto?
  ;; They split the identifier's type into two separate XML elements.
  (define ptypes '()) ; should be only one
  (define names '()) ; should be only one
  (define groups '()) ; should be at most one
  (for [(p proto)]
    (match p
      ["void " (set! ptypes (cons '_void ptypes))]
      ; TODO: Is thre an important distinction between void here and a ptype element?
      [(cons '@ attributes)
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
      (unique names ":TODO: too many names ~s in protoype ~s~n" names proto))))
    (define ptype
      (unique ptypes ":TODO: too many ptypes ~s in protoype ~s~n" ptypes proto))
  #;(fprintf anomaly "process-proto sees groups ~s~n" groups)
  (define group
    (atmostone groups "; TODO: not just one group ~s in prototype" groups proto))
  (prototype name (racket-type ptype) group) ; Do we want racket-type here?
  )

(define (strip-white s) (string-trim s " " #:repeat? #t))

(struct parameter (type name len const? stars))

(define (primitive-ctype t2)
  (hash-ref basic-type-map t2 t2)
  )
  
(define (process-param param)
  (define types '())
  (define names '())
  (define lengths '())
  (define const? #f)
  (define stars 0)
  (for [(p param)]
    (match p
      [(list '@ (list 'group g))
       (fprintf anomaly
                "; LATER: figure out what these groups in param's are for.~n")]
      [(list 'ptype t) (set! types (cons t types))]
      [(list 'name n) (set! names (cons (string->symbol (strip-white n)) names))]
      [(or (list '@ (list 'len len))
           (list '@ (list 'len len) (list 'group _))) ; TODO: what's group?
           (set! lengths (cons len lengths))
           (fprintf anomaly "length ~s in parameter ~s~n" p param)
           ]
      ["const " (set! const? #t)
                (fprintf anomaly "const ~s in parameter ~s~n" p param)]
      [" *" (set! stars ( + 1 stars))
            (fprintf anomaly "star ~s in parameter ~s~n" p param)]
      [_ (fprintf anomaly "; TODO: strange parameter specifier ~s~n" p)]
    ))
  (define type (unique types
                       "; TODO: not just one type ~s in param ~s~n" types param))
  (define name (unique names
                       "; TODO: not just one name ~s in param ~s~n" names param))
  (define len (atmostone lengths
                         "; TODO: not just one length ~s in param ~s~n" names param)) 
  (parameter type name len const? stars)
    #;(list name ': (racket-type type))
  )

(define (param-to-contract) (void))

(define (param->ctype param)
  (fprintf anomaly "DEBUG: param->ctype ~s~n" param)
  (match param
    [(parameter type name len const? stars)
     (fprintf anomaly "DEBUG: param->ctype ~s ~s ~s ~s ~s ~n"
              type name len const? stars)
     (define t (primitive-ctype type))
     (fprintf anomaly "DEBUGt t ~s~n" t)
#|     (when (equal? t "GLenum") (set! t '_int32))
     (when (equal? t "GLsizei") (set! t '_int32))
     (when (equal? t "GLuint") (set! t '_uint32))
     (when (equal? t "GLbyte") (set! t '_uint8))
     (when (equal? t "GLfloat") (set! t '_float))
|#
     (if (and (equal? stars 1)
              (or (equal? t '_byte)
                  (equal? t '_char) (equal? t 'char) (equal? t "GLchar") ; ><<><>
                  )
              (equal? len "COMPSIZE(name)") const?)
         (set! t '_string*/utf-8)
         (when (or (not (equal? stars 0)) (not (null? len)))
             (set! t '_cptr)
             )
         )
     (list name ': t)
     ]
    [ _ (fprintf anomaly "Unrecognised param ~s~n" param)
        param]
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
    
(define (params->ctypes params) ; params contains parameters in ???reverse order.
  #;(fprintf output "DEBUG: params->ctypes ~s~n" params) ; <><>><
  (cond
    ((null? params) '())
    ((pair? params)
     (cons (param->ctype (car params)) (params->ctypes (cdr params))))
    (#t (fprintf anomaly "What is ~s doing in my parameter list?~n" params) )
    ))

(define (process-command command)
  (define name '()) ; TODO: check there's only one
  (define resulttype '()) ; TODO: check there's only one
  (define group '())
  (define proto '()) ; TODO: check there's only one
  (define params '()) ; list of parameters in reverse order
  (define glxtype '())
  (define opcode '())
  (for ([item command])
    (match item
      #;[(list 'proto t (list 'name n))
       (set! resulttype
             (if (and (string? t) (equal? "void" (strip-white t))) '_void t))
       (set! name (string->symbol (strip-white n)))
       ]
      [(cons 'proto rest)
       (set! proto (process-proto rest))
       (match proto
         [(prototype n t g) ; formerly list n ': t)
          (set! name n)
          (set! resulttype
                (if (and (string? t) (equal? "void" (strip-white t))) '_void t))
          (set! group g) #;(fprintf anomaly "proto had group ~s\n" g)
          ]
         [ _ (fprintf anomaly "; TODO: strange proto in command: ~s~n" command)]
         )]         
      [(cons 'param rest) (set! params (cons (process-param rest) params))]
      [(list 'glx (list '@ (list 'type t) (list 'opcode o)))
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
  (fprintf anomaly "TODO: debug: Parameter list is ~s from command ~s~n" params command)
  (fprintf anomaly "; DEBUGG ~s~n" (map parameter-type params))
  (fprintf output "~s~n"
           (list 'define-gl name (length params)
                   (reverse (cons resulttype (cons '-> (params->ctypes params))))
                   (cons '->> (reverse (cons (racket-predicate resulttype)
                                             (map racket-predicate ; <><><>
                                                  #;(map parameter-type params)
                                                  (map caddr (params->ctypes params))
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

(define (process-commands commands)
  (for [(command commands)]
    (match command
      [(list '@ (list 'namespace namespace))
       (fprintf anomaly "; LATER: namespace for commands is ~s~n" namespace)
       ]
      [(cons 'command commanddef)
       (process-command commanddef)]
      [(cons (cons 'namespace spacedef) rest)
       (fprintf "; TODO: namespace~n")]
      [(cons (cons '@ stuff) rest)
       (fprintf anomaly "; TODO: at-item in command list ~s~n" stuff)]
      ['() '()]
      [ _ (fprintf anomaly "; TODO: unrecognised command~s~n" command)]
      ))
  )

(define (process-regitem item)
  (match item
    [(cons 'comment rest) (process-comment rest)]
    [(cons 'types rest) (process-types rest)]
    [(cons 'groups rest) (process-groups rest)]
    [(cons 'enums rest) (if do-enums (process-enums rest) void)]
    [(cons 'commands rest) (process-commands rest)]
    [ _ (fprintf anomaly "; TODO: strange registry item ~s~n" (car item))]
    ))

(define (process-registry reg)
  (for [(item reg)]
    (process-regitem item)
    )
  )

(define (find-registry spec)
  (match spec
    [(cons '*TOP*
           (cons (cons '*PI* pi)
                 (list(cons 'registry stuff )))) stuff
                                                      ]
    [ _ (cons 'nope: spec)]
    ))

(define (read-spec)
  (define in (open-input-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/api/gl.xml"))
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
