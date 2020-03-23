#lang racket
(require sxml)

;(require srfi/13) Don't do this.  It has the wrong ersion of string-trim)

; Debugging flags

#| THis program reads the xml specfile that has been downloaded from Khronos
   and writes an (as yet incomplete) binding got openGL on Racket.
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

;; more verbatim code

(define (base-to-ffi-type type)
  (cond
    ((hash-ref enums type #f) 
     =>
     (lambda (t2) t2))
    ((hash-ref type-map type #f)
     =>
     (lambda (t2) (hash-ref basic-type-map t2 t2)))
    (else type)))

;; and more
#;(define (cleanup-type-for-doc type)
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
;; end verbatim code

(define (racket-type xmltype)
  (or (hash-ref basic-type-map xmltype #f) xmltype) ; this one seems to work.
  )

(define (racket-predicate xmltype) ;; TODO: There has to be a better way to do this
    ;; In fact, this whole stratagy is probaby wrong.  It's a stopgap unti I understand enough.
  (define rtype (racket-type xmltype))
    (cond ((eq? rtype '_int32) 'exact-integer?)
          ((eq? rtype '_uint32) 'exact-nonnegative-integer?)
          ((eq? rtype '_float) 'flonum?)
          ((eq? rtype '_void) 'any)
          (#t rtype)
          ))

(define output (open-output-file "/home/hendrik/dv/opengl-project/RacketGL/opengl/results.rkt" #:exists 'replace))
;  regular output file.
; TODO: use call-with-output-port

(define anomaly output) ; the file to which to report anomalies
; NOTE: the distinction between output and anomaly is currently not well respected

(fprintf output "#lang racket~n")
(define export-list '())

; --- processing starts here.

(define (process-comment stuff) (fprintf anomaly "; there was a comment here.")) ; TODO

(define (process-types stuff) (fprintf anomaly "; TODO: process types"))

(define (process-groups stuff) (fprintf anomaly "; TODO: process groups"))

(define (process-enum-header header)
  (fprintf output "; TODO: enum-header ~s~n" header)
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
          [ other (fprintf output "; TODO: other enum attribute ~s~n" other)]
          ;; TODO: Do I have to do anything with type or api?
          )
        )
      (when (not (equal? (length names) 1))
          (fprintf output "; TODO too many names in enum ~s~n" names))
      (when (not (equal? (length names) 1))
          (fprintf output "; TODO too many values i enum ~s ~s~n" names values))
      (for ([name names] [value values])
        (fprintf output "~s~n" (list 'define name (rehexify value)))
        #;(set! export-list (cons name export-list))
        (fprintf output "~s~n" (list 'provide name))
        (for ([a aliases])
          (fprintf output "~s~n" (list 'define a (rehexify value)))
          (fprintf output "; alias for ~s~n " name)
          #;(set! export-list (cons a export-list))
          (fprintf output "~s~n" (list 'provide a))
          ))
      ]
    [(cons 'unused rest) (fprintf output "; unused ~s~n" item)]
    [ _ (fprintf "; unknown-enum-item ~s~n" item)]
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
    [ _ (fprintf "strnge argument to enums. ~s~n" enums) ]
    ))

; Rename types

(define (process-param param)
  (define type '())
  (define name '())
  (for [(p param)]
    (match p
      [(list '@ (list 'group g)) void] ; TODO: firgure out what these groups are for
      [(list 'ptype t) (set! type t)] ; TODO: guard against multiple definition
      [(list 'name n) (set! name n)] ; TODO: guard against multiple definition
      [_ (fprintf output "strange parameter specifier ~s~n" p)]
    ))
  (list name ': (racket-type type))
  )

(define (process-command command)
  (define name '())
  (define resulttype '())
  (define params '()) ; list of parameters in reverse order
  (define glxtype '())
  (define opcode '())
  (for ([item command])
    (match item
      [(list 'proto t (list 'name n))
       (set! resulttype
             (if (and (string? t) (equal? "void" (string-trim t " " #:repeat? #t))) '_void t))
       (set! name n)
       #;(fprintf output "~s~n" (list 'resulttype 'is resulttype))
       ]
      [(cons 'proto rest) (fprintf output "TODO strange proto in command ~s:" command)]
      [(cons 'param rest) (set! params (cons (process-param rest) params))]
      [(list 'glx (list '@ (list 'type t) (list 'opcode o)))
       (set! glxtype t)
       (set! opcode o)
       (fprintf output ";TODO whatever do i do with item ~s in command ~s~n" item name)]
      [ _ (fprintf output ";TODO unonn command item ~s~n" item) ]
      ))
  (fprintf output "~s~n"
           (list 'define-gl name (length params)
                   (reverse (cons resulttype (cons '-> params)))
                   (cons '->> (reverse (cons (racket-predicate resulttype)
                                             (map racket-predicate (map caddr params)))))
                   'check-gl-error ; TODO: when to generate check-gl-error?
                   )
           )
  (fprintf output "~s~n"
           (list "TODO: what to do with" 'type glxtype 'opcode opcode))
  )

(define (process-commands commands)
  (for [(command commands)]
    (match command
      [(list '@ (list 'namespace namespace))
       (fprintf output "~n; TODO: namespace for commands is ~s~n" namespace)
       ; TODO: Why is there a newline within namespace?  Is there still?
        ]
      [(cons 'command commanddef)
       (process-command commanddef)]
      [(cons (cons 'namespace spacedef) rest)
       (fprintf "; namespaceTODO\n")]
      [(cons (cons '@ stuff) rest)
       (fprintf output "TODO at-item in ommand list ~s~n" stuff)]
      ['() '()]
      [ _ (fprintf output "TODO: unrecognised command~s~n" command)]
      ))
  )

(define (process-regitem item)
  (match item
    [(cons 'comment rest) (process-comment rest)]
    [(cons 'types rest) (process-types rest)]
    [(cons 'groups rest) (process-groups rest)]
    [(cons 'enums rest) (if do-enums (process-enums rest) void)]
    [(cons 'commands rest) (process-commands rest)]
    [ _ (fprintf output "; TODO strange registry item ~s~n" (car item))]
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
