; 2.73

; 1. sum? and product? are checks for a specific symbol that is contained within
;    the representation of a sum and product (you can see in the selector that we
;    grab the operator with (car exp). This would not work for a number or variable,
;    as we're not representing those built in types in the same way.

; 2. 
(define (install-deriv-package)
  (define (deriv-sum operands var)
    (let ((addend (car operands))
          (augend (cadr operands)))
      (make-sum (deriv-old addend var)
                (deriv-old augend var))))
  (define (deriv-product operands var)
    (let ((multiplier (car operands))
          (multiplicand (cadr operands)))
      (make-sum
        (make-product 
          multiplier
          (deriv-old multiplicand var))
        (make-product 
          (deriv-old multiplier var)
          multiplicand))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)

; 3.
; In order to add the exponential rule:

(define (deriv-exponential operands var)
  (let ((base (car operands))
        (exponent (cadr operands)))
    (make-product
      (make-product exponent
                    (make-exponentiation base
                                         (make-sum exponent -1)))
      (deriv base var))))

; to register it:
(put 'deriv '** deriv-exponential)

; 4. The functions would have to be registered with the first and second arguments
;    in the opposite order.

; 2.75

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* x (cos y)))
          ((eq? op 'imag-part)
           (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op: 
            MAKE-FROM-MAG-ANG" op))))
  dispatch)

; 2.76

; Explicit dispatch
; -----------------
; Adding a new operation would require a new function to be defined, where we would
; need to ensure all the supported types were included.
;
; Adding a new type would require us to find all existing functions, updating each with
; the relevant handling code for the new type.

; Message passing
; ---------------
; Adding new operations (assuming we would want to add the operation for all existing
; types) would require us to find all existing constructor functions and add a new
; clause to the `cond` statement.

; Adding a new type would require a new constructor function for the new type, where
; we would define all the required operations. Requires no changes to existing code.

; Data dispatch
; -------------
; Adding new operations would require us to find the existing packages for existing
; types, and register a new function with the correct operation and type. Alternatively,
; we could just directly register the new type and operation combinations in the table.

; Adding a new type would require a new package to be created to support the new type,
; which would need to be installed by the user.

; 2.77

; When calling `(magnitude z)`:
; Initial creation of the complex number was done by:
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(make-complex-from-real-imag 3 4)
; Which calls:
(tag (make-from-real-imag x y))
; Where make-from-real-imag is:
((get 'make-from-real-imag 
          'rectangular) 
     x y)
; Which is defined in the rectangular package as:
(tag (cons x y))
; The first tag function is defined as:
(attach-tag 'complex z)
; And the second tag function is defined as:
(attach-tag 'rectangular x)
; Overall you'd end up with something like:
(attach-tag 'complex (attach-tag 'rectangular (cons 3 4)))
; Where attach tag just ends up cons-ing the tag and the content
; i.e. you'd end up with a structure like '(complex rectangular 3 . 4)

; Calling (magnitude z) would result in the following calls:
(apply-generic 'magnitude z)
; Which would grab the function from within the complex number package, and then
; strip off the first type tag, resulting in a structure like '(rectangular 3 . 4).
; Then, the following would be called AGAIN:
(apply-generic 'magnitude z)
; However, this time, it would be sent to the rectangular number package instead of
; the complex number package. This would then grab the function defined internally
; in the rectangular number package:
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

; The reason that calling the same function twice works is due to the stripping of
; the outer type tag in the `apply-generic` function.

; 2.78

; The functions could be reworked to allow primitive scheme numbers:
(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
                     TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
                     CONTENTS" datum))))

; 2.79

; Adding equ? function to each number package

; To register in the scheme-number package:
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))
; To register in the rational-number package:
(put 'equ? '(rational rational)
     (lambda (x y)
       (and (= (numer x) (numer y))
            (= (denom x) (denom y)))))
; To register in the complex-number package:
(put 'equ? '(complex complex)
     (lambda (x y)
       (and (= (real-part x) (real-part y))
            (= (imag-part x) (imag-part y)))))

; 2.80

; Adding =zero? function to each number package

; To register in the scheme-number package:
(put '=zero? '(scheme-number)
     (lambda (x) (zero? x)))
; To register in the rational-number package:
(put '=zero? '(rational-number)
     (lambda (x) (= (numer x) 0)))
; To register in the complex-number package:
(put '=zero? '(complex-number)
     (lambda (x) (and (= (real-part x) 0)
                      (= (imag-part x) 0))))

; 2.81

; Pt. 1:

; If we called the exp function with two complex numbers as arguments, there would
; be no function found at the first attempt. When following the false part of the if
; statement, we would find a coercion from complex->complex, and then call apply-generic
; again, and again, in an endless loop.

; Pt. 2:

; The current implementation works OK without doing anything about coercing the same types.
; However, there is somewhat of a waste of computation looking up coercions when not needed
; for the same type.

; Pt. 3:

; The if statement 
(if (= (length args) 2)
    ...
    ...)
; can be amended to also check equality of types:
(if (and (= (length args) 2)
         (not (eq? (car type-tags) (cadr type-tags))))
    ...
    ...)

; 2.82

; New function to not bother with coercions if all types are already the same:
(define (all-same-type? type args)
  (or (null? args)
      (and (eq? type (type-tag (car args)))
           (all-same-type? type (cdr args)))))

; Function to receive a list of type tags and args, and return either the
; coerced list, or false
(define (find-coercion type-tags args)
  (and (not (null? type-tags))
       (or (coerce-all-to-type (car type-tags) args)
           (find-coercion (cdr type-tags) args))))

; Takes a list of args and tries to coerce to the specified type, or returns false
(define (coerce-all-to-type type args)
  (define (iter args coerced)
    (cond ((null? args) coerced)
          ((eq? (type-tag (car args)) type)
           (iter (cdr args)
                 (append coerced (list (car args)))))
          ((get-coercion (type-tag (car args)) type)
           (iter (cdr args)
                 (append coerced (list ((get-coercion (type-tag (car args)) type)
                                        (car args))))))
          (else false)))
  (iter args '()))

; Updated version of apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (> (length args) 1)
                   (not (all-same-type? (car type-tags) args)))
              (let ((coerced (find-coercion type-tags args)))
                (if coerced
                    (apply apply-generic (cons op coerced))
                    (error 
                      "No method for 
                      these types"
                      (list 
                        op 
                        type-tags))))
                (error 
                  "No method for these types"
                  (list op type-tags)))))))

; Note that we'd need to ensure that the `get` function could handle more than two
; arguments. For example, if we registered an `add` function with '(rational rational),
; that would also need to match if the type list was two or more rationals. Note that,
; if we changed apply-generic to instead only send up to two types, that could end up
; breaking if, say, the third argument was of a different type.

; 2.83

; First of all we'll need to replace the scheme-number package with an integer package
; and a real package:
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "Not an integer!"))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (zero? x)))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'level 'integer (lambda (x) 0))
  'done)

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "Not a real number"))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real)
       (lambda (x) (zero? x)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  'done)

; To raise integer to rational, you can see the function registered above in the
; integer package. Same for real -> complex

; To raise rational to real:
(put 'raise '(rational)
     (lambda (x) (make-real (/ (numer x) (denom x)))))

; 2.84

; For integer package:
(put 'level '(integer)
     (lambda (x) 0))
; For rational package:
(put 'level '(rational)
     (lambda (x) 1))
; For real package:
(put 'level '(real)
     (lambda (x) 2))
; For complex package:
(put 'level '(complex)
     (lambda (x) 3))

; We can then use the level to decide, for each argument, how many levels to raise
; it, in order to bring all arguments to the same level:
; repeated function was defined in chapter 1.4
(define (coerce-all-to-same-level args)
  (if (null? args)
      '()
      (cons ((repeated raise (- (apply max (map level args))
                                (level (car args))))
             (car args))
            (coerce-all-to-same-level (cdr args))))))

; Note that, in order to add a new level to the tower, it may involve remembering
; to amend old levels (if we are adding a new intermediate level).

; Then apply generic becomes even simpler. We don't need to do any recursive type
; casting - once we've raised all arguments to the same level, we either find a
; procedure to apply, or we don't. Note it's important we still keep the check on
; same types, otherwise it would recurse forever
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (> (length args) 1)
                   (not (all-same-elements? type-tags)))
              (apply apply-generic (cons op (coerce-all-to-same-level args)))
              (error 
                "No method for these types"
                (list op type-tags)))))))

; Changed the same types check to be a generic helper function to check whether a given
; list only contains one repeated element:
(define (all-same-elements? sequence)
  (define (iter comparison-element remaining)
    (or (null? remaining)
        (and (eq? comparison-element (car remaining))
             (iter comparison-element (cdr remaining)))))
  (iter (car sequence) sequence))

; 2.85

; Project complex to real:
(put 'project '(complex) (make-real (real-part x)))

; Project real to rational. Note that all real numbers in scheme are also rational due
; to how numbers are represented, so casting from real to rational should ALWAYS work:

; Function internal to the rational package:
(define (project x)
  (define (iter numer denom)
    (if (integer? numer)
        (make-rational (inexact->exact numer) denom)
        (iter (* numer 10.0) (* denom 10))))
  (iter x 1))

(put 'project '(real) project)

; Project rational to integer:
(put 'project '(rational) (make-integer (numer x)))

(define (project x)
  (apply-generic 'project x))

; Implementation of drop function. Note that we're specifically calling get first to
; check existence of the project function, since it's not defined for integer, and would
; cause an error if we tried to call apply generic for project operation on type integer.
; The function stops when it can't be dropped any further, or alternatively if it can't
; be projected any further:
(define (drop x)
  (let ((project (get 'project (list (type-tag x)))))
    (cond ((not project) x)
          ((apply-generic 'equ? x (raise (apply-generic 'project x)))
           (drop (apply-generic 'project x)))
          (else x))))

; Since drop is defined in terms of apply-generic, it's impossible to embed a call to drop
; within apply-generic. Even if the above function is redefined using get directly, there
; are further issues as some of the project and raise operations also rely on apply-generic.

; You'd be able to create a function along the lines of:
(define (apply-generic-and-drop op . args)
  (drop (apply apply-generic (cons op args))))

; 2.86

; Within the complex package, all basic arithmetic on integers and real numbers must be
; replaced with the generic operators, and in addition, when creating a complex number,
; we must use the `make-real`, `make-rational` or `make-integer` constructors, like:
(define c1 (make-complex-from-real-imag (make-integer 4) (make-integer 2)))

; Some of the internal procedures that have been rewritten. For example, take `add-complex`
; - instead of using +, we're using the generic `add` function, which will allow a real,
; rational or integer typed number:
(define (add-complex z1 z2)
  (make-from-real-imag
    (add (real-part z1) (real-part z2))
    (add (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag 
    (sub (real-part z1) (real-part z2))
    (sub (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang 
    (mul (magnitude z1) (magnitude z2))
    (add (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang 
    (div (magnitude z1) (magnitude z2))
    (sub (angle z1) (angle z2))))
(define (=zero? x)
  (and (=zero? (real-part x))
       (=zero? (imag-part x))))

; We need to define the following operations on real, rational and integer packages:
; - sine
; - cosine
; - square-root
; - square
; - arc-tangent

; Of course, we can write up the corresponding function definitions:
(define (square x)
  (apply-generic 'square x))
(define (sine x)
  (apply-generic 'sine x))
(define (cosine x)
  (apply-generic 'cosine x))
(define (square-root x)
  (apply-generic 'square-root x))
(define (arc-tan x)
  (apply-generic 'arc-tan x))

; To install in the integer package. Note that for most of the operations, they will return
; a real number, not an integer.
(put 'sine '(integer)
     (lambda (x) (make-real (sin x))))
(put 'cosine '(integer)
     (lambda (x) (make-real (cos x))))
(put 'arc-tan '(integer)
     (lambda (x) (make-real (atan x))))
(put 'square '(integer)
     (lambda (x) (tag (* x x))))
(put 'square-root '(integer)
     (lambda (x) (make-real (sqrt x))))

; To install in the real package. All operations also return a real number:
(put 'sine '(real)
     (lambda (x) (tag (sin x))))
(put 'cosine '(real)
     (lambda (x) (tag (cos x))))
(put 'arc-tan '(real)
     (lambda (x) (tag (atan x))))
(put 'square '(real)
     (lambda (x) (tag (* x x))))
(put 'square-root '(real)
     (lambda (x) (tag (sqrt x))))

; To install in the rational package. All operations return a real number, and while
; we could project them down again, the results of trigonometry operations can only
; be projected back to a rational due to limitations of the software. They shouldn't
; really be rational numbers:
(put 'sine '(rational)
     (lambda (x) (make-real (sin (/ (numer x) (denom x))))))
(put 'cosine '(rational)
     (lambda (x) (make-real (cos (/ (numer x) (denom x))))))
(put 'arc-tan '(rational)
     (lambda (x) (make-real (atan (/ (numer x) (denom x))))))
(put 'square '(rational)
     (lambda (x) (tag (make-rat (* (numer x) (numer x))
                                (* (denom x) (denom x))))))
(put 'square-root '(rational)
     (lambda (x) (make-real (sqrt (/ (numer x) (denom x))))))

; Note, we could also reimplement sine, cosine, arc-tan and square-root to return a
; rational number, due to the fact that we can't physically represent an irrational
; number. We'd do this by raising, applying the generic function, and projecting
; again. This would however require three calls to `apply-generic`, as opposed to
; the once in the above versions:
(put 'sine '(rational)
     (lambda (x) (project (sine (raise x)))))

; 2.87

; To register =zero? in the polynomial package. Note that since we're also using
; the 'global' =zero? function, this function is named =zero-internal? to avoid
; the name clash:
(define (=zero-internal? p)
  (define (iter term-list)
    (or (null? term-list)
        (and (=zero? (coeff (car term-list)))
             (iter (cdr term-list)))))
  (iter (term-list p)))

(put '=zero? '(polynomial) =zero-internal?)

; 2.88

(define (negate-poly p)
  (define (iter remaining)
    (if (null? remaining)
        '()
        (cons (make-term (order (car remaining))
                         (sub (make-integer 0) (coeff (car remaining))))
              (iter (cdr remaining)))))
  (make-poly (variable p)
             (iter (term-list p))))

(put 'sub '(polynomial polynomial)
     (lambda (p1 p2)
       (tag (add-poly p1 (negate-poly p2)))))

; 2.89

; Primary changes:
; - adjoin-term becomes more complex
; - we no longer need the coeff function
; - we no longer need the make-term function
; ^ Note the above two functions COULD be kept as noops but the differences
; required between the two implementations is quite significant that there's
; no real point trying to keep them compatible
; - order function takes the remaining sublist to work out what the order is
; - we need to pass the order as an argument to mul-term-by-all-terms function,
;   as we can't work it out from the term alone any more

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (=zero-internal? p)
    (define (iter term-list)
      (or (null? term-list)
          (and (=zero? (car term-list))
               (iter (cdr term-list)))))
    (iter (term-list p)))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (variable? x) (symbol? x))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              ADD-POLY"
               (list p1 p2))))
  (define (negate-poly p)
    (define (iter remaining)
      (if (null? remaining)
          '()
          (cons (sub (make-integer 0)
                     (car remaining))
                (iter (cdr remaining)))))
    (make-poly (variable p)
               (iter (term-list p))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              MUL-POLY"
               (list p1 p2))))
  (define zero-term (make-integer 0))
  (define (adjoin-term term-order term-coeff term-list)
    (cond ((=zero? term-coeff) term-list)
          ((= term-order (+ 1 (order term-list)))
           (cons term-coeff term-list))
          ((> term-order (order term-list))
           (adjoin-term term-order
                        term-coeff
                        (cons zero-term term-list)))
          (else (error "Cannot adjoin term of lower order than term list -- ADJOIN-TERM"
                       (list term-order term-coeff term-list)))))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
    (null? term-list))
  (define (order sublist) (+ (length sublist) 1))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let* ((t1 (first-term L1)) 
                  (t2 (first-term L2))
                  (o1 (order L1))
                  (o2 (order L2)))
             (cond ((> o1 o2)
                    (adjoin-term
                     o1
                     t1 
                     (add-terms (rest-terms L1) 
                                L2)))
                   ((< o1 o2)
                    (adjoin-term
                     o2
                     t2 
                     (add-terms 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                      o1
                      (add t1 t2)
                      (add-terms 
                       (rest-terms L1)
                       (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) (order L1) L2)
         (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 t1-order L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (+ t1-order (order L))
            (mul t1 t2)
           (mul-term-by-all-terms 
            t1
            t1-order
            (rest-terms L))))))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 (negate-poly p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-internal?)
  'done)

; 2.90

; One way of achieving both a sparse and dense package would be to define a slim
; polynomial package like so. The package above would become dense polynomial package
; (with some amendments), and the old version of the package would become the sparse
; polynomial package.

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul p1 p2))))
  (put 'make-sparse 'polynomial
       (lambda (var terms)
         (tag ((get 'make-poly 'sparse) var terms))))
  (put 'make-dense 'polynomial
       (lambda (var terms)
         (tag ((get 'make-poly 'dense) var terms))))
  (put '=zero? '(polynomial) =zero?)
  'done)

; 2.91

; Internal functions to sparse-polynomial package:
(define (div-poly p1 p2)
  (if (eq? (variable p1) (variable p2))
      (let ((quotient-and-remainder (div-terms (term-list p1)
                                               (term-list p2))))
        (list (make-poly (variable p1) (car quotient-and-remainder))
              (make-poly (variable p1) (cadr quotient-and-remainder))))
      (error "variables do not match in DIV-POLY" (list p1 p2))))

(define (zero-termlist)
  (adjoin-term (make-term 0 (make-integer 0))
               (the-empty-termlist)))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) 
            (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) 
                              (coeff t2)))
                  (new-o (- (order t1) 
                            (order t2))))
              (let ((rest-of-result
                      (if (=zero-internal? (make-poly 'x L1))
                          (list (zero-termlist) (zero-termlist))
                          (div-terms (term-list (sub-poly (make-poly 'x L1)
                                                          (mul-poly (make-poly 'x (list (make-term new-o new-c)))
                                                                    (make-poly 'x L2))))
                                     L2))))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))

; Registering the function in sparse package:
(put 'div '(sparse sparse)
     (lambda (p1 p2)
       (let ((quotient-and-remainder (div-poly p1 p2)))
         (list (tag (car quotient-and-remainder))
               (tag (cadr quotient-and-remainder))))))

; Registering the function in polynomial package:
(put 'div '(polynomial polynomial)
     (lambda (p1 p2)
       (let ((quotient-and-remainder (div p1 p2)))
         (list (tag (car quotient-and-remainder))
               (tag (cadr quotient-and-remainder))))))
