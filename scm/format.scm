;;; "format.scm" Common LISP text output formatter for SLIB
; Written 1992-1994 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
; 2004 Aubrey Jaffer: made reentrant; call slib:error for errors.
;
; This code is in the public domain.

; Authors of the original version (< 1.4) were Ken Dickey and Aubrey Jaffer.
; Please send error reports to the email address above.
; For documentation see slib.texi and format.doc.
; For testing load formatst.scm.
;
; Version 3.1

; Modified for CHICKEN Kon Lovett, Sep 25 2005
;
; - depends on srfi-13 functionality
;
; - no local defines for string & number operations
;
; - unprocessed arguments are not an error
;
; - fix for E format; wasn't leaving off leading 0 when result-len > len
; so considered overflow
;
; - explicit use of fixnum arithmetic
;
; - keeps format:* style naming
;
; - exports configuration symbols
;
; - does not use intermediate string when output is a port
;
; - moved defines to toplevel


(declare
	(no-bound-checks)
	(no-argc-checks)
	(no-procedure-checks)
	(always-bound
		format:error-save)
)

(module format
    (format:symbol-case-conv
		format:iobj-case-conv
		format:expch
		format:iteration-bounded
		format:max-iterations
		format:floats
		format:complex-numbers
		format:radix-pref
		#;format:ascii-non-printable-charnames
		format:fn-max
		format:en-max
		format:unprocessed-arguments-error?
		#;format:version
		#;format:iobj->str
		format)

(import scheme)
(import (chicken base))
(import (chicken port))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken fixnum))
(import (srfi 13))


;;; Configuration ------------------------------------------------------------

(define format:symbol-case-conv #f)
;; Symbols are converted by symbol->string so the case of the printed
;; symbols is implementation dependent. format:symbol-case-conv is a
;; one arg closure which is either #f (no conversion), string-upcase!,
;; string-downcase! or string-capitalize!.

(define format:iobj-case-conv #f)
;; As format:symbol-case-conv but applies for the representation of
;; implementation internal objects.

(define format:expch #\E)
;; The character prefixing the exponent value in ~e printing.

(define format:iteration-bounded #t)
;; If #t, "~{...~}" iterates no more than format:max-iterations times;
;; if #f, there is no bound.

(define format:max-iterations 100)
;; Compatible with previous versions.

(define format:floats #t)
;; Detects if the scheme system implements flonums (see at eof).

(define format:complex-numbers #f)
;; Detects if the scheme system implements complex numbers.
;; See use below for invocation-time detection of complex support.

(define format:radix-pref (char=? #\# (string-ref (number->string 8 8) 0)))
;; Detects if number->string adds a radix prefix.

(define format:ascii-non-printable-charnames
  '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
     "bs"  "ht"  "nl"  "vt"  "np"  "cr"  "so"  "si"
     "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
     "can" "em"  "sub" "esc" "fs"  "gs"  "rs"  "us" "space"))

(define format:fn-max 200)              ; max. number of number digits
(define format:en-max 10)               ; max. number of exponent digits

(define format:unprocessed-arguments-error? #f) ; CL says this is not an error

;;; End of configuration ----------------------------------------------------

(define format:version "3.1")

(define format:space-ch (char->integer #\space))
(define format:zero-ch (char->integer #\0))

(define format:parameter-characters
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\v #\# #\') )

(define format:conditional-directives-characters
	(append '(#\[ #\] #\; #\: #\@ #\^) format:parameter-characters) )

(define format:iteration-directives-characters
	(append '(#\{ #\} #\: #\@ #\^) format:parameter-characters) )

;; cardinals & ordinals (from dorai@cs.rice.edu)

(define format:cardinal-thousand-block-list
  '#("" " thousand" " million" " billion" " trillion" " quadrillion"
				" quintillion" " sextillion" " septillion" " octillion" " nonillion"
				" decillion" " undecillion" " duodecillion" " tredecillion"
				" quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
				" octodecillion" " novemdecillion" " vigintillion") )

(define format:cardinal-ones-list
  '#(#f "one" "two" "three" "four" "five"
				"six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
				"fourteen" "fifteen" "sixteen" "seventeen" "eighteen"
				"nineteen") )

(define format:cardinal-tens-list
  '#(#f #f "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty"
					 "ninety") )

(define format:ordinal-ones-list
  '#(#f "first" "second" "third" "fourth" "fifth"
				"sixth" "seventh" "eighth" "ninth" "tenth" "eleventh" "twelfth"
				"thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth"
				"eighteenth" "nineteenth") )

(define format:ordinal-tens-list
  '#(#f #f "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth"
				"seventieth" "eightieth" "ninetieth") )

;; roman numerals (from dorai@cs.rice.edu).

(define format:roman-alist
  '((1000 #\M) (500 #\D) (100 #\C) (50 #\L) (10 #\X) (5 #\V) (1 #\I)))

(define format:roman-boundary-values
  '(100 100 10 10 1 1 #f))

;; globals

(define format:port #f)         ; curr. format output port
(define format:output-col 0)    ; curr. format output tty column
(define format:flush-output #f) ; flush output at end of formatting
(define format:case-conversion #f)
(define format:error-continuation #f)
(define format:args #f)
(define format:pos 0)           ; curr. format string parsing position
(define format:arg-pos 0)       ; curr. format argument position
                                ; this is global for error presentation
(define format:read-proof #f)   ; resulting string is additionally set into string quotes

;;

(define (format:list-head l k)
  (if (fx= k 0)
		'()
		(cons (car l) (format:list-head (cdr l) (- k 1)))))

;; Aborts the program when a formatting error occures. This is a null
;; argument closure to jump to the interpreters toplevel continuation.

(define (format:abort) (##sys#error "error in format"))

;; error handler

(define (format:error . args)           ; never returns!
  (let ((error-continuation format:error-continuation)
        (format-args format:args)
        (port (current-error-port)))
    (set! format:error format:intern-error)
    (if (and (>= (length format:args) 2)
             (string? (cadr format:args)))
			(let ((format-string (cadr format-args)))
				(unless (zero? format:arg-pos)
					(set! format:arg-pos (- format:arg-pos 1)))
				(format port "~%FORMAT: error with call: (format ~a \"~a<===~a\" ~
																~{~a ~}===>~{~a ~})~%        "
					(car format:args)
					(substring format-string 0 format:pos)
					(substring format-string format:pos
						(string-length format-string))
					(format:list-head (cddr format:args) format:arg-pos)
					(list-tail (cddr format:args) format:arg-pos)))
			(format port
				"~%FORMAT: error with call: (format~{ ~a~})~%        "
				format:args))
    (apply format port args)
    (newline port)
    (set! format:error format:error-save)
    (set! format:error-continuation error-continuation)
    (format:abort)
    (format:intern-error "format:abort does not jump to toplevel!")))

(define format:error-save format:error)

(define (format:intern-error . args)   ;if something goes wrong in format:error
  (display "FORMAT: INTERNAL ERROR IN FORMAT:ERROR!") (newline)
  (display "        format args: ") (write format:args) (newline)
  (display "        error args:  ") (write args) (newline)
  (set! format:error format:error-save)
  (format:abort))

;; format string and char output routines on format:port

(define (format:out-str str)
	(if format:case-conversion
		(display (format:case-conversion str) format:port)
		(display str format:port))
	(set! format:output-col (fx+ format:output-col (string-length str))))

(define (format:out-char ch)
	(if format:case-conversion
		(display (format:case-conversion (string ch)) format:port)
		(write-char ch format:port))
	(set! format:output-col
		(if (char=? ch #\newline) 0 (fx+ format:output-col 1))))

(define (format:out-substr str i n)
	(do ((k i (fx+ k 1)))
			((fx= k n))
		(write-char (string-ref str k) format:port))
	(set! format:output-col (fx+ format:output-col n)))

(define (format:out-fill n ch)
	(do ((i 0 (fx+ i 1)))
			((fx= i n))
		(write-char ch format:port))
	(set! format:output-col (fx+ format:output-col n)))

;;

(define (format:par pars length index default name)
  (if (fx> length index)
		(let ((par (list-ref pars index)))
			(if par
				(if name
					(if (fx< par 0)
						(error name "parameter must be a positive integer")
						par)
					par)
				default))
		default))

(define (format:out-obj-padded pad-left obj slashify pars)
	(if (null? pars)
		(format:out-str (format:obj->str obj slashify))
		(let ((l (length pars)))
			(let ((mincol (format:par pars l 0 0 "mincol"))
						(colinc (format:par pars l 1 1 "colinc"))
						(minpad (format:par pars l 2 0 "minpad"))
						(padchar (integer->char
											(format:par pars l 3 format:space-ch #f)))
						(objstr (format:obj->str obj slashify)))
				(unless pad-left
					(format:out-str objstr))
				(do ((objstr-len (string-length objstr))
						 (i minpad (fx+ i colinc)))
						((fx>= (fx+ objstr-len i) mincol)
						 (format:out-fill i padchar)))
				(when pad-left
					(format:out-str objstr))))))

(define (format:out-num-padded modifier number pars radix)
	(unless (integer? number)
	  (set! number (inexact->exact (truncate number)))
	  #;(format:error "argument not an integer" number))
	(let ((numstr (number->string number radix)))
		(when (and format:radix-pref (not (fx= radix 10)))
			(set! numstr (substring numstr 2 (string-length numstr))))
		(if (and (null? pars) (not modifier))
			(format:out-str numstr)
			(let ((l (length pars))
						(numstr-len (string-length numstr)))
				(let ((mincol (format:par pars l 0 #f "mincol"))
							(padchar (integer->char
												(format:par pars l 1 format:space-ch #f)))
							(commachar (integer->char
													(format:par pars l 2 (char->integer #\,) #f)))
							(commawidth (format:par pars l 3 3 "commawidth")))
					(if mincol
						(let ((numlen numstr-len)) ; calc. the output len of number
							(when (and (memq modifier '(at colon-at)) (positive? number))
								(set! numlen (fx+ numlen 1)))
							(when (memq modifier '(colon colon-at))
								(set! numlen
									(fx+ numlen
										(fx/ (fx- numstr-len (if (negative? number) 2 1))
											commawidth))))
							(when (fx> mincol numlen)
								(format:out-fill (fx- mincol numlen) padchar))))
					(if (and (memq modifier '(at colon-at))
									 (positive? number))
							(format:out-char #\+))
					(if (memq modifier '(colon colon-at)) ; insert comma character
							(let ((start (fxmod numstr-len commawidth))
										(ns (if (negative? number) 1 0)))
								(format:out-substr numstr 0 start)
								(do ((i start (fx+ i commawidth)))
										((fx>= i numstr-len))
									(if (fx> i ns)
											(format:out-char commachar))
									(format:out-substr numstr i (fx+ i commawidth))))
							(format:out-str numstr)))))))

(define (format:tabulate modifier pars)
	(let ((l (length pars)))
		(let ((colnum (format:par pars l 0 1 "colnum"))
					(colinc (format:par pars l 1 1 "colinc"))
					(padch (integer->char (format:par pars l 2 format:space-ch #f))))
			(case modifier
				((colon colon-at)
					(format:error "unsupported modifier for ~~t" modifier))
				((at)                         ; relative tabulation
				(format:out-fill
					(if (fx= colinc 0)
						colnum                  ; colnum = colrel
						(do ((c 0 (fx+ c colinc))
								 (col (fx+ format:output-col colnum)))
								((fx>= c col)
								 (fx- c format:output-col))))
					padch))
				(else                         ; absolute tabulation
				(format:out-fill
					(cond
					((fx< format:output-col colnum)
						(fx- colnum format:output-col))
					((fx= colinc 0)
						0)
					(else
						(do ((c colnum (fx+ c colinc)))
								((fx>= c format:output-col)
								 (fx- c format:output-col)))))
					padch))))))

(define (format:num->old-roman n)
	(if (and (integer? n) (>= n 1))
		(let loop ((n n)
							 (romans format:roman-alist)
							 (s '()))
			(if (null? romans)
				(list->string (reverse s))
				(let ((roman-val (caar romans))
							(roman-dgt (cadar romans)))
					(do ((q (quotient n roman-val) (- q 1))
							 (s s (cons roman-dgt s)))
							((zero? q)
							 (loop (remainder n roman-val)
										 (cdr romans) s))))))
		(format:error "only positive integers can be romanized")))

(define (format:num->roman n)
	(if (and (integer? n) (positive? n))
		(let loop ((n n)
							 (romans format:roman-alist)
							 (boundaries format:roman-boundary-values)
							 (s '()))
			(if (null? romans)
				(list->string (reverse s))
				(let ((roman-val (caar romans))
							(roman-dgt (cadar romans))
							(bdry (car boundaries)))
					(let loop2 ((q (quotient n roman-val))
											(r (remainder n roman-val))
											(s s))
						(if (zero? q)
							(if (and bdry (>= r (- roman-val bdry)))
								(loop (remainder r bdry) (cdr romans)
											(cdr boundaries)
											(cons roman-dgt (append (cdr (assv bdry romans)) s)))
								(loop r (cdr romans) (cdr boundaries) s))
							(loop2 (- q 1) r (cons roman-dgt s)))))))
		(format:error "only positive integers can be romanized")))

(define (format:num->cardinal999 n)
		;;this procedure is inspired by the Bruno Haible's CLisp
		;;function format-small-cardinal, which converts numbers
		;;in the range 1 to 999, and is used for converting each
		;;thousand-block in a larger number
	(let* ((hundreds (quotient n 100))
				 (tens+ones (remainder n 100))
				 (tens (quotient tens+ones 10))
				 (ones (remainder tens+ones 10)))
		(append
			(if (positive? hundreds)
				(append
					(string->list (vector-ref format:cardinal-ones-list hundreds))
					(string->list" hundred")
					(if (> tens+ones 0) '(#\space) '()))
				'())
			(if (< tens+ones 20)
				(if (positive? tens+ones)
					(string->list (vector-ref format:cardinal-ones-list tens+ones))
					'())
				(append
					(string->list (vector-ref format:cardinal-tens-list tens))
					(if (positive? ones)
						(cons #\-
							(string->list (vector-ref format:cardinal-ones-list ones)))
						'()))))))

(define (format:num->cardinal n)
	(cond
		((not (integer? n))
			(format:error "only integers can be converted to English cardinals"))
		((zero? n) "zero")
		((negative? n) (string-append "minus " (format:num->cardinal (- n))))
		(else
			(let ((power3-word-limit (vector-length format:cardinal-thousand-block-list)))
				(let loop ((n n)
									 (power3 0)
									 (s '()))
					(if (zero? n)
						(list->string s)
						(let ((n-before-block (quotient n 1000))
									(n-after-block (remainder n 1000)))
							(loop n-before-block
								(fx+ power3 1)
								(if (positive? n-after-block)
									(append
										(if (positive? n-before-block)
											(string->list ", ")
											'())
										(format:num->cardinal999 n-after-block)
										(if (fx< power3 power3-word-limit)
											(string->list
												(vector-ref
													format:cardinal-thousand-block-list
													power3))
											(append
												(string->list " times ten to the ")
												(string->list (format:num->ordinal (fx* power3 3)))
												(string->list " power")))
										s)
									s)))))))))

(define (format:num->ordinal n)
	(cond
		((not (integer? n))
			(format:error
				"only integers can be converted to English ordinals"))
		((zero? n) "zeroth")
		((negative? n) (string-append "minus " (format:num->ordinal (- n))))
		(else
			(let ((hundreds (quotient n 100))
						(tens+ones (remainder n 100)))
				(string-append
					(if (positive? hundreds)
						(string-append
							(format:num->cardinal (* hundreds 100))
							(if (zero? tens+ones) "th" " "))
						"")
					(if (zero? tens+ones)
						""
						(if (< tens+ones 20)
							(vector-ref format:ordinal-ones-list tens+ones)
							(let ((tens (quotient tens+ones 10))
										(ones (remainder tens+ones 10)))
								(if (zero? ones)
									(vector-ref format:ordinal-tens-list tens)
									(string-append
										(vector-ref format:cardinal-tens-list tens)
										"-"
										(vector-ref format:ordinal-ones-list ones)))))))))))

;; format fixed flonums (~F)

(define (format:out-fixed modifier number pars)
	(unless (or (number? number) (string? number))
		(format:error "argument is not a number or a number string" number))
	(let ((l (length pars)))
		(let ((width (format:par pars l 0 #f "width"))
					(digits (format:par pars l 1 #f "digits"))
					(scale (format:par pars l 2 0 #f))
					(overch (format:par pars l 3 #f #f))
					(padch (format:par pars l 4 format:space-ch #f)))

			(if digits

				(begin                      ; fixed precision
					(format:parse-float
					(if (string? number) number (number->string number)) #t scale)
					(if (fx<= (fx- format:fn-len format:fn-dot) digits)
						(format:fn-zfill #f (fx- digits (fx- format:fn-len format:fn-dot)))
						(format:fn-round digits))
					(if width
						(let ((numlen (fx+ format:fn-len 1)))
							(when (or (not format:fn-pos?) (eq? modifier 'at))
								(set! numlen (fx+ numlen 1)))
							(when (and (fx= format:fn-dot 0) (fx> width (fx+ digits 1)))
								(set! numlen (fx+ numlen 1)))
							(when (fx< numlen width)
								(format:out-fill (fx- width numlen) (integer->char padch)))
							(if (and overch (fx> numlen width))
								(format:out-fill width (integer->char overch))
								(format:fn-out modifier (fx> width (fx+ digits 1)))))
						(format:fn-out modifier #t)))

				(begin                      ; free precision
					(format:parse-float
					(if (string? number) number (number->string number)) #t scale)
					(format:fn-strip)
					(if width
						(let ((numlen (fx+ format:fn-len 1)))
							(when (or (not format:fn-pos?) (eq? modifier 'at))
								(set! numlen (fx+ numlen 1)))
							(when (fx= format:fn-dot 0)
								(set! numlen (fx+ numlen 1)))
							(when (fx< numlen width)
								(format:out-fill (fx- width numlen) (integer->char padch)))
							(if (fx> numlen width) ; adjust precision if possible
								(let ((dot-index (fx- numlen
																		(fx- format:fn-len format:fn-dot))))
									(if (fx> dot-index width)
										(if overch ; numstr too big for required width
											(format:out-fill width (integer->char overch))
											(format:fn-out modifier #t))
										(begin
											(format:fn-round (fx- width dot-index))
											(format:fn-out modifier #t))))
								(format:fn-out modifier #t)))
						(format:fn-out modifier #t)))))))

;; format exponential flonums (~E)

(define (format:out-expon modifier number pars)
	(unless (or (number? number) (string? number))
		(format:error "argument is not a number" number))
	(let ((l (length pars)))
		(let ((width (format:par pars l 0 #f "width"))
					(digits (format:par pars l 1 #f "digits"))
					(edigits (format:par pars l 2 #f "exponent digits"))
					(scale (format:par pars l 3 1 #f))
					(overch (format:par pars l 4 #f #f))
					(padch (format:par pars l 5 format:space-ch #f))
					(expch (format:par pars l 6 #f #f)))

			(if digits                      ; fixed precision

				(let ((digits (if (fx> scale 0)
												(if (fx< scale (fx+ digits 2))
													(fx+ (fx- digits scale) 1)
													0)
												digits)))
					(format:parse-float
						(if (string? number) number (number->string number)) #f scale)
					(if (fx<= (fx- format:fn-len format:fn-dot) digits)
						(format:fn-zfill #f (fx- digits (fx- format:fn-len format:fn-dot)))
						(format:fn-round digits))
					(if width
						(if (and edigits overch (fx> format:en-len edigits))
							(format:out-fill width (integer->char overch))
							(let ((numlen (fx+ format:fn-len 3)) ; .E+
												(leading-0 #f))
								(when (or (not format:fn-pos?) (eq? modifier 'at))
									(set! numlen (fx+ numlen 1)))
								(when (and (fx= format:fn-dot 0) (fx> width (fx+ digits 1)))
									(begin (set! leading-0 #t) (set! numlen (fx+ numlen 1))))
								(set! numlen
									(fx+ numlen
										(if (and edigits (fx>= edigits format:en-len))
											edigits
											format:en-len)))
								(when (fx< numlen width)
									(format:out-fill (fx- width numlen) (integer->char padch)))
								(if (and overch (fx> numlen width) (not leading-0))
									(format:out-fill width (integer->char overch))
									(begin
										(format:fn-out modifier (fx> width (fx- numlen 1)))
										(format:en-out edigits expch)))))
						(begin
							(format:fn-out modifier #t)
							(format:en-out edigits expch))))

				(begin                      ; free precision
					(format:parse-float
					(if (string? number) number (number->string number)) #f scale)
					(format:fn-strip)
					(if width
						(if (and edigits overch (fx> format:en-len edigits))
							(format:out-fill width (integer->char overch))
							(let ((numlen (fx+ format:fn-len 3))) ; .E+
								(when (or (not format:fn-pos?) (eq? modifier 'at))
									(set! numlen (fx+ numlen 1)))
								(when (fx= format:fn-dot 0) ; leading 0
									(set! numlen (fx+ numlen 1)))
								(set! numlen
									(fx+ numlen
										(if (and edigits (fx>= edigits format:en-len))
											edigits
											format:en-len)))
								(when (fx< numlen width)
									(format:out-fill (fx- width numlen) (integer->char padch)))
								(if (fx> numlen width) ; adjust precision if possible
									(let ((f (fx- format:fn-len format:fn-dot))) ; fract len
										(if (fx> (fx- numlen f) width)
											(if overch ; numstr too big for required width
												(format:out-fill width (integer->char overch))
												(begin
													(format:fn-out modifier #t)
													(format:en-out edigits expch)))
											(begin
												(format:fn-round (fx+ (fx- f numlen) width))
												(format:fn-out modifier #t)
												(format:en-out edigits expch))))
									(begin
										(format:fn-out modifier #t)
										(format:en-out edigits expch)))))
						(begin
							(format:fn-out modifier #t)
							(format:en-out edigits expch))))))))

;; format general flonums (~G)

(define (format:out-general modifier number pars)
	(unless (or (number? number) (string? number))
		(format:error "argument is not a number or a number string" number))
	(let ((l (length pars)))
		(let ((width (if (fx> l 0) (list-ref pars 0) #f))
					(digits (if (fx> l 1) (list-ref pars 1) #f))
					(edigits (if (fx> l 2) (list-ref pars 2) #f))
					(overch (if (fx> l 4) (list-ref pars 4) #f))
					(padch (if (fx> l 5) (list-ref pars 5) #f)))
			(format:parse-float
			(if (string? number) number (number->string number)) #t 0)
			(format:fn-strip)
			(let* ((ee (if edigits (fx+ edigits 2) 4)) ; for the following algorithm
						 (ww (if width (fx- width ee) #f)) ; see Steele's CL book p.395
						 (n (if (fx= format:fn-dot 0) ; number less than (abs 1.0) ?
									(fxneg (format:fn-zlead))
									format:fn-dot))
						 (d (if digits
									digits
									(fxmax format:fn-len (fxmin n 7)))) ; q = format:fn-len
						 (dd (fx- d n)))
				(if (and (fx<= 0 dd) (fx<= dd d))
					(begin
						(format:out-fixed modifier number (list ww dd #f overch padch))
						(format:out-fill ee #\space)) ;~@T not implemented yet
					(format:out-expon modifier number pars))))))

;; format dollar flonums (~$)

(define (format:out-dollar modifier number pars)
	(unless (or (number? number) (string? number))
		(format:error "argument is not a number or a number string" number))
	(let ((l (length pars)))
		(let ((digits (format:par pars l 0 2 "digits"))
					(mindig (format:par pars l 1 1 "mindig"))
					(width (format:par pars l 2 0 "width"))
					(padch (format:par pars l 3 format:space-ch #f)))

			(format:parse-float
			(if (string? number) number (number->string number)) #t 0)
			(if (fx<= (fx- format:fn-len format:fn-dot) digits)
				(format:fn-zfill #f (fx- digits (fx- format:fn-len format:fn-dot)))
				(format:fn-round digits))
			(let ((numlen (fx+ format:fn-len 1)))
				(when (or (not format:fn-pos?) (memq modifier '(at colon-at)))
					(set! numlen (fx+ numlen 1)))
				(when (and mindig (fx> mindig format:fn-dot))
					(set! numlen (fx+ numlen (fx- mindig format:fn-dot))))
				(when (and (fx= format:fn-dot 0) (not mindig))
					(set! numlen (fx+ numlen 1)))
				(if (fx< numlen width)
					(case modifier
						((colon)
							(if (not format:fn-pos?)
									(format:out-char #\-))
							(format:out-fill (fx- width numlen) (integer->char padch)))
						((at)
							(format:out-fill (fx- width numlen) (integer->char padch))
							(format:out-char (if format:fn-pos? #\+ #\-)))
						((colon-at)
							(format:out-char (if format:fn-pos? #\+ #\-))
							(format:out-fill (fx- width numlen) (integer->char padch)))
						(else
							(format:out-fill (fx- width numlen) (integer->char padch))
							(unless format:fn-pos?
								(format:out-char #\-))))
					(if format:fn-pos?
						(if (memq modifier '(at colon-at)) (format:out-char #\+))
						(format:out-char #\-))))
			(when (and mindig (fx> mindig format:fn-dot))
				(format:out-fill (fx- mindig format:fn-dot) #\0))
			(when (and (fx= format:fn-dot 0) (not mindig))
				(format:out-char #\0))
			(format:out-substr format:fn-str 0 format:fn-dot)
			(format:out-char #\.)
			(format:out-substr format:fn-str format:fn-dot format:fn-len))))

;; the flonum buffers

(define format:fn-str (make-string format:fn-max)) ; number buffer
(define format:fn-len 0)                ; digit length of number
(define format:fn-dot #f)               ; dot position of number
(define format:fn-pos? #t)              ; number positive?
(define format:en-str (make-string format:en-max)) ; exponent buffer
(define format:en-len 0)                ; digit length of exponent
(define format:en-pos? #t)              ; exponent positive?

(define (format:parse-float num-str fixed? scale)
	(set! format:fn-pos? #t)
	(set! format:fn-len 0)
	(set! format:fn-dot #f)
	(set! format:en-pos? #t)
	(set! format:en-len 0)
	(do ((i 0 (fx+ i 1))
			 (left-zeros 0)
			 (mantissa? #t)
			 (all-zeros? #t)
			 (num-len (string-length num-str))
			 (c #f))                  ; current exam. character in num-str
			((fx= i num-len)
			 (unless format:fn-dot
				 (set! format:fn-dot format:fn-len))

			 (when all-zeros?
				 (set! left-zeros 0)
				 (set! format:fn-dot 0)
				 (set! format:fn-len 1))

			 ;; now format the parsed values according to format's need
			 (if fixed?

				 (begin                     ; fixed format m.nnn or .nnn
					 (when (and (fx> left-zeros 0) (fx> format:fn-dot 0))
						 (if (fx> format:fn-dot left-zeros)
							 (begin           ; norm 0{0}nn.mm to nn.mm
								 (format:fn-shiftleft left-zeros)
								 (set! left-zeros 0)
								 (set! format:fn-dot (fx- format:fn-dot left-zeros)))
							 (begin           ; normalize 0{0}.nnn to .nnn
								 (format:fn-shiftleft format:fn-dot)
								 (set! left-zeros (fx- left-zeros format:fn-dot))
								 (set! format:fn-dot 0))))
					 (when (or (not (fx= scale 0)) (fx> format:en-len 0))
						 (let ((shift (fx+ scale (format:en-int))))
							 (cond
								(all-zeros? #t)
								((fx> (fx+ format:fn-dot shift) format:fn-len)
								 (format:fn-zfill
									#f (fx- shift (fx- format:fn-len format:fn-dot)))
								 (set! format:fn-dot format:fn-len))
								((fx< (fx+ format:fn-dot shift) 0)
								 (format:fn-zfill #t (fx- (fxneg shift) format:fn-dot))
								 (set! format:fn-dot 0))
								(else
								 (if (fx> left-zeros 0)
									 (if (fx<= left-zeros shift) ; shift always > 0 here
										 (format:fn-shiftleft shift) ; shift out 0s
										 (begin
											 (format:fn-shiftleft (fx- left-zeros shift))
											 (set! format:fn-dot (fxmax 0 (fx- shift left-zeros)))))
									 (set! format:fn-dot (fx+ format:fn-dot shift))))))))

				 (let ((negexp              ; expon format m.nnnEee
								(if (fx> left-zeros 0)
									(fx+ (fx- left-zeros format:fn-dot) 1)
									(if (fx= format:fn-dot 0) 1 0))))
					 (if (fx> left-zeros 0)
						 (begin               ; normalize 0{0}.nnn to n.nn
							 (format:fn-shiftleft left-zeros)
							 (set! format:fn-dot 1))
						 (when (fx= format:fn-dot 0)
							 (set! format:fn-dot 1)))
					 (format:en-set (fx- (fx+ (fx- format:fn-dot scale) (format:en-int)) negexp))
					 (cond
						(all-zeros?
						 (format:en-set 0)
						 (set! format:fn-dot 1))
						((fx< scale 0)          ; leading zero
						 (format:fn-zfill #t (fxneg scale))
						 (set! format:fn-dot 0))
						((fx> scale format:fn-dot)
						 (format:fn-zfill #f (fx- scale format:fn-dot))
						 (set! format:fn-dot scale))
						(else
						 (set! format:fn-dot scale)))))
			 #t)

		;; do body
		(set! c (string-ref num-str i)) ; parse the output of number->string
		(cond                            ; which can be any valid number
		 ((char-numeric? c)              ; representation of R4RS except
			(if mantissa?                   ; complex numbers
				(begin
					(if (char=? c #\0)
						(when all-zeros?
							(set! left-zeros (fx+ left-zeros 1)))
						(set! all-zeros? #f))
					(string-set! format:fn-str format:fn-len c)
					(set! format:fn-len (fx+ format:fn-len 1)))
				(begin
					(string-set! format:en-str format:en-len c)
					(set! format:en-len (fx+ format:en-len 1)))))
		 ((or (char=? c #\-) (char=? c #\+))
			(if mantissa?
				(set! format:fn-pos? (char=? c #\+))
				(set! format:en-pos? (char=? c #\+))))
		 ((char=? c #\.)
			(set! format:fn-dot format:fn-len))
		 ((char=? c #\e)
			(set! mantissa? #f))
		 ((char=? c #\E)
			(set! mantissa? #f))
		 ((char-whitespace? c) #t)
		 ((char=? c #\d) #t)              ; decimal radix prefix
		 ((char=? c #\#) #t)
		 (else
			(format:error "illegal character in number->string" c)))))

(define (format:en-int)   ; convert exponent string to integer
	(if (fx= format:en-len 0)
		0
		(do ((i 0 (fx+ i 1))
				 (n 0))
				((fx= i format:en-len)
				 (if format:en-pos? n (fxneg n)))
			(set! n
				(fx+ (fx* n 10)
					(fx- (char->integer (string-ref format:en-str i))
						format:zero-ch))))))

(define (format:en-set en)              ; set exponent string number
	(set! format:en-len 0)
	(set! format:en-pos? (fx>= en 0))
	(let ((en-str (number->string en)))
		(do ((i 0 (fx+ i 1))
				 (en-len (string-length en-str))
				 (c #f))
				((fx= i en-len))
			(set! c (string-ref en-str i))
			(when (char-numeric? c)
				(string-set! format:en-str format:en-len c)
				(set! format:en-len (fx+ format:en-len 1))))))

(define (format:fn-zfill left? n) ; fill current number string with 0s
	(when (fx> (fx+ n format:fn-len) format:fn-max) ; from the left or right
		(format:error "number is too long to format (enlarge format:fn-max)"))
	(set! format:fn-len (fx+ format:fn-len n))
	(if left?
		(do ((i format:fn-len (fx- i 1)))       ; fill n 0s to left
				((fx< i 0))
			(string-set! format:fn-str i
				(if (fx< i n)
					#\0
					(string-ref format:fn-str (fx- i n)))))
		(do ((i (fx- format:fn-len n) (fx+ i 1))) ; fill n 0s to the right
				((fx= i format:fn-len))
			(string-set! format:fn-str i #\0))))

(define (format:fn-shiftleft n) ; shift left current number n positions
	(when (fx> n format:fn-len)
		(format:error "internal error in format:fn-shiftleft"
			(list n format:fn-len)))
	(do ((i n (fx+ i 1)))
			((fx= i format:fn-len)
			 (set! format:fn-len (fx- format:fn-len n)))
		(string-set! format:fn-str (fx- i n) (string-ref format:fn-str i))))

(define (format:fn-round digits)        ; round format:fn-str
	(set! digits (fx+ digits format:fn-dot))
	(do ((i digits (fx- i 1))           ; "099",2 -> "10"
			 (c 5))                         ; "023",2 -> "02"
			((or (fx= c 0) (fx< i 0))               ; "999",2 -> "100"
			 (if (fx= c 1)                  ; "005",2 -> "01"
				 (begin                    		; carry overflow
					 (set! format:fn-len digits)
					 (format:fn-zfill #t 1)   ; add a 1 before fn-str
					 (string-set! format:fn-str 0 #\1)
					 (set! format:fn-dot (fx+ format:fn-dot 1)))
				 (set! format:fn-len digits)))
		(set! c
			(fx+ c
				(fx- (char->integer (string-ref format:fn-str i)) format:zero-ch)))
		(string-set! format:fn-str i
			(integer->char
				(if (fx< c 10)
					(fx+ c format:zero-ch)
					(fx+ (fx- c 10) format:zero-ch))))
		(set! c (if (fx< c 10) 0 1))))

(define (format:fn-out modifier add-leading-zero?)
	(if format:fn-pos?
		(when (eq? modifier 'at)
			(format:out-char #\+))
		(format:out-char #\-))
	(if (fx= format:fn-dot 0)
		(when add-leading-zero?
			(format:out-char #\0))
		(format:out-substr format:fn-str 0 format:fn-dot))
	(format:out-char #\.)
	(format:out-substr format:fn-str format:fn-dot format:fn-len))

(define (format:en-out edigits expch)
	(format:out-char (if expch (integer->char expch) format:expch))
	(format:out-char (if format:en-pos? #\+ #\-))
	(when (and edigits (fx< format:en-len edigits))
		(format:out-fill (fx- edigits format:en-len) #\0))
	(format:out-substr format:en-str 0 format:en-len))

(define (format:fn-strip)               ; strip trailing zeros but one
	(string-set! format:fn-str format:fn-len #\0)
	(do ((i format:fn-len (fx- i 1)))
			((or (not (char=? (string-ref format:fn-str i) #\0))
					 (fx<= i format:fn-dot))
			 (set! format:fn-len (fx+ i 1)))))

(define (format:fn-zlead)               ; count leading zeros
	(do ((i 0 (fx+ i 1)))
			((or (fx= i format:fn-len)
					 (not (char=? (string-ref format:fn-str i) #\0)))
			 (if (fx= i format:fn-len)      ; found a real zero
				 0
				 i))))

;; format:char->str converts a character into a slashified string as
;; done by `write'. The procedure is dependent on the integer
;; representation of characters and assumes a character number
;; according to the ASCII character set.

(define (format:char->str ch)
  (let ((int-rep (char->integer ch)))
    (if (fx< int-rep 0)                 ; if chars are [-128...+127]
        (set! int-rep (fx+ int-rep 256)))
    (string-append
     "#\\"
     (cond
      ((char=? ch #\newline) "newline")
      ((and (fx>= int-rep 0) (fx<= int-rep 32))
       (vector-ref format:ascii-non-printable-charnames int-rep))
      ((fx= int-rep 127) "del")
      ((fx>= int-rep 128)                       ; octal representation
       (if format:radix-pref
           (let ((s (number->string int-rep 8)))
             (substring s 2 (string-length s)))
           (number->string int-rep 8)))
      (else (string ch))))))

;; format:iobj->str reveals the implementation dependent
;; representation of #<...> objects with the use of display and
;; call-with-output-string.

(define (format:iobj->str iobj format:read-proof)
  (if (or format:read-proof
          format:iobj-case-conv)
		(string-append
			(if format:read-proof "\"" "")
			(if format:iobj-case-conv
				(format:iobj-case-conv
					(call-with-output-string (lambda (p) (display iobj p))))
				(call-with-output-string (lambda (p) (display iobj p))))
			(if format:read-proof "\"" ""))
		(call-with-output-string (lambda (p) (display iobj p)))))

;; format:obj->str returns a R4RS representation as a string of an
;; arbitrary scheme object.
;;
;; First parameter is the object, second parameter is a boolean if
;; the representation should be slashified as `write' does.
;;
;; It uses format:char->str which converts a character into a
;; slashified string as `write' does and which is implementation
;; dependent.
;;
;; It uses format:iobj->str to print out internal objects as quoted
;; strings so that the output can always be processed by (read)

(define (format:obj->str obj slashify)
  (cond
   ((string? obj)
		(if slashify
			(let ((obj-len (string-length obj)))
				(string-append
				"\""
				(let loop ((i 0) (j 0))   ; taken from Marc Feeley's pp.scm
					(if (fx= j obj-len)
						(string-append (substring obj i j) "\"")
						(let ((c (string-ref obj j)))
							(if (or (char=? c #\\)
											(char=? c #\"))
								(string-append (substring obj i j) "\\"
																(loop j (fx+ j 1)))
								(loop i (fx+ j 1))))))))
			obj))

   ((boolean? obj) (if obj "#t" "#f"))

   ((number? obj) (number->string obj))

   ((symbol? obj)
		(if format:symbol-case-conv
			(format:symbol-case-conv (symbol->string obj))
			(symbol->string obj)))

   ((char? obj)
		(if slashify
			(format:char->str obj)
			(string obj)))

   ((null? obj) "()")

   ((input-port? obj)
    (format:iobj->str obj format:read-proof))

   ((output-port? obj)
    (format:iobj->str obj format:read-proof))

   ((list? obj)
		(string-append
			"("
			(let loop ((obj-list obj))
				(if (null? (cdr obj-list))
					(format:obj->str (car obj-list) #t)
					(string-append
						(format:obj->str (car obj-list) #t)
						" "
						(loop (cdr obj-list)))))
			")"))

		((pair? obj)
			(string-append
				"("
				(format:obj->str (car obj) #t)
				" . "
				(format:obj->str (cdr obj) #t)
				")"))

   ((vector? obj)
		(string-append "#" (format:obj->str (vector->list obj) #t)))

   (else                                ; only objects with an #<...>
    (format:iobj->str obj format:read-proof))))
                                        ; representation should fall in here

;;

(define (format:string-capitalize-first str) ; "hello" -> "Hello"
  (let ((cap-str (string-copy str))     ; "hELLO" -> "Hello"
        (non-first-alpha #f)            ; "*hello" -> "*Hello"
        (str-len (string-length str)))  ; "hello you" -> "Hello you"
    (do ((i 0 (fx+ i 1)))
        ((fx= i str-len) cap-str)
      (let ((c (string-ref str i)))
        (if (char-alphabetic? c)
					(if non-first-alpha
						(string-set! cap-str i (char-downcase c))
						(begin
							(set! non-first-alpha #t)
							(string-set! cap-str i (char-upcase c)))))))))
;;

(define (format:format-work format-string arglist) ; does the formatting work
	(letrec (
			(format-string-len (string-length format-string))
			(arg-pos 0)                    ; argument position in arglist
			(arg-len (length arglist))     ; number of arguments
			(modifier #f)									; 'colon | 'at | 'colon-at | #f
			(params '())                   ; directive parameter list
			(param-value-found #f)					; a directive parameter value found
			(conditional-nest 0)           ; conditional nesting level
			(clause-pos 0)									; last cond. clause beginning char pos
			(clause-default #f)						; conditional default clause string
			(clauses '())									; conditional clause string list
			(conditional-type #f)					; reflects the contional modifiers
			(conditional-arg #f)						; argument to apply the conditional
			(iteration-nest 0)             ; iteration nesting level
			(iteration-pos 0)							; iteration string beginning char pos
			(iteration-type #f)						; reflects the iteration modifiers
			(max-iterations #f)            ; maximum number of iterations
			(recursive-pos-save format:pos)

			(next-char											; gets the next char from format-string
				(lambda ()
					(let ((ch (peek-next-char)))
						(set! format:pos (fx+ 1 format:pos))
						ch)))

			(peek-next-char
				(lambda ()
					(if (fx>= format:pos format-string-len)
							(format:error "illegal format string")
							(string-ref format-string format:pos))))

			(one-positive-integer?
				(lambda (params)
					(cond
					((null? params) #f)
					((and (fixnum? (car params))
								(fx>= (car params) 0)
								(fx= (length params) 1)) #t)
					(else (format:error "one positive integer parameter expected")))))

			(next-arg
				(lambda ()
					(if (fx>= arg-pos arg-len)
							(begin
								(set! format:arg-pos (+ arg-len 1))
								(format:error "missing argument(s)")))
					(add-arg-pos 1)
					(list-ref arglist (fx- arg-pos 1))))

			(prev-arg
				(lambda ()
					(add-arg-pos -1)
					(if (fx< arg-pos 0)
							(format:error "missing backward argument(s)"))
					(list-ref arglist arg-pos)))

			(rest-args
				(lambda ()
					(let loop ((l arglist) (k arg-pos)) ; list-tail definition
						(if (fx= k 0) l (loop (cdr l) (fx- k 1))))))

			(add-arg-pos
				(lambda (n)
					(set! arg-pos (fx+ n arg-pos))
					(set! format:arg-pos arg-pos)))

			(anychar-dispatch              ; dispatches the format-string
				(lambda ()
					(if (fx>= format:pos format-string-len)
						arg-pos                 ; used for ~? continuance
						(let ((char (next-char)))
							(cond
							((char=? char #\~)
								(set! modifier #f)
								(set! params '())
								(set! param-value-found #f)
								(tilde-dispatch))
							(else
								(when (and (fx= 0 conditional-nest) (fx= 0 iteration-nest))
									(format:out-char char))
								(anychar-dispatch)))))))

			(tilde-dispatch
				(lambda ()
					(cond
					((fx>= format:pos format-string-len)
						(format:out-str "~")				; tilde at end of string is just output
						arg-pos)                  	; used for ~? continuance
					((and (or (fx= 0 conditional-nest)
										(memv (peek-next-char)
													format:conditional-directives-characters))
								(or (fx= 0 iteration-nest)
										(memv (peek-next-char)
													format:iteration-directives-characters)))
						(case (char-upcase (next-char))

							;; format directives

							((#\A)                  ; Any -- for humans
							(set! format:read-proof (memq modifier '(colon colon-at)))
							(format:out-obj-padded (memq modifier '(at colon-at))
																			(next-arg) #f params)
							(anychar-dispatch))
							((#\S)                  ; Slashified -- for parsers
							(set! format:read-proof (memq modifier '(colon colon-at)))
							(format:out-obj-padded (memq modifier '(at colon-at))
																			(next-arg) #t params)
							(anychar-dispatch))
							((#\D)                  ; Decimal
							(format:out-num-padded modifier (next-arg) params 10)
							(anychar-dispatch))
							((#\X)                  ; Hexadecimal
							(format:out-num-padded modifier (next-arg) params 16)
							(anychar-dispatch))
							((#\O)                  ; Octal
							(format:out-num-padded modifier (next-arg) params 8)
							(anychar-dispatch))
							((#\B)                  ; Binary
							(format:out-num-padded modifier (next-arg) params 2)
							(anychar-dispatch))
							((#\R)
							(if (null? params)
								(format:out-obj-padded ; Roman, cardinal, ordinal numerals
									#f
									((case modifier
										((at) format:num->roman)
										((colon-at) format:num->old-roman)
										((colon) format:num->ordinal)
										(else format:num->cardinal))
									(next-arg))
									#f params)
								(format:out-num-padded ; any Radix
									modifier (next-arg) (cdr params) (car params)))
							(anychar-dispatch))
							((#\F)                  ; Fixed-format floating-point
							(if format:floats
								(format:out-fixed modifier (next-arg) params)
								(format:out-str (number->string (next-arg))))
							(anychar-dispatch))
							((#\E)                  ; Exponential floating-point
							(if format:floats
								(format:out-expon modifier (next-arg) params)
								(format:out-str (number->string (next-arg))))
							(anychar-dispatch))
							((#\G)                  ; General floating-point
							(if format:floats
								(format:out-general modifier (next-arg) params)
								(format:out-str (number->string (next-arg))))
							(anychar-dispatch))
							((#\$)                  ; Dollars floating-point
							(if format:floats
								(format:out-dollar modifier (next-arg) params)
								(format:out-str (number->string (next-arg))))
							(anychar-dispatch))
							((#\I)                  ; Complex numbers
							(unless format:complex-numbers
								(format:error
									"complex numbers not supported by this scheme system"))
							(let ((z (next-arg)))
								(unless (complex? z)
									(format:error "argument not a complex number"))
								(format:out-fixed modifier (real-part z) params)
								(format:out-fixed 'at (imag-part z) params)
								(format:out-char #\i))
							(anychar-dispatch))
							((#\C)                  ; Character
							(let ((ch (if (one-positive-integer? params)
													(integer->char (car params))
													(next-arg))))
								(unless (char? ch)
									(format:error "~~c expects a character" ch))
								(case modifier
									((at)
										(format:out-str (format:char->str ch)))
									((colon)
										(let ((c (char->integer ch)))
											(when (fx< c 0)
												(set! c (fx+ c 256)))       	; compensate complement impl.
											(cond
											((fx< c #x20) ; assumes that control chars are < #x20
												(format:out-char #\^)
												(format:out-char (integer->char (fx+ c #x40))))
											((fx>= c #x7f)
												(format:out-str "#\\")
												(format:out-str
												(if format:radix-pref
													(let ((s (number->string c 8)))
														(substring s 2 (string-length s)))
													(number->string c 8))))
											(else
												(format:out-char ch)))))
									(else (format:out-char ch))))
							(anychar-dispatch))
							((#\P)                  ; Plural
							(when (memq modifier '(colon colon-at))
								(prev-arg))
							(let ((arg (next-arg)))
								(unless (number? arg)
									(format:error "~~p expects a number argument" arg))
								(if (fx= arg 1)
									(when (memq modifier '(at colon-at))
										(format:out-char #\y))
									(if (memq modifier '(at colon-at))
										(format:out-str "ies")
										(format:out-char #\s))))
							(anychar-dispatch))
							((#\~)                  ; Tilde
							(if (one-positive-integer? params)
								(format:out-fill (car params) #\~)
								(format:out-char #\~))
							(anychar-dispatch))
							((#\%)                  ; Newline
							(if (one-positive-integer? params)
								(format:out-fill (car params) #\newline)
								(format:out-char #\newline))
							(set! format:output-col 0)
							(anychar-dispatch))
							((#\&)                  ; Fresh line
							(if (one-positive-integer? params)
								(begin
									(when (fx> (car params) 0)
										(format:out-fill
											(fx- (car params) (if (fx> format:output-col 0) 0 1))
											#\newline))
									(set! format:output-col 0))
								(when (fx> format:output-col 0)
									(format:out-char #\newline)))
							(anychar-dispatch))
							((#\_)                  ; Space character
							(if (one-positive-integer? params)
								(format:out-fill (car params) #\space)
								(format:out-char #\space))
							(anychar-dispatch))
							((#\/)                  ; Tabulator character
							(if (one-positive-integer? params)
								(format:out-fill (car params) #\tab)
								(format:out-char #\tab))
							(anychar-dispatch))
							((#\|)                  ; Page seperator
							(if (one-positive-integer? params)
								(format:out-fill (car params) #\page)
								(format:out-char #\page))
							(set! format:output-col 0)
							(anychar-dispatch))
							((#\T)                  ; Tabulate
							(format:tabulate modifier params)
							(anychar-dispatch))
							((#\Y)                  ; Pretty-print
							(pretty-print (next-arg) format:port)
							(set! format:output-col 0)
							(anychar-dispatch))
							((#\? #\K)         ; Indirection (is "~K" in T-Scheme)
							(cond
								((memq modifier '(colon colon-at))
								(format:error "illegal modifier in ~~?" modifier))
								((eq? modifier 'at)
								(let* ((frmt (next-arg))
												(args (rest-args)))
									(add-arg-pos (format:format-work frmt args))))
								(else
								(let* ((frmt (next-arg))
												(args (next-arg)))
									(format:format-work frmt args))))
							(anychar-dispatch))
							((#\!)                  ; Flush output
							(set! format:flush-output #t)
							(anychar-dispatch))
							((#\newline)            ; Continuation lines
							(when (eq? modifier 'at)
								(format:out-char #\newline))
							(when (fx< format:pos format-string-len)
								(do ((ch (peek-next-char) (peek-next-char)))
										((or (not (char-whitespace? ch))
													(fx= format:pos (fx- format-string-len 1))))
									(if (eq? modifier 'colon)
										(format:out-char (next-char))
										(next-char))))
							(anychar-dispatch))
							((#\*)                  ; Argument jumping
							(case modifier
								((colon)             ; jump backwards
									(if (one-positive-integer? params)
										(do ((i 0 (fx+ i 1)))
												((fx= i (car params)))
											(prev-arg))
										(prev-arg)))
								((at)                ; jump absolute
									(set! arg-pos
										(if (one-positive-integer? params) (car params) 0)))
								((colon-at)
									(format:error "illegal modifier `:@' in ~~* directive"))
								(else                ; jump forward
									(if (one-positive-integer? params)
										(do ((i 0 (fx+ i 1)))
												((fx= i (car params)))
											(next-arg))
										(next-arg))))
							(anychar-dispatch))
							((#\()                  ; Case conversion begin
							(set! format:case-conversion
								(case modifier
									((at) format:string-capitalize-first)
									((colon) string-titlecase)
									((colon-at) string-upcase)
									(else string-downcase)))
							(anychar-dispatch))
							((#\))                  ; Case conversion end
							(unless format:case-conversion
								(format:error "missing ~~)"))
							(set! format:case-conversion #f)
							(anychar-dispatch))
							((#\[)                  ; Conditional begin
							(set! conditional-nest (fx+ conditional-nest 1))
							(cond
								((fx= conditional-nest 1)
								(set! clause-pos format:pos)
								(set! clause-default #f)
								(set! clauses '())
								(set! conditional-type
									(case modifier
										((at) 'if-then)
										((colon) 'if-else-then)
										((colon-at) (format:error "illegal modifier in ~~]"))
										(else 'num-case)))
								(set! conditional-arg
									(if (one-positive-integer? params)
										(car params)
										(next-arg)))))
							(anychar-dispatch))
							((#\;)                  ; Conditional separator
							(when (fx= 0 conditional-nest)
								(format:error "~~; not in ~~]~~[ conditional"))
							(unless (null? params)
								(format:error "no parameter allowed in ~~;"))
							(when (fx= conditional-nest 1)
								(let ((clause-str
												(cond
													((eq? modifier 'colon)
														(set! clause-default #t)
														(substring format-string clause-pos
															(fx- format:pos 3)))
													((memq modifier '(at colon-at))
														(format:error "illegal modifier in ~~;"))
													(else
														(substring format-string clause-pos
															(fx- format:pos 2))))))
									(set! clauses (append clauses (list clause-str)))
									(set! clause-pos format:pos)))
							(anychar-dispatch))
							((#\])                  ; Conditional end
							(when (fx= 0 conditional-nest)
								(format:error "missing ~~]"))
							(set! conditional-nest (fx- conditional-nest 1))
							(when modifier
								(format:error "no modifier allowed in ~~["))
							(unless (null? params)
								(format:error "no parameter allowed in ~~["))
							(cond
								((fx= 0 conditional-nest)
								(let ((clause-str (substring format-string clause-pos
																							(fx- format:pos 2))))
									(if clause-default
										(set! clause-default clause-str)
										(set! clauses (append clauses (list clause-str)))))
								(case conditional-type
									((if-then)
										(when conditional-arg
											(format:format-work (car clauses)
												(list conditional-arg))))
									((if-else-then)
										(add-arg-pos
										(format:format-work
											(if conditional-arg (cadr clauses) (car clauses))
											(rest-args))))
									((num-case)
										(when (or (not (integer? conditional-arg))
															(fx< conditional-arg 0))
											(format:error "argument not a positive integer"))
										(when (not (and (fx>= conditional-arg (length clauses))
																		(not clause-default)))
											(add-arg-pos
											(format:format-work
												(if (fx>= conditional-arg (length clauses))
													clause-default
													(list-ref clauses conditional-arg))
												(rest-args))))))))
							(anychar-dispatch))
							((#\{)                  ; Iteration begin
							(set! iteration-nest (fx+ iteration-nest 1))
							(cond
								((fx= iteration-nest 1)
								(set! iteration-pos format:pos)
								(set! iteration-type
									(case modifier
										((at) 'rest-args)
										((colon) 'sublists)
										((colon-at) 'rest-sublists)
										(else 'list)))
								(set! max-iterations
									(if (one-positive-integer? params) (car params) #f))))
							(anychar-dispatch))
							((#\})                  ; Iteration end
							(when (fx= 0 iteration-nest)
								(format:error "missing ~~{"))
							(set! iteration-nest (fx- iteration-nest 1))
							(case modifier
								((colon)
									(unless max-iterations (set! max-iterations 1)))
								((colon-at at) (format:error "illegal modifier" modifier))
								(else (unless max-iterations
												(set! max-iterations format:max-iterations))))
							(unless (null? params)
								(format:error "no parameters allowed in ~~}" params))
							(when (fx= 0 iteration-nest)
								(let ((iteration-str
												(substring format-string iteration-pos
																	(fx- format:pos (if modifier 3 2)))))
									(when (string=? iteration-str "")
										(set! iteration-str (next-arg)))
									(case iteration-type
										((list)
											(let ((args (next-arg))
														(args-len 0))
												(unless (list? args)
													(format:error "expected a list argument" args))
												(set! args-len (length args))
												(do ((arg-pos 0 (fx+ arg-pos
																					(format:format-work
																						iteration-str
																						(list-tail args arg-pos))))
														(i 0 (fx+ i 1)))
														((or (fx>= arg-pos args-len)
																(and format:iteration-bounded
																			(fx>= i max-iterations)))))))
										((sublists)
											(let ((args (next-arg))
														(args-len 0))
												(unless (list? args)
													(format:error "expected a list argument" args))
												(set! args-len (length args))
												(do ((arg-pos 0 (fx+ arg-pos 1)))
														((or (fx>= arg-pos args-len)
																(and format:iteration-bounded
																			(fx>= arg-pos max-iterations))))
													(let ((sublist (list-ref args arg-pos)))
														(unless (list? sublist)
															(format:error
																"expected a list of lists argument" args))
														(format:format-work iteration-str sublist)))))
										((rest-args)
											(let* ((args (rest-args))
														(args-len (length args))
														(usedup-args
															(do ((arg-pos 0 (fx+ arg-pos
																								(format:format-work
																									iteration-str
																									(list-tail
																									args arg-pos))))
																	(i 0 (fx+ i 1)))
																	((or (fx>= arg-pos args-len)
																			(and format:iteration-bounded
																						(fx>= i max-iterations)))
																	arg-pos))))
												(add-arg-pos usedup-args)))
										((rest-sublists)
											(let* ((args (rest-args))
														(args-len (length args))
														(usedup-args
															(do ((arg-pos 0 (fx+ arg-pos 1)))
																	((or (fx>= arg-pos args-len)
																			(and format:iteration-bounded
																						(fx>= arg-pos max-iterations)))
																	arg-pos)
																(let ((sublist (list-ref args arg-pos)))
																	(if (not (list? sublist))
																			(format:error "expected list arguments" args))
																	(format:format-work iteration-str sublist)))))
												(add-arg-pos usedup-args)))
										(else (format:error "internal error in ~~}")))))
							(anychar-dispatch))
							((#\^)                  ; Up and out
							(let* ((continue
											(cond
												((not (null? params))
												(not
													(case (length params)
														((1) (fx= 0 (car params)))
														((2) (fx= (list-ref params 0) (list-ref params 1)))
														((3) (and (fx<= (list-ref params 0) (list-ref params 1))
																			(fx<= (list-ref params 0) (list-ref params 2))))
														(else (format:error "too many parameters")))))
												(format:case-conversion ; if conversion stop conversion
												(set! format:case-conversion string-copy) #t)
												((fx= iteration-nest 1) #t)
												((fx= conditional-nest 1) #t)
												((fx>= arg-pos arg-len)
												(set! format:pos format-string-len) #f)
												(else #t))))
								(when continue
									(anychar-dispatch))))

							;; format directive modifiers and parameters

							((#\@)                  ; `@' modifier
							(when (memq modifier '(at colon-at))
								(format:error "double `@' modifier"))
							(set! modifier (if (eq? modifier 'colon) 'colon-at 'at))
							(tilde-dispatch))
							((#\:)                  ; `:' modifier
							(when (memq modifier '(colon colon-at))
								(format:error "double `:' modifier"))
							(set! modifier (if (eq? modifier 'at) 'colon-at 'colon))
							(tilde-dispatch))
							((#\')                  ; Character parameter
							(when modifier
								(format:error "misplaced modifier" modifier))
							(set! params (append params (list (char->integer (next-char)))))
							(set! param-value-found #t)
							(tilde-dispatch))
							((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+) ; num. paramtr
							(when modifier
								(format:error "misplaced modifier" modifier))
							(let ((num-str-beg (fx- format:pos 1))
										(num-str-end format:pos))
								(do ((ch (peek-next-char) (peek-next-char)))
										((not (char-numeric? ch)))
									(next-char)
									(set! num-str-end (fx+ 1 num-str-end)))
								(set! params
									(append params
										(list
											(string->number
												(substring format-string num-str-beg num-str-end))))))
							(set! param-value-found #t)
							(tilde-dispatch))
							((#\V)           ; Variable parameter from next argum.
							(when modifier
								(format:error "misplaced modifier" modifier))
							(set! params (append params (list (next-arg))))
							(set! param-value-found #t)
							(tilde-dispatch))
							((#\#)         ; Parameter is number of remaining args
							(when modifier
								(format:error "misplaced modifier" modifier))
							(set! params (append params (list (length (rest-args)))))
							(set! param-value-found #t)
							(tilde-dispatch))
							((#\,)                  ; Parameter separators
							(when modifier
								(format:error "misplaced modifier" modifier))
							(unless param-value-found
								(set! params (append params '(#f)))) ; append empty paramtr
							(set! param-value-found #f)
							(tilde-dispatch))
							((#\Q)                  ; Inquiry messages
							(if (eq? modifier 'colon)
								(format:out-str format:version)
								(let ((nl (string #\newline)))
									(format:out-str
										(string-append
										"SLIB Common LISP format version " format:version nl
										"  This code is in the public domain." nl
										"  Please send bug reports to `lutzeb@cs.tu-berlin.de'"
										nl))))
							(anychar-dispatch))
							(else                   ; Unknown tilde directive
							(format:error "unknown control character"
								(string-ref format-string (fx- format:pos 1))))))
					(else (anychar-dispatch)))))) ; in case of conditional

		(set! format:pos 0)
		(set! format:arg-pos 0)
		(anychar-dispatch)                ; start the formatting
		(set! format:pos recursive-pos-save)
		arg-pos))

;; the output handler for a port

(define (format:out fmt args)
    (set! format:case-conversion #f)    ; modifier case conversion procedure
    (set! format:flush-output #f)       ; ~! reset
    (let ((arg-pos (format:format-work fmt args))
          (arg-len (length args)))
      (cond
        ((fx< arg-pos arg-len)
          (set! format:arg-pos (+ arg-pos 1))
          (set! format:pos (string-length fmt))
          (if format:unprocessed-arguments-error?
            (format:error "superfluous arguments" (fx- arg-len arg-pos))))
        ((fx> arg-pos arg-len)
          (set! format:arg-pos (+ arg-len 1))
          (format:error "missing arguments" (fx- arg-pos arg-len))))))

;; We should keep separate track of columns for each port, but
;; keeping pointers to ports will foil GC.  Instead, keep
;; associations indexed by the string representation of the ports.

(define *port-columns* '())

(define-inline (format:port-name port)
	(->string port) )

(define (format:get-port-column port)
  (let ([pair (assoc (format:port-name port) *port-columns*)])
    (if pair (cdr pair) 0)))

(define (format:set-port-column! port col)
  (let* ([pname (format:port-name port)]
         [pair (assoc pname *port-columns*)])
    (if pair
			(set-cdr! pair col)
			(set! *port-columns* (cons (cons pname col) *port-columns*)))))

;;; Format entry-point

;@
(define (format . args)
  (set! format:args args)
  (set! format:arg-pos 0)
  (set! format:pos 0)
  (set! format:read-proof #f)
  (if (fx< (length args) 1)
    (format:error "not enough arguments"))

  ;; If the first argument is a string, then that's the format string.
  ;; (Scheme->C)
  ;; In this case, put the argument list in canonical form.
  (let ((args (if (string? (car args)) (cons #f args) args)))
    ;; Use this canonicalized version when reporting errors.
    (set! format:args args)
    (let ((destination (car args))
          (arglist (cdr args)))
      (cond
       ((or (and (boolean? destination) ; port output
                 destination)
            (output-port? destination)
            (number? destination))
        (let ((port (cond ((boolean? destination) (current-output-port))
                          ((output-port? destination) destination)
                          ((number? destination) (current-error-port)))))
          (set! format:port port)    ; global port for output routines
          (set! format:output-col (format:get-port-column port))
          (format:out (car arglist) (cdr arglist))
          (format:set-port-column! port format:output-col)
          (if format:flush-output (flush-output port))
          #t))
       ((and (boolean? destination)     ; string output
             (not destination))
        (call-with-output-string
            (lambda (port)
              (set! format:port port)
              (set! format:output-col 0)
              (format:out (car arglist) (cdr arglist)))))
       (else
        (format:error "illegal destination" destination))))))

)
