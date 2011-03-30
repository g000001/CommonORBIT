;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: (CL-USER :use (corbit lisp)) -*-

;;; some tests for CORBIT

(PROVIDE "CORBIT-DEMO")
(IN-PACKAGE "CL-USER")

(REQUIRE "CORBIT")
(USE-PACKAGE "CORBIT")

;;; helper function

(DEFMACRO CURRENT-YEAR ()
  `(MULTIPLE-VALUE-BIND (SECOND MINUTE HOUR DATE MONTH YEAR)
       (GET-DECODED-TIME)
     (DECLARE (IGNORE SECOND MINUTE HOUR DATE MONTH))
     YEAR))

;;; some prototype objects

(DEFRAME MAN
  "A male person."
  PERSON
  (SEX :VALUE 'MALE))

(DEFFRAME WOMAN
  "A female person."
  PERSON
  (SEX 'FEMALE))			;type defaults to :VALUE

(DEFOBJECT NAME
  (SPELLING :IF-NEEDED #'OBJECT-STRING))	;a function specification

(DEFOBJECT PERSON
  (FIRST-NAME
    ;; object as filler will cause structured delegation
    :OBJECT (A NAME))
  (PREFIX
    :VALUE NIL)
  (LAST-NAME
    (A NAME))				;type defaults to :OBJECT
  (NAME
    :IF-NEEDED (SELF)
    ;; another kind of function specification: args and body
    (CONCATENATE 'STRING
		 (STRING-CAPITALIZE (SPELLING (FIRST-NAME SELF)))
		 " "
		 (LET ((PREFIX (PREFIX SELF)))
		   (IF PREFIX
		       (CONCATENATE 'STRING
				    (STRING-DOWNCASE (PREFIX SELF))
				    " ")))
		 (STRING-CAPITALIZE (SPELLING (LAST-NAME SELF)))))
  (BIRTHYEAR :ASK)
  (AGE
    ;; an aspect of type :function will be recomputed every time
    :FUNCTION (SELF &OPTIONAL (YEAR (CURRENT-YEAR)))
    (- YEAR (BIRTHYEAR SELF)))
  (RESIDENCE
    (A CITY))
  (CHILD
    (A PERSON
       (LAST-NAME :IF-NEEDED (SELF)
		  (LAST-NAME (PARENT SELF)))
       (PREFIX :IF-NEEDED (SELF)
	       (PREFIX (PARENT SELF)))))
  (PARENT
    :FUNCTION				;yet another function specification
    #'(LAMBDA (SELF)
	(WHERE SELF = CHILD)))
  )

;;; some examples
 
(DEFOBJECT KOEN
  MAN
  (FIRST-NAME :OBJECT 'KOENRAAD)
  (PREFIX 'DE)
  (LAST-NAME :OBJECT 'SMEDT)
  (FRIEND-S				;implicit plural aspect
    '(JAN PIET))
  (COLLEAGUE				;explicit plural aspect
    :OBJECTS '(KAREL ANNA))
  ((CHILD FIRST-NAME)
   ;; demonstrate structured delegation: the object specified here will
   ;; delegate to the object which is FIRST-NAME of CHILD of PERSON
   ;; because KOEN is a PERSON;
   ;; this works well because first the referent of CHILD for KOEN is
   ;; computed and then the FIRST-NAME aspect is defined
   :OBJECT 'MATTHIJS))

(DEFOBJECT PIET MAN)
(DEFOBJECT JAN MAN)

(DEFASPECT RESIDENCE 'KOEN :OBJECT 'LEIDEN)

(DEFOBJECT MARLEEN
  WOMAN M.D.
  (FIRST-NAME
    :OBJECT 'MARLEEN)
  (PREFIX
    'DE)
  (LAST-NAME
    :IF-NEEDED (SELF)
    (DECLARE (IGNORE SELF))
    (LAST-NAME 'KOEN))
  (CHILD
   ;; again, structured delegation
    (A WOMAN (FIRST-NAME :OBJECT 'MIEKE)))
  (BIRTHYEAR 1955))

(DEFASPECT RESIDENCE 'MARLEEN
  :OBJECT 'LUXEMBOURG)

(DEFOBJECT DOCTOR
  PERSON
  (NAME					;delegation of one aspect
    :IF-NEEDED (SELF)
    (CONCATENATE 'STRING "Dr. " (DELEGATE (NAME 'PERSON) SELF))))

(DEFOBJECT M.D.
  PERSON
  (NAME					;another way to delegate one aspect
    :DELEGATE 'DOCTOR))

;;; some possible tests:

(DEFVAR *TESTS*
	'((AGE 'MARLEEN)
	  (AGE 'KOEN)
	  (AGE 'KOEN 2000)
	  (NAME 'KOEN)
	  (NAME 'MARLEEN)
	  (NAME (CHILD 'KOEN))
	  (NAME (CHILD 'MARLEEN))
	  (>> 'KOEN CHILD NAME)
	  (PATH 'KOEN CHILD)
	  (PATH 'KOEN CHILD CAT)
	  (SEX 'MARLEEN)
	  (WHOSE 'FRIEND 'JAN)
	  (WHERE 'JAN = FRIEND)
	  (<< 'JAN = FRIEND)
	  (WHOSE 'COLLEAGUE 'KAREL)
	  (MAPCAR 'SEX (FRIEND-S 'KOEN))
	  (RESIDENCE 'KOEN)
	  (SHOW 'KOEN)
	  (>> 'KOEN CHILD SHOW)
	  (SHOW 'MARLEEN)))

(DEFUN TEST-CORBIT (&OPTIONAL (TESTS *TESTS*))
  "Print test, then print result"
  (DOLIST (TEST TESTS)
    (PRINT TEST)
    (PRINC "=>")
    (PRINT (EVAL TEST))))
