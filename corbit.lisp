;;; -*-Mode: Lisp; Syntax: Common-lisp; Package: (CORBIT :use (CL))-*-

;;; Copyright (c) 1992,1991,1990,1989,1988 Koenraad De Smedt

;;;   Koenraad De Smedt
;;;   Unit of Experimental and Theoretical Psychology 
;;;   Leiden University
;;;   P.O. Box 9555
;;;   2300 RB  Leiden
;;;   The Netherlands
;;;   E-mail: desmedt@rulfsw.leidenuniv.nl

;;; CommonORBIT (abbreviated as CORBIT) is an applicative
;;; object-oriented programming system implemented as an extension of
;;; Common LISP.  It is based on a prototype by Luc Steels made in
;;; 1981-1983 which was originally called ORBIT.  Both systems use
;;; generic functions and delegation (classless inheritance).

;;; CommonORBIT is easy to use and allows a high degree of flexibility
;;; and reusability in programming.  It mixes well with ordinary Lisp.
;;; Several powerful constructs are provided, for example structured
;;; inheritance as in KL-ONE.

;;; CommonORBIT is distributed in the hope that it will be useful, but
;;; without any warranty.  No author or distributor accepts
;;; responsibility to anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless
;;; he says so in writing.

;;; Copyright Release Statement:

;;; Everyone is granted permission to copy, modify and redistribute
;;; CommonORBIT, but only under the conditions that (1) distribution
;;; is free and without cost, (2) any modifications are also sent to
;;; the above address, and (3) this entire notice is preserved on all
;;; copies.

;;; The current version is expected to run on systems conforming to
;;; CLtL 2 and additonal X3J13 actions until the end of 1991.  It has
;;; been tested in Allegro CL 4.1.

;;; This module contains the kernel, which may be everything you need.

(PROVIDE "CORBIT")

(defpackage "CORBIT"
  (:use "COMMON-LISP")
  (:export
   "<<"
   ">>"
   "A"
   "ADD-TO-ASPECT"
   "ALL-ASPECT-NAMES"
   "ALL-CLIENTS"
   "ALL-NAMED-CLIENTS"
   "ALL-NAMED-PROXIES"
   "ALL-PROXIES"
   "ALL-SORTED-PROXIES"
   "AN"
   "AND-DEFINED"
   "ANONYMOUSP"
   "CLIENT-OF-MEMBER"
   "CLIENTP"
   "COMPILE-ASPECTS"
   "COPY-NAMED-OBJECT"
   "COPY-OBJECT"
   "DEFASPECT"
   "DEFFRAME"
   "DEFINEDP"
   "DEFOBJECT"
   "DEFRAME"
   "DEFUN-GLOBAL"
   "DELEGATE"
   "DELETE-FROM-ASPECT"
   "DETECT-INFINITE-LOOPS"
   "DETECTING-INFINITE-LOOPS"
   "FIRST-IF-NEEDED"
   "HAS-ROLE-P"
   "IF-DEFINED"
   "INHERIT"
   "INSTANTIATE-OBJECTS"
   "INTENSION"
   "IS-CLIENT"
   "IS-LIKE"
   "IS-LIKE-P"
   "IS-LIKE?"
   "IS-NO-CLIENT"
   "IS-NO-PROXY"
   "IS-PROXY"
   "ISA"
   "ISA?"
   "ISAP"
   "KILL-OBJECT"
   "LET-DEFINED"
   "LIST-DEFINED"
   "LIST-IF-NEEDED"
   "MAKE-ANONYMOUS-CLIENT"
   "MAKE-NAMED-REFERENCE"
   "MAKE-OBJECT"
   "MAKE-REFERENCE"
   "MEMOIZE-ASPECTS"
   "NAME-OR-OBJECT"
   "NAMED-CLIENTS"
   "NAMED-OBJECT"
   "NAMED-OBJECT-NAME"
   "NAMED-OBJECT-P"
   "NOT-DEFINED"
   "OBJECT"
   "OBJECT"
   "OBJECT-CLIENTS"
   "OBJECT-DOCUMENTATION"
   "OBJECT-FBOUNDP"
   "OBJECT-NAME"
   "OBJECT-PROXIES"
   "OBJECT-REFERENT"
   "OBJECT-STRING"
   "OBJECTP"
   "OEQL"
   "ONLY-NAME"
   "OQL"
   "OR-DEFINED"
   "PATH"
   "POP-ASPECT"
   "PRESENT-OBJECT"
   "PROXY-OF-MEMBER"
   "PROXYP"
   "PRUNE-PROXIES"
   "PUSH-ASPECT"
   "PUSHNEW-ASPECT"
   "REINITIALIZE-OBJECTS"
   "ROLE-NAMES"
   "SELECT-ALL-CLIENTS"
   "SETASPECT"
   "SHOW"
   "SHOW-SWITCHES"
   "THE-NAMED-OBJECT"
   "THE-OBJECT"
   "UNDEFASPECT"
   "UNDEFINED"
   "UNDEFINEDP"
   "UPDATE-MEMOS"
   "WHERE"
   "WHOSE"
   "WITH"
   ))

(IN-PACKAGE "CORBIT")

;;; ----- Constants and variables -----

;;; constants
(DEFCONSTANT *VERSION* "CLtL 2, 30 September 1992")

;;; parameters
(DEFPARAMETER *SWITCHES* NIL "A list of all CommonORBIT switches.")
(DEFPARAMETER UNDEFINED 'UNDEFINED
  "The value returned from an object-oriented function call
   when the aspect is not defined for the object.")
(DEFPARAMETER *ASK-FORMAT-STRING* "~&What is the ~A of ~A? "
  "Default format string used by the :ASK aspect type.")

;;; switches (will be initialized later)
(DEFVAR *MEMOIZE-ASPECTS* NIL
  "If true, store values retrieved by delegation.")
(DEFVAR *UPDATE-MEMOS* NIL
  "If true, update memoized information after change.")
(DEFVAR *PRUNE-PROXIES* NIL
  "If true, prune the delegation hierarchy whenever appropriate.")
(DEFVAR *DETECT-INFINITE-LOOPS* NIL
  "If true, keep a stack to detect infinite loops.")
(DEFVAR *COMPILE-ASPECTS* NIL
  "If true, compile lambdas in aspects instead of making closures.")
(DEFVAR *REINITIALIZE-OBJECTS* NIL
  "Reinitialize an object when evaluating a DEFOBJECT form.")
(DEFVAR *ONLY-NAME* NIL
  "Print a named object by just printing its name.")
(DEFVAR *ASPECT-COMPILER* NIL
  "Either a function which compiles or one which evaluates an aspect
   definition, depending on the value of the variable *COMPILE-ASPECTS*.")
(DEFVAR *LAMBDA-COMPILER* NIL
  "Either a function which compiles or one which evaluates a lambda
   expression, depending on the value of the variable *COMPILE-ASPECTS*.")

;;; other variables
(DEFVAR *STACK* NIL "The CommonORBIT evaluation stack.
   This stack is used to detect infinite loops.")

;;; ----- Print herald -----

(WHEN *LOAD-VERBOSE*
  (FORMAT T "~&CommonORBIT  ~A  running in ~A."
	  *VERSION* (LISP-IMPLEMENTATION-TYPE)))

;;; ----- Plural aspect names -----

(DEFUN PLURALP (WORD)
  "True if the word ends on -S and is at least 3 characters long.
   The argument must be coercible to a string."
  (LET* ((STRING (STRING WORD))
	 (LENGTH (LENGTH STRING)))
    (AND (> LENGTH 2)
	 (CHAR= (CHAR STRING (- LENGTH 2))
		#\-)
	 (CHAR-EQUAL (CHAR STRING (1- LENGTH))
		     #\S))))

(DEFUN SINGULAR (WORD)
  "If the word is PLURALP, drop its last two characters, otherwise return
   the word unchanged.
   The argument must be coercible to a string. The result is a symbol."
  (IF (PLURALP WORD)
      (LET
	((STRING (STRING WORD)))
	(INTERN (SUBSEQ STRING 0 (- (LENGTH STRING) 2))
		(SYMBOL-PACKAGE WORD)))
      WORD))

(DEFUN PLURALIZE (WORD)
  "Add -S to the word if not already PLURALP.
   The argument must be coercible to a string. The result is a symbol."
  (IF (PLURALP WORD)
      WORD
      (INTERN (CONCATENATE 'STRING (STRING WORD) "-S")
	      (SYMBOL-PACKAGE WORD))))

;;; ----- Undefined -----

(DEFUN UNDEFINEDP (THING)
  "True if the argument is undefined, i.e. EQ to UNDEFINED."
  (EQ THING UNDEFINED))

(DEFUN DEFINEDP (THING)
  "If undefined then NIL, else the argument itself."
  (IF (UNDEFINEDP THING)
      NIL
      THING))

(DEFUN LIST-IF-NEEDED (THING)
  "NIL if the argument is UNDEFINED, the argument itself if it is a list,
   else a list containing the argument."
  (COND ((UNDEFINEDP THING) NIL)
	((LISTP THING) THING)
	(T (LIST THING))))

(DEFUN FIRST-IF-NEEDED (THING)
  "Take first element of argument if a list of one element, otherwise the
   argument itself."
  (IF (AND (CONSP THING)
	   (NULL (REST THING)))
      (FIRST THING)
      THING))

;;; ----- Basic structures and access -----

;;; An object is a two-level structure. The first level is a structure
;;; (a reference) which contains a pointer to the second level. 
;;; In addition, the first level of a named object contains the name.
;;; (reference by name :-)
;;; The second level is a structure which is called the referent of the
;;; object. Most information about an object (everything except the
;;; name) is stored in its referent.

;;; The difference between an object and its referent allows two
;;; objects to be 'coreferential', i.e. they can have different names
;;; but refer to the same thing.

(DEFSTRUCT (REFERENT
	     (:TYPE VECTOR))
  "CommonORBIT internal referent. Each object has a referent. The referent
   contains all information (except the object name of a named object).
   This structure is not named."
  (PROXIES NIL :TYPE LIST)
  (CLIENTS NIL :TYPE LIST)
  (ROLES NIL :TYPE LIST)
  (ASPECTS NIL :TYPE LIST)
  (DEPENDENCIES NIL :TYPE LIST)
  (MEMOS NIL :TYPE LIST)
  (DOCUMENTATION "" :TYPE STRING)
  )

;;; The basic object

(DEFSTRUCT (OBJECT
	     (:PREDICATE OBJECTP)
	     (:PRINT-FUNCTION PRESENT-ANONYMOUS-OBJECT))
  "CommonORBIT object. Each object has a referent. The referent is an
   internal structure which contains all information (except the name
   of a named object)."
  (REFERENT (MAKE-REFERENT)))

;;; Make indirections from objects to their referents

(DEFMACRO OBJECT-PROXIES (OBJECT)
  "The proxies of an object (a list)."
  `(REFERENT-PROXIES (OBJECT-REFERENT ,OBJECT)))

(DEFMACRO OBJECT-CLIENTS (OBJECT)
  "The clients of an object (a list)."
  `(REFERENT-CLIENTS (OBJECT-REFERENT ,OBJECT)))

(DEFMACRO OBJECT-ROLES (OBJECT)
  "The roles of an object (an assoc list)."
  `(REFERENT-ROLES (OBJECT-REFERENT ,OBJECT)))

(DEFMACRO OBJECT-ASPECTS (OBJECT)
  "The aspects of an object (an assoc list)."
  `(REFERENT-ASPECTS (OBJECT-REFERENT ,OBJECT)))

(DEFMACRO OBJECT-DEPENDENCIES (OBJECT)
  "The dependencies of an object (an assoc list)."
  `(REFERENT-DEPENDENCIES (OBJECT-REFERENT ,OBJECT)))

(DEFMACRO OBJECT-MEMOS (OBJECT)
  "The memos (memoized aspects) of an object (a list)."
  `(REFERENT-MEMOS (OBJECT-REFERENT ,OBJECT)))

(DEFMACRO OBJECT-DOCUMENTATION (OBJECT)
  "The documentation string of an object."
  `(REFERENT-DOCUMENTATION (OBJECT-REFERENT ,OBJECT)))

;;; We have to define equality of objects now

(DEFUN OQL (OBJECT1 OBJECT2)
  "Conceptual equality of CommonORBIT objects: two objects are considered
   equal if their referents are EQ. This is because different objects may
   have the same referent."
  (EQ (OBJECT-REFERENT OBJECT1)
      (OBJECT-REFERENT OBJECT2)))

;;; Make an additional function to use instead of EQL.
;;; In principle, EQL could be changed, but this may have
;;; some implementation-dependent dangerous consequences.

(DEFUN OEQL (OBJECT1 OBJECT2)
  "This is an extension of EQL which also works for conceptual equality
   of CommonORBIT objects. For objects, this predicate is the same as OQL.
   If the arguments are no CommonORBIT objects, EQL is used."
  (OR (EQL OBJECT1 OBJECT2)
      (AND (OBJECTP OBJECT1)
	   (OBJECTP OBJECT2)
	   (OQL OBJECT1 OBJECT2))))

;;; ----- Access to internal components of objects -----

;;; PROXIES and CLIENTS are lists of objects

;;; ROLES of an object are a list.
;;; Each role consists of a role-name and role-objects

(DEFMACRO FIND-ROLE (OBJECT ROLE-NAME)
  "Find role in an object."
  `(ASSOC ,ROLE-NAME (OBJECT-ROLES ,OBJECT)))

(DEFMACRO ROLE-NAME (ROLE)
  "Return the name of this role."
  `(FIRST ,ROLE))

(DEFMACRO ROLE-OBJECTS (ROLE)
  "Return the objects in which the object has this role."
  `(REST ,ROLE))

(DEFMACRO MAKE-ROLE (ROLE-NAME ROLE-OBJECT)
  "Return an initial role list with one object."
  `(LIST ,ROLE-NAME ,ROLE-OBJECT))

;;; ASPECTS of an object are a list
;;; Each aspect consists of a name and a definition
;;; A definition consists of a type and a filler

(DEFMACRO FIND-ASPECT (OBJECT ASPECT-NAME)
  "Find aspect in an object."
  `(ASSOC ,ASPECT-NAME (OBJECT-ASPECTS ,OBJECT)))

(DEFUN ASPECT-NAME (ASPECT)
  "Return the name of this aspect."
  (FIRST ASPECT))

(DEFMACRO ASPECT-NAMES (OBJECT)
  "Retrieve all the aspect-names in an object."
  `(MAPCAR #'ASPECT-NAME
	   (OBJECT-ASPECTS ,OBJECT)))

(DEFMACRO ASPECT-DEFINITION (ASPECT)
  "Return the definition of this aspect, in terms of type and filler."
  `(REST ,ASPECT))

(DEFMACRO ASPECT-TYPE (ASPECT-DEFINITION)
  "Return the type of this aspect definition."
  `(FIRST ,ASPECT-DEFINITION))

(DEFMACRO ASPECT-FILLER (ASPECT-DEFINITION)
  "Return the filler of this aspect definition."
  `(REST ,ASPECT-DEFINITION))

(DEFMACRO MAKE-ASPECT-DEFINITION (TYPE FILLER)
  "Make aspect definition with given type and filler."
  `(CONS ,TYPE ,FILLER))

(DEFMACRO MAKE-ASPECT (NAME TYPE FILLER)
  "Make aspect with given name, type and filler."
  `(CONS ,NAME (MAKE-ASPECT-DEFINITION ,TYPE ,FILLER)))

;;; MEMOS is simply a list of aspect-names

;;; DEPENDENCIES of an object are a list
;;; Each dependency consists of a dependent-aspect and dependent-objects

(DEFMACRO FIND-DEPENDENCY (OBJECT DEPENDENT-ASPECT-NAME)
  "Find dependency in an object."
  `(ASSOC ,DEPENDENT-ASPECT-NAME (OBJECT-DEPENDENCIES ,OBJECT)))

(DEFMACRO DEPENDENCY-ASPECT (DEPENDENCY)
  "Return the aspect which is dependent."
  `(FIRST ,DEPENDENCY))

(DEFMACRO DEPENDENCY-OBJECTS (DEPENDENCY)
  "Return the objects which are dependent."
  `(REST ,DEPENDENCY))

(DEFMACRO MAKE-DEPENDENCY (DEPENDENT-ASPECT DEPENDENT-OBJECT)
  "Make an initial dependency with one dependent object."
  `(LIST ,DEPENDENT-ASPECT ,DEPENDENT-OBJECT))

;;; ----- Names and coercing -----

;;; The NAMED OBJECT is an extension of the basic object

(DEFSTRUCT (NAMED-OBJECT
	     (:INCLUDE OBJECT)
	     (:PRINT-FUNCTION PRESENT-NAMED-OBJECT))
  "Named CommonORBIT object. Such an object not only has a referent, but
   also a name, which must be a non-NIL symbol."
  (NAME NIL :TYPE SYMBOL))

(DEFUN ANONYMOUSP (OBJECT)
  "True if the argument is an anonymous CommonORBIT object, that is,
   an object but not a named object."
  (AND (OBJECTP OBJECT)
       (NOT (NAMED-OBJECT-P OBJECT))))

;;; A named object has a name, which is a symbol, and may be accessed by
;;; that name in all user applications.
;;; A symbol is considered as an object if it has an object under the
;;; indicator :STRUCTURE on its property list.

(DEFMACRO OBJECT-STRUCTURE (SYMBOL)
  "Retrieve the object structure of this symbol."
  `(GET ,SYMBOL :STRUCTURE))

(DEFUN OBJECT (OBJECT)
  "Coerce the argument to a CommonORBIT object, if possible, otherwise
   return NIL. The argument may be an object or a symbol which is the name
   of an existing object. Can also be used as a predicate to check whether
   the argument is either an object or the name of one."
  (IF (OBJECTP OBJECT)
      OBJECT
      (IF (SYMBOLP OBJECT)
	  (OBJECT-STRUCTURE OBJECT))))

(DEFMACRO EOBJECT (OBJECT)
  "Coerce the argument to a CommonORBIT object. The argument may be an
   object or a symbol which is the name of an existing object. If the
   argument cannot be coerced to an object, an error is signaled."
   `(OR (OBJECT ,OBJECT)
	(ERROR "~S cannot be coerced to an object." ,OBJECT)))

(DEFUN OBJECT-NAME (OBJECT)
  "Return the name (a symbol) of the argument (a CommonORBIT object).
   If the argument is no CommonORBIT object, NIL is returned."
  (LET ((OBJ (OBJECT OBJECT)))
    (IF (NAMED-OBJECT-P OBJ)
	(NAMED-OBJECT-NAME OBJ))))

(DEFUN OBJECT-STRING (OBJECT)
  "Coerce the argument to a string. Like STRING, but if the argument is a
   named CommonORBIT object, the symbol-name of the object name is used."
  (IF (NAMED-OBJECT-P OBJECT)
      (SYMBOL-NAME (NAMED-OBJECT-NAME OBJECT))
      (STRING OBJECT)))

(DEFUN NAME-OR-OBJECT (OBJECT)
  "Return the name if the argument is a named object, or the object itself
   if it is an anonymous object, or NIL if it is not an object at all."
  (LET ((OBJ (OBJECT OBJECT)))
    (IF (NAMED-OBJECT-P OBJ)
	(NAMED-OBJECT-NAME OBJ)
	OBJ)))

;;; ----- multiple references -----

;;; Because an object is a two-level structure, we can create multiple
;;; references to the same object. Any changes to an object
;;; will also affect all other objects with the same referent.

(DEFUN MAKE-REFERENCE (OBJECT)
  "Create a new object which gets the same referent as the given
   object."
  (MAKE-OBJECT :REFERENT (OBJECT-REFERENT (OBJECT OBJECT))))

(DEFUN MAKE-NAMED-REFERENCE (OBJECT NAME)
  "Create a new named object which gets the same referent as the given
   object."
  (UNLESS (AND (OBJECT NAME)
	       (OQL (OBJECT OBJECT) (OBJECT NAME)))
    (WHEN (AND (OBJECT-STRUCTURE NAME) (NOT *REINITIALIZE-OBJECTS*))
      (CERROR "Reuse the name."
	      "~S is already the name of an object."
	      NAME))
    (SETF (OBJECT-STRUCTURE NAME)
	  (MAKE-NAMED-OBJECT :REFERENT (OBJECT-REFERENT (OBJECT OBJECT))
			     :NAME NAME))))

;;; ----- Presenting objects -----

;;; PRESENT-NAMED-OBJECT, PRESENT-ANONYMOUS-OBJECT and PRESENT-OBJECT
;;; may be redefined according to user needs, for example to use
;;; special interaction environments such as CLIM.

(DEFUN PRESENT-NAMED-OBJECT (OBJECT &OPTIONAL
			     (OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Present object in a stream.
   This function is used in the object structure definition."
  (DECLARE (IGNORE MEMORY))
  (PRINT-NAMED-OBJECT OBJECT OUTPUT-STREAM))

(DEFUN PRINT-NAMED-OBJECT (OBJECT &OPTIONAL
			   (OUTPUT-STREAM *STANDARD-OUTPUT*))
  "Print a named object using its name."
  (FORMAT OUTPUT-STREAM (IF *ONLY-NAME*	;just write the name
			    "~S"
			    "#<object ~S>")
	  (NAMED-OBJECT-NAME OBJECT)))

(DEFUN PRESENT-ANONYMOUS-OBJECT (OBJECT &OPTIONAL
				 (OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Present object in a stream.
   This function is used in the object structure definition."
  (PRINT-ANONYMOUS-OBJECT OBJECT OUTPUT-STREAM MEMORY))

(DEFUN PRINT-ANONYMOUS-OBJECT (OBJECT &OPTIONAL
			       (OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Print an anonymous object using roles or proxies."
  (IF (NOT (LISTP MEMORY))		;no memory (probably 0)
      (SETF MEMORY NIL))		;initialize memory
  (IF (MEMBER OBJECT MEMORY :TEST #'OQL)	;circular
      (PRINC "#<itself>" OUTPUT-STREAM)
      (PROGN
	(PUSH OBJECT MEMORY)		;remember object
	(OR (DOLIST (ROLE (OBJECT-ROLES OBJECT))
	      ;; choose appropriate role
	      (UNLESS (INTERSECTION MEMORY (ROLE-OBJECTS ROLE)
				    :TEST #'OQL)
		;; this role does not cause circularity
		(PRINT-ROLE ROLE OUTPUT-STREAM MEMORY)
		(RETURN T)))
	    (IF (OBJECT-PROXIES OBJECT)
		(PRINT-PROXIES OBJECT OUTPUT-STREAM MEMORY)
		;; anonymous without proxies or roles
		(PRINC "#<an object>" OUTPUT-STREAM))))))

(DEFUN PRESENT-OBJECT (OBJECT &OPTIONAL
		       (OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Present object. Dispatch according to named or not."
  (IF (NAMED-OBJECT-P OBJECT)
      (PRINT-NAMED-OBJECT OBJECT OUTPUT-STREAM)
      (PRINT-ANONYMOUS-OBJECT OBJECT OUTPUT-STREAM MEMORY)))

;;; Present several objects

(DEFUN PRESENT-OBJECTS (LIST &OPTIONAL
			(OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Present the objects in the list, separated by comma's and 'and'."
  (UNLESS (NULL LIST)
    (PRINC " " OUTPUT-STREAM)
    (IF (ATOM LIST)			;it is really just one
	(PRESENT-OBJECT LIST OUTPUT-STREAM MEMORY)
	(PROGN				;print first element, then recurse
	  (PRESENT-OBJECT (FIRST LIST) OUTPUT-STREAM MEMORY)
	  (COND ((REST (REST LIST))	;at least two more elements
		 (PRINC "," OUTPUT-STREAM)
		 (PRESENT-OBJECTS (REST LIST) OUTPUT-STREAM MEMORY))
		((REST LIST)		;there is one more element
		 (PRINC " and" OUTPUT-STREAM)
		 (PRESENT-OBJECTS (REST LIST) OUTPUT-STREAM MEMORY)))))))

;;; Present object by means of some property

(DEFUN PRINT-ROLE (ROLE OUTPUT-STREAM MEMORY)
  "Print object by means of role; used only by
   PRINT-ANONYMOUS-OBJECT."
  (FORMAT OUTPUT-STREAM "#<the ~S of" (ROLE-NAME ROLE))
  (PRESENT-OBJECTS (ROLE-OBJECTS ROLE) OUTPUT-STREAM MEMORY)
  (PRINC ">" OUTPUT-STREAM))

(DEFUN PRINT-PROXIES (OBJECT OUTPUT-STREAM MEMORY)
  "Print object by means of proxies; used only by
   PRINT-ANONYMOUS-OBJECT."
  (PRINC "#<a client of" OUTPUT-STREAM)
  (PRESENT-OBJECTS (OBJECT-PROXIES OBJECT) OUTPUT-STREAM MEMORY)
  (PRINC ">" OUTPUT-STREAM))

;;; Show description

(DEFUN SHOW (OBJECT &OPTIONAL (OUTPUT-STREAM *STANDARD-OUTPUT*))
  "Display a description of the object to the output stream.
   The name (if named), proxies, roles, and aspect definitions are shown."
  (LET ((OBJ (OBJECT OBJECT)))
    (IF (NOT (OBJECTP OBJ))
	(FORMAT OUTPUT-STREAM "~&~S is not an object." OBJECT)
	(PROGN
	  ;; print name if a named object
	  (IF (NAMED-OBJECT-P OBJ)
	      (FORMAT OUTPUT-STREAM "~&An object with name ~S."
		      (NAMED-OBJECT-NAME OBJ))
	      (FORMAT OUTPUT-STREAM "~&An anonymous object."))
	  ;; print documentation unless empty string
	  (UNLESS (STRING= (OBJECT-DOCUMENTATION OBJ) "")
	    (FORMAT OUTPUT-STREAM "~%~S" (OBJECT-DOCUMENTATION OBJ)))
	  ;; print proxies
	  (WHEN (OBJECT-PROXIES OBJ)
	    (FORMAT OUTPUT-STREAM "~% It is a client of")
	    (PRESENT-OBJECTS (OBJECT-PROXIES OBJ) OUTPUT-STREAM (LIST OBJ))
	    (PRINC "." OUTPUT-STREAM))
	  ;; print role-objects for each role-name
	  (DOLIST (ROLE (OBJECT-ROLES OBJ)) 
	    (FORMAT OUTPUT-STREAM "~% It is ~S of" (ROLE-NAME ROLE))
	    (PRESENT-OBJECTS (ROLE-OBJECTS ROLE) OUTPUT-STREAM (LIST OBJ))
	    (PRINC "." OUTPUT-STREAM))
	  ;; print definition for each aspect-name
	  (DOLIST (ASPECT (OBJECT-ASPECTS OBJ))
	    (LET ((TYPE (ASPECT-TYPE (ASPECT-DEFINITION ASPECT)))
		  (FILLER (ASPECT-FILLER (ASPECT-DEFINITION ASPECT))))
	      (FORMAT OUTPUT-STREAM "~%  with ~S ~S = "
		      (ASPECT-NAME ASPECT) TYPE)
	      (TYPECASE FILLER
		(OBJECT (PRESENT-OBJECT FILLER OUTPUT-STREAM (LIST OBJ)))
		(STRING (PRIN1 FILLER OUTPUT-STREAM))
		(VECTOR (PRINC "a vector with elements:" OUTPUT-STREAM)
			(DOTIMES (INDEX (LENGTH FILLER))
			  (FORMAT OUTPUT-STREAM "~%~8D: ~S"
				  INDEX (AREF FILLER INDEX))))
		(CONS (IF (EQ TYPE :OBJECTS)
			  (PRESENT-OBJECTS FILLER OUTPUT-STREAM (LIST OBJ))
			  (WRITE FILLER :STREAM OUTPUT-STREAM :PRETTY T)))
		(OTHERWISE
		  ;; symbol, number, character, array, etc.
		  (WRITE FILLER :STREAM OUTPUT-STREAM :PRETTY T)))))))
    OBJ))

;;; ----- Creation of a named object -----

(DEFUN THE-NAMED-OBJECT (OBJECT-NAME)
  "Find or create a named object with the given name, which must be
   a symbol. If the argument is NIL, UNDEFINED or not a symbol,
   then an error is signaled."
  (IF (OR (NULL OBJECT-NAME)		;do not allow NIL
	  (NOT (SYMBOLP OBJECT-NAME))	;allow only symbols
	  (UNDEFINEDP OBJECT-NAME))	;do not allow UNDEFINED
      (ERROR
	"Attempt to define ~S as the name of a CommonORBIT object."
	OBJECT-NAME)
      (OR (OBJECT-STRUCTURE OBJECT-NAME)
	  ;; the object does not exist, make a new one
	  (SETF (OBJECT-STRUCTURE OBJECT-NAME)
		(MAKE-NAMED-OBJECT :NAME OBJECT-NAME)))))

(DEFUN THE-OBJECT (OBJECT)
  "Coerce the argument to a CommonORBIT object. If it is already an object,
   return it; if it is a symbol, find or create the associated object."
  (IF (OBJECTP OBJECT)
      OBJECT
      (THE-NAMED-OBJECT OBJECT)))

;;; ----- Creation of aspects as lisp functions -----

(DEFMACRO OBJECT-FBOUNDP (ASPECT-NAME)
  "True if the argument, a symbol, is the name of a generic CommonORBIT
   function."
  `(GET ,ASPECT-NAME :OBJECT-ORIENTED))

(DEFMACRO GLOBAL-DEFINITION (ASPECT-NAME)
  "True if the argument, a symbol, is a generic CommonORBIT function which
   also has a global definition."
  `(GET ,ASPECT-NAME :GLOBAL-DEFINITION))

(DEFUN SAVE-GLOBAL-DEFINITION (ASPECT-NAME)
  "Save the global definition of a function."
  (SETF (GLOBAL-DEFINITION ASPECT-NAME)
	(SYMBOL-FUNCTION ASPECT-NAME)))

(DEFUN DEFUN-ASPECT (ASPECT-NAME)
  "Define an aspect as an object-oriented function."
  (EVAL
   `(DEFUN ,ASPECT-NAME (&REST ARGS)
      "CommonORBIT aspect."
      (GENERIC-APPLY ',ASPECT-NAME ARGS)))
  (SETF (OBJECT-FBOUNDP ASPECT-NAME) T)
  ASPECT-NAME)

;;; The following may be more efficient but does not allow
;;; (system-dependent) warnings for redefinitions

;(DEFUN DEFUN-ASPECT (ASPECT-NAME)
;  "Define an aspect as an object-oriented function."
;  (SETF (SYMBOL-FUNCTION ASPECT-NAME)
;	#'(LAMBDA (&REST ARGS)
;	    (GENERIC-APPLY ASPECT-NAME ARGS)))
;  (SETF (OBJECT-FBOUNDP ASPECT-NAME) T)
;  ASPECT-NAME)

(DEFUN INITIALIZE-ASPECT (ASPECT-NAME)
  "Initialize an aspect as an object-oriented function if necessary."
  (COND ((OR (NULL ASPECT-NAME)		;NIL is invalid
	     (NOT (SYMBOLP ASPECT-NAME)))	;name must be a symbol
	 (ERROR "Attempt to use ~S as a CommonORBIT aspect name."
		ASPECT-NAME))
	((OBJECT-FBOUNDP ASPECT-NAME)	;it is already defined, do nothing
	 ASPECT-NAME)
	((special-operator-p ASPECT-NAME)	;formerly SPECIAL-FORM-P
	 (ERROR				;don't allow
	   "Attempt to define special form ~S as a CommonORBIT aspect."
	   ASPECT-NAME))
	((MACRO-FUNCTION ASPECT-NAME)	;macro
	 ;; ask if the user really wants this
	 (IF (Y-OR-N-P "Redefine macro ~S as a CommonORBIT aspect? "
		       ASPECT-NAME)
	     (DEFUN-ASPECT ASPECT-NAME)))
	((FBOUNDP ASPECT-NAME)		;global function definition
	 (SAVE-GLOBAL-DEFINITION ASPECT-NAME)
	 (IF *LOAD-VERBOSE*
	     (WARN "Function ~S will be redefined as a CommonORBIT ~
	     aspect.~% The global definition is saved."
		   ASPECT-NAME))
	 (DEFUN-ASPECT ASPECT-NAME))
	(T				;new function
	 (DEFUN-ASPECT ASPECT-NAME))
	))

(DEFMACRO DEFUN-GLOBAL (ASPECT-NAME PARAMETER-LIST &BODY BODY)
  "Define a global definition for a generic function. This is similar to
   DEFUN, but allows the function to retain its generic definition.
   Instead of a parameter list and body, a function may be specified,
   which is then simply installed as the global definition."
  `(PROGN (INITIALIZE-ASPECT ',ASPECT-NAME)	;just to make sure
	  (SETF (GLOBAL-DEFINITION ',ASPECT-NAME)
		,(IF BODY
		     (FUNCALL *ASPECT-COMPILER*
			      PARAMETER-LIST  BODY)
		     ;; just a function name
		     PARAMETER-LIST))
	  ',ASPECT-NAME))

;;; ----- Extended access -----

;;; ----- Predicates -----

(DEFMACRO CLIENTP-INTERNAL (CLIENT PROXY)
  "True if CLIENT is a client of PROXY."
  `(CLIENTP-IN-LIST ,PROXY (OBJECT-PROXIES ,CLIENT)))

(DEFUN CLIENTP-IN-LIST (PROXY PROXIES)
  "True if proxy, or a client of it, is in the proxies list.
   Depth first search."
  (LOOP
    (COND ((ENDP PROXIES) (RETURN NIL))
	  ((OR (OQL PROXY (FIRST PROXIES))
	       (CLIENTP-IN-LIST PROXY (OBJECT-PROXIES (POP PROXIES))))
	   (RETURN T)))))

(DEFUN CLIENTP (CLIENT PROXY)
  "True if the first argument is a client of the second."
  (AND (SETF CLIENT (OBJECT CLIENT))
       (SETF PROXY (OBJECT PROXY))
       (CLIENTP-INTERNAL CLIENT PROXY)))

(DEFUN PROXYP (PROXY CLIENT)
  "True if the first argument is a proxy of the second."
  (CLIENTP CLIENT PROXY))

(DEFUN IS-LIKE-P (CLIENT PROXY)
  "Synonym of CLIENTP."
  (CLIENTP CLIENT PROXY))

(DEFUN IS-LIKE? (CLIENT PROXY)
  "Synonym of CLIENTP."
  (CLIENTP CLIENT PROXY))

(DEFUN ISAP (CLIENT PROXY)
  "Synonym of CLIENTP."
  (CLIENTP CLIENT PROXY))

(DEFUN ISA? (CLIENT PROXY)
  "Synonym of CLIENTP."
  (CLIENTP CLIENT PROXY))

(DEFUN CLIENT-OF-MEMBER (CLIENT PROXY-LIST)
  "True if the first argument is either a member of the proxy-list or a
   client of a member."
  (COND ((ENDP PROXY-LIST) NIL)
	((OR (OQL CLIENT (FIRST PROXY-LIST))
	     (CLIENTP CLIENT (FIRST PROXY-LIST)))
	 PROXY-LIST)
	(T (CLIENT-OF-MEMBER CLIENT (REST PROXY-LIST)))))

(DEFUN PROXY-OF-MEMBER (PROXY CLIENT-LIST)
  "True if the first argument is either a member of the client-list or a
   proxy of a member."
  (COND ((ENDP CLIENT-LIST) NIL)
	((OR (OQL PROXY (FIRST CLIENT-LIST))
	     (CLIENTP (FIRST CLIENT-LIST) PROXY))
	 CLIENT-LIST)
	(T (PROXY-OF-MEMBER PROXY (REST CLIENT-LIST)))))

;;; ----- Proxies -----

(DEFUN ALL-PROXIES (OBJECT)
  "Retrieve a list of all proxies of this object, and proxies of proxies
   recursively."
  (ALL-PROXIES-AUX OBJECT NIL))

(DEFUN ALL-PROXIES-AUX (OBJECT RESULT)
  "Recursive help function for ALL-PROXIES."
  ;; depth first, preserve local ordering but not delegation
  (DOLIST (P (OBJECT-PROXIES OBJECT) RESULT)
    (UNLESS (MEMBER P RESULT)		; :TEST #'OQL ?
      (SETF RESULT
	    (ALL-PROXIES-AUX P (CONS P RESULT))))))

(DEFUN ALL-NAMED-PROXIES (OBJECT)
  "Retrieve a list of all named proxies of this object, and proxies of
   their proxies recursively."
  (ALL-NAMED-PROXIES-AUX OBJECT NIL))

(DEFUN ALL-NAMED-PROXIES-AUX (OBJECT RESULT)
  "Recursive help function for ALL-NAMED-PROXIES."
  ;; depth first, preserve local ordering but not delegation
  (DOLIST (P (OBJECT-PROXIES OBJECT) RESULT)
    (UNLESS (MEMBER P RESULT)		; :TEST #'OQL ?
      (WHEN (NAMED-OBJECT-P P)
	(SETF RESULT
	      (ALL-NAMED-PROXIES-AUX P (CONS P RESULT)))))))

;;; Sorting objects in the specialization hierarchy

(DEFUN ALL-SORTED-PROXIES (OBJECT)
  "Retrieve a list of all proxies of this object, and proxies of proxies
   recursively, sorted according to the multiple delegation search
   principles:
   1. An object precedes its proxies.
   2. The proxy added last comes first.
   3. Local order of objects (as in the DEFOBJECT form) is preserved."
  (SEARCH-SPECIALIZATION-HIERARCHY (OBJECT-PROXIES OBJECT)))

(DEFUN SEARCH-SPECIALIZATION-HIERARCHY (LIST-OF-PROXIES)
  "Search the specialization hierarchy recursively, collecting a sorted
   list of proxies."
  (COND ((ENDP LIST-OF-PROXIES) NIL)
	((CHECK-CURRENT-PROXY (FIRST LIST-OF-PROXIES)
			      (REST LIST-OF-PROXIES))
	 ;; this proxy still has clients waiting, skip it
	 (SEARCH-SPECIALIZATION-HIERARCHY
	   (REST LIST-OF-PROXIES)))
	(T				;no clients waiting, take it
	 (CONS (FIRST LIST-OF-PROXIES)
	       (SEARCH-SPECIALIZATION-HIERARCHY
		 ;; put this proxy's proxies first in the list
		 (APPEND (OBJECT-PROXIES (FIRST LIST-OF-PROXIES))
			 ;; remove this one from the waiting list
			 ;; in case of a redundant link
			 (REMOVE (FIRST LIST-OF-PROXIES)
				 (REST LIST-OF-PROXIES))))))))

(DEFUN CHECK-CURRENT-PROXY (PROXY LIST-OF-PROXIES)
  "Does this proxy have clients in the list?"
  (LOOP
    (COND ((ENDP LIST-OF-PROXIES)
	   (RETURN NIL))
	  ((CLIENTP-INTERNAL (FIRST LIST-OF-PROXIES) PROXY)
	   (RETURN T))
	  (T (POP LIST-OF-PROXIES)))))

;;; ----- Roles -----

(DEFUN ROLE-NAMES (OBJECT &OPTIONAL (ROLE-OBJECT UNDEFINED))
  "Return a list of names of roles which the object has in the
   role-object. If the role-object is UNDEFINED or not given, all role
   names of the object in any other objects are returned."
  (DO ((ROLES (OBJECT-ROLES OBJECT) (REST ROLES))
       (RESULT))
      ((ENDP ROLES) RESULT)
    (IF (OR (UNDEFINEDP ROLE-OBJECT)
	    (MEMBER ROLE-OBJECT (ROLE-OBJECTS (FIRST ROLES))
		    :TEST #'OQL))
	(PUSH (ROLE-NAME (FIRST ROLES)) RESULT))))

(DEFUN HAS-ROLE-P (OBJECT ROLE-OBJECT ROLE-NAME)
  "True if the object has the given role in role-object."
  (MEMBER ROLE-OBJECT (ROLE-OBJECTS (FIND-ROLE OBJECT ROLE-NAME))
	  :TEST #'OQL))

;;; ----- Aspects -----

(DEFUN ALL-ASPECT-NAMES (OBJECT)
  "Return a list of all aspect names for which definitions exist in
   this object or can be retrieved by delegation."
  (UNION (REC-ASPECT-NAMES (OBJECT-PROXIES OBJECT))
	 (ASPECT-NAMES OBJECT)))

(DEFUN REC-ASPECT-NAMES (LIST-OF-OBJECTS)
  "Recursive auxiliary function used by ALL-ASPECT-NAMES."
  (AND LIST-OF-OBJECTS
       (UNION (ALL-ASPECT-NAMES (FIRST LIST-OF-OBJECTS))
	      (REC-ASPECT-NAMES (REST LIST-OF-OBJECTS)))))

;;; ----- Clients -----

(DEFUN NAMED-CLIENTS (PROXY)
  "Return the named clients of the given proxy."
  (LET ((NAMED-CLIENTS NIL))
    (DOLIST (CLIENT (OBJECT-CLIENTS PROXY))
      (WHEN (NAMED-OBJECT-P CLIENT)
	(PUSH CLIENT NAMED-CLIENTS)))
    NAMED-CLIENTS))

(DEFUN ALL-CLIENTS (PROXY &REST OBJECT-LISTS)
  "Return all clients of the given proxy; if OBJECT-LISTS are given,
   select only clients from the intersection of the given lists."
  (AND (SETF PROXY (OBJECT PROXY))
       (IF OBJECT-LISTS
	   ;; possible objects are given, select those which are clients
	   (DO ((OBJECTS (REDUCE #'INTERSECTION OBJECT-LISTS)
			 (REST OBJECTS))
		(RESULT NIL (IF (CLIENTP-INTERNAL (FIRST OBJECTS) PROXY)
				(ADJOIN (FIRST OBJECTS) RESULT)
				RESULT)))
	       ((ENDP OBJECTS) RESULT))
	   ;; no objects are given, simply give all clients of proxy
	   (DO ((SERIES (OBJECT-CLIENTS PROXY)
			(APPEND (REST SERIES) 
				(OBJECT-CLIENTS (FIRST SERIES))))
		(RESULT NIL (ADJOIN (FIRST SERIES) RESULT)))
	       ((ENDP SERIES) (REVERSE RESULT))))))

(DEFUN ALL-NAMED-CLIENTS (PROXY &REST OBJECT-LISTS)
  "Return all named clients of the given proxy; if OBJECT-LISTS are given,
   select only clients from the intersection of the given lists."
  (SETF PROXY (OBJECT PROXY))
  (WHEN PROXY
    (IF OBJECT-LISTS
	;; possible objects are given, select named clients
	(DO ((OBJECTS (REDUCE #'INTERSECTION OBJECT-LISTS)
		      (REST OBJECTS))
	     (RESULT NIL (IF (AND (CLIENTP-INTERNAL (FIRST OBJECTS) PROXY)
				  (NAMED-OBJECT-P (FIRST OBJECTS)))
			     (ADJOIN (FIRST OBJECTS) RESULT)
			     RESULT)))
	    ((ENDP OBJECTS) RESULT))
	;; no objects are given, simply give all named clients of proxy
	(DO ((SERIES (OBJECT-CLIENTS PROXY)
		     (APPEND (REST SERIES)
			     (OBJECT-CLIENTS (FIRST SERIES))))
	     (RESULT NIL (IF (NAMED-OBJECT-P (FIRST SERIES))
			     (ADJOIN (FIRST SERIES) RESULT)
			     RESULT)))
	    ((ENDP SERIES) (REVERSE RESULT))))))

;;; ----- Adding and deleting information in objects -----
;;; (only internal functions, no coercing or typechecking)

;;; ----- Adding and removing proxies -----

(DEFUN ADD-INITIAL-PROXY (CLIENT PROXY)
  "Establish client-proxy relationship. We may assume that client is
   a new object."
  ;; proxy becomes only object of client
  (SETF (OBJECT-PROXIES CLIENT) (LIST PROXY))
  ;; add client to proxy
  (PUSH CLIENT (OBJECT-CLIENTS PROXY))
  CLIENT)

(DEFUN ADD-PROXY (CLIENT PROXY)
  "Establish client-proxy relationship."
  ;; add proxy to client
  (PUSHNEW PROXY (OBJECT-PROXIES CLIENT))
  ;; add client to proxy
  (PUSHNEW CLIENT (OBJECT-CLIENTS PROXY))
  ;; add dependencies to proxy for every memoized aspect in client
  (WHEN *UPDATE-MEMOS*
    (MAPCAR #'(LAMBDA (ASPECT)
		(ADD-DEPENDENCY PROXY CLIENT ASPECT))
	    (OBJECT-MEMOS CLIENT)))
  CLIENT)

(DEFUN ADD-PROXY-AT-END (CLIENT PROXY)
  "Establish client-proxy relationship.
   Add proxy to the object after existing proxies."
  ;; add proxy to client
  (UNLESS (MEMBER PROXY (OBJECT-PROXIES CLIENT))
    (SETF (OBJECT-PROXIES CLIENT)
	  (APPEND (OBJECT-PROXIES CLIENT) (LIST PROXY)))
    ;; add client to proxy
    (PUSH CLIENT (OBJECT-CLIENTS PROXY))
    ;; add dependencies to proxy for every memoized aspect in client
    (WHEN *UPDATE-MEMOS*
      (MAPCAR #'(LAMBDA (ASPECT)
		  (ADD-DEPENDENCY PROXY CLIENT ASPECT))
	      (OBJECT-MEMOS CLIENT)))
    CLIENT))

(DEFUN DELETE-PROXY (CLIENT PROXY)
  "Delete proxy from an object."
  ;; erase memoized aspects in client which have a definition in proxy
  (WHEN *UPDATE-MEMOS*
    (MAPCAR #'(LAMBDA (ASPECT)
		(DELETE-ASPECT CLIENT ASPECT))
	    (INTERSECTION (ASPECT-NAMES PROXY)
			  (OBJECT-MEMOS CLIENT))))
  ;; delete proxy in client
  (SETF (OBJECT-PROXIES CLIENT)
	(DELETE PROXY (OBJECT-PROXIES CLIENT)))
  ;; delete client in proxy
  (SETF (OBJECT-CLIENTS PROXY)
	(DELETE CLIENT (OBJECT-CLIENTS PROXY)))
  CLIENT)

;;; ----- Adding and removing roles -----
;;; these are only used by ADD-ASPECT and DELETE-ASPECT

(DEFUN ADD-ROLE (OBJECT ROLE-OBJECT ROLE-NAME)
  "Add a role to the list of roles."
  (LET ((CURRENT-ROLE (FIND-ROLE OBJECT ROLE-NAME)))
    (IF (NULL (ROLE-OBJECTS CURRENT-ROLE))
	;; no current objects, make new role
	(PUSH (MAKE-ROLE ROLE-NAME ROLE-OBJECT) (OBJECT-ROLES OBJECT))
	;; add to existing objects
	(PUSHNEW ROLE-OBJECT (ROLE-OBJECTS CURRENT-ROLE)))))

(DEFUN DELETE-ROLE (OBJECT ROLE-OBJECT ROLE-NAME)
  "Delete role in an object."
  (LET ((CURRENT-ROLE (FIND-ROLE OBJECT ROLE-NAME)))
    (UNLESS (NULL CURRENT-ROLE)
      (IF (AND (ENDP (REST (ROLE-OBJECTS CURRENT-ROLE)))
	       (EQ ROLE-OBJECT (FIRST (ROLE-OBJECTS CURRENT-ROLE))))
	  ;; there is only one object, which is ours
	  ;; delete the whole list
	  (SETF (OBJECT-ROLES OBJECT)
		(DELETE CURRENT-ROLE (OBJECT-ROLES OBJECT)))
	  ;; delete the object
	  (SETF (ROLE-OBJECTS CURRENT-ROLE)
		(DELETE ROLE-OBJECT (ROLE-OBJECTS CURRENT-ROLE)))))))

(DEFUN UNDO-ROLE-OF-FILLER (OBJECT ASPECT-NAME CURRENT-TYPE CURRENT-FILLER
			    &OPTIONAL NEW-FILLER)
  "Undo the role which a current filler has in an object."
  (COND ((AND (EQ CURRENT-TYPE :OBJECT)
	      (OBJECTP CURRENT-FILLER))
	 ;; current definition is an object, delete role
	 (DELETE-ROLE CURRENT-FILLER
		      OBJECT
		      ASPECT-NAME))
	((AND (EQ CURRENT-TYPE :OBJECTS)
	      (CONSP CURRENT-FILLER))
	 ;; plural aspect, delete role of each object in current
	 ;; definition which is not in new definition
	 (LET ((SINGULAR (SINGULAR ASPECT-NAME)))
	   (MAPCAR
	     #'(LAMBDA (EACH-OBJECT)
		 (LET
		   ((OBJ (OBJECT EACH-OBJECT)))
		   (WHEN (AND (OBJECTP OBJ)
			      (OR (NOT (CONSP NEW-FILLER))
				  (NOT (MEMBER EACH-OBJECT NEW-FILLER))))
		     (DELETE-ROLE
		       OBJ
		       OBJECT
		       SINGULAR))))
	     CURRENT-FILLER)))))

(DEFUN ERASE-IN-ROLE-OBJECT (ROLE-OBJECT ROLE-NAME FILLER)
  "Erase a filler in an object where it has a role."
  (LET ((ASPECT (ASPECT-DEFINITION (FIND-ASPECT ROLE-OBJECT ROLE-NAME))))
    (WHEN ASPECT
      (IF (AND (EQ :OBJECT (ASPECT-TYPE ASPECT))
	       (EQ FILLER (ASPECT-FILLER ASPECT)))
	  ;; singular aspect, delete the entire aspect
	  (DELETE-ASPECT ROLE-OBJECT ROLE-NAME)
	  (IF (AND (EQ :OBJECTS (ASPECT-TYPE ASPECT))
		   (CONSP (ASPECT-FILLER ASPECT))
		   (MEMBER FILLER (ASPECT-FILLER ASPECT)))
	      ;; plural aspect, pull out the filler
	      (SETASPECT-INTERNAL ROLE-NAME ROLE-OBJECT
				  (DELETE FILLER (ASPECT-FILLER ASPECT))
				  :OBJECTS))))
    ;; now check if there is also a plural aspect
    (SETF ROLE-NAME (PLURALIZE ROLE-NAME))
    (SETF ASPECT (ASPECT-DEFINITION (FIND-ASPECT ROLE-OBJECT ROLE-NAME)))
    (WHEN (AND ASPECT
	       (EQ :OBJECTS (ASPECT-TYPE ASPECT))
	       (CONSP (ASPECT-FILLER ASPECT))
	       (MEMBER FILLER (ASPECT-FILLER ASPECT))
	       ;; plural aspect, pull out the filler
	       (SETASPECT-INTERNAL ROLE-NAME ROLE-OBJECT
				   (DELETE FILLER (ASPECT-FILLER ASPECT))
				   :OBJECTS)))))

;;; ----- Adding and removing aspects -----

(DEFUN ADD-ASPECT (OBJECT ASPECT-NAME FILLER TYPE)
  "Add an aspect to an object; undo previous roles and update."
  (INITIALIZE-ASPECT ASPECT-NAME)
  (ADD-ASPECT-INTERNAL OBJECT ASPECT-NAME FILLER TYPE))

(DEFUN ADD-ASPECT-INTERNAL (OBJECT ASPECT-NAME FILLER TYPE)
  "See ADD-ASPECT. The aspect is not initialized."
  (LET ((CURRENT-ASPECT (FIND-ASPECT OBJECT ASPECT-NAME)))
    (IF (NULL CURRENT-ASPECT)
	;; new aspect
	(PROGN
	  (PUSH (MAKE-ASPECT ASPECT-NAME TYPE FILLER)
		(OBJECT-ASPECTS OBJECT))
	  ;; do update of dependency network
	  (UPDATE-DEPENDENCY-NETWORK OBJECT ASPECT-NAME))
	;; there is an aspect
	(LET
	  ((CURRENT-DEFINITION (ASPECT-DEFINITION CURRENT-ASPECT)))
	  (UNLESS
	    ;; if type and filler are eq to those in current aspect,
	    ;; do nothing
	    (AND (EQ (ASPECT-TYPE CURRENT-DEFINITION)
		     TYPE)
		 (EQ (ASPECT-FILLER CURRENT-DEFINITION)	;EQUAL better?
		     FILLER))
	    ;; else replace it
	    ;; first undo role for current definition
	    (UNDO-ROLE-OF-FILLER
	      OBJECT
	      ASPECT-NAME
	      (ASPECT-TYPE CURRENT-DEFINITION)
	      (ASPECT-FILLER CURRENT-DEFINITION)
	      FILLER)
	    ;; replace the definition
	    (SETF (ASPECT-DEFINITION CURRENT-ASPECT)
		  (MAKE-ASPECT-DEFINITION TYPE FILLER))
	    ;; do update of dependency network
	    (UPDATE-DEPENDENCY-NETWORK OBJECT ASPECT-NAME)))))
  ASPECT-NAME)

(DEFUN DELETE-ASPECT (OBJECT ASPECT-NAME)
  "Delete an aspect in an object; undo previous roles and update."
  (LET ((CURRENT-ASPECT (FIND-ASPECT OBJECT ASPECT-NAME)))
    (UNLESS (NULL CURRENT-ASPECT)
      (LET ((CURRENT-DEFINITION (ASPECT-DEFINITION CURRENT-ASPECT)))
	;; undo role for the current definition
	(UNDO-ROLE-OF-FILLER
	  OBJECT
	  ASPECT-NAME
	  (ASPECT-TYPE CURRENT-DEFINITION)
	  (ASPECT-FILLER CURRENT-DEFINITION))
	;; delete the definition
	(SETF (OBJECT-ASPECTS OBJECT)
	      (DELETE CURRENT-ASPECT (OBJECT-ASPECTS OBJECT)))
	;; do update of dependency network
	(UPDATE-DEPENDENCY-NETWORK OBJECT ASPECT-NAME)))))

;;; ----- Some stuff to deal with dependencies -----

;;; When *memoize-aspects* is non-nil, and in the case of if-needed,
;;; values or objects can be copied from one object to another.
;;; Normally these values don't change anymore.
;;; When *update-memos* is non-nil, a dependency network is maintained.
;;; Whenever some value is memoized, dependencies are stored in
;;; the all-proxies of the object that gets the value.
;;; If the aspect definition is changed in one of those all-proxies
;;; having dependencies, the memoized definition in dependent objects
;;; is erased so that the value will be computed anew when it is asked.
;;; Also, when an object which has memoized gets a new aspect
;;; definition, it is no longer dependent.

;;; Adding and removing dependencies

(DEFUN ADD-DEPENDENCY (SOURCE DEP-OBJECT ASPECT-NAME)
  "Add a dependency in an object."
  (LET ((CURRENT-DEPENDENCY (FIND-DEPENDENCY SOURCE ASPECT-NAME)))
    (IF (NULL (DEPENDENCY-OBJECTS CURRENT-DEPENDENCY))
	;; no current dependencies, make new dependency
	(PUSH (MAKE-DEPENDENCY ASPECT-NAME DEP-OBJECT)
	      (OBJECT-DEPENDENCIES SOURCE))
	;; add to existing objects
	(PUSHNEW DEP-OBJECT (DEPENDENCY-OBJECTS CURRENT-DEPENDENCY)))))

(DEFUN DELETE-DEPENDENCY (SOURCE DEP-OBJECT ASPECT-NAME)
  "Delete a dependency in an object."
  (LET ((CURRENT-DEPENDENCY (FIND-DEPENDENCY SOURCE ASPECT-NAME)))
    (UNLESS (NULL CURRENT-DEPENDENCY)
      (IF (AND (ENDP (REST (DEPENDENCY-OBJECTS CURRENT-DEPENDENCY)))
	       (EQ DEP-OBJECT
		   (FIRST (DEPENDENCY-OBJECTS CURRENT-DEPENDENCY))))
	  ;; there is only one object, which is ours
	  ;; delete the whole list
	  (SETF (OBJECT-DEPENDENCIES SOURCE)
		(DELETE CURRENT-DEPENDENCY (OBJECT-DEPENDENCIES SOURCE)))
	  ;; delete the object
	  (SETF (DEPENDENCY-OBJECTS CURRENT-DEPENDENCY)
		(DELETE DEP-OBJECT
			(DEPENDENCY-OBJECTS CURRENT-DEPENDENCY)))))))

;;; Adding dependencies in all-proxies

(DEFUN ADD-DEPENDENCIES (OBJECT ASPECT-NAME)
  "Add dependency in all-proxies of an object and add memo in the object."
  (MAPCAR #'(LAMBDA (ALL-PROXY)
	      (ADD-DEPENDENCY ALL-PROXY OBJECT ASPECT-NAME))
	  (ALL-PROXIES OBJECT))
  ;; add memo
  (PUSHNEW ASPECT-NAME (OBJECT-MEMOS OBJECT)))

(DEFMACRO MEMOIZE-ASPECT (ASPECT-NAME OBJECT FILLER &OPTIONAL TYPE)
  "Store aspect in an object and add dependencies in all-proxies."
  `(PROGN
     (SETASPECT-INTERNAL ,ASPECT-NAME ,OBJECT ,FILLER ,TYPE)
     (WHEN *UPDATE-MEMOS*
       ;; construct dependency network
       (ADD-DEPENDENCIES ,OBJECT ,ASPECT-NAME))))

;;; Erasing memoized aspects in dependent objects

(DEFUN ERASE-MEMOIZED-ASPECTS-IN-DEPENDENTS (OBJECT ASPECT-NAME)
  "Undo memoized aspects in dependents of an object."
  (MAPCAR #'(LAMBDA (DEP-OBJECT)
	      (DELETE-ASPECT DEP-OBJECT ASPECT-NAME))
	  (DEPENDENCY-OBJECTS (FIND-DEPENDENCY OBJECT ASPECT-NAME))))

;;; Update network when aspect is changed

(DEFUN UPDATE-DEPENDENCY-NETWORK (OBJECT ASPECT-NAME)
  "Update the dependency network when an object gets a new aspect."
  (WHEN *UPDATE-MEMOS*
    ;; first erase definitions memoized by other objects
    (ERASE-MEMOIZED-ASPECTS-IN-DEPENDENTS OBJECT ASPECT-NAME)
    ;; then delete dependency pointers to this object
    (MAPCAR
      #'(LAMBDA (ALL-PROXY)
	  (DELETE-DEPENDENCY ALL-PROXY OBJECT ASPECT-NAME))
      (ALL-PROXIES OBJECT))
    ;; the object knows that the memoization is undone
    (SETF (OBJECT-MEMOS OBJECT)
	  (DELETE ASPECT-NAME (OBJECT-MEMOS OBJECT)))))

;;; ----- High-level creation and change -----

;;; Making an anonymous client

(DEFUN MAKE-ANONYMOUS-CLIENT-INTERNAL (PROXY)
  "Return an anonymous client of the given object, without coercing."
  (ADD-INITIAL-PROXY (MAKE-OBJECT) PROXY))

(DEFUN MAKE-ANONYMOUS-CLIENT (PROXY)
  "Return an anonymous client of the given object."
  (ADD-INITIAL-PROXY (MAKE-OBJECT) (THE-OBJECT PROXY)))

;;; Making delegation links

(DEFUN IS-CLIENT-INTERNAL (CLIENT PROXY)
  "Make client-proxy relationship. Check circularity. Maybe prune the
   specialization hierarchy if *PRUNE-PROXIES* is on."
  (COND
    ((OQL CLIENT PROXY)			;client = proxy
     (CERROR "Do it anyway."		;at your own risk...
	     "Attempt to make ~S a proxy of itself."
	     CLIENT)
     (ADD-PROXY CLIENT PROXY))
    ((NOT *PRUNE-PROXIES*)		;don't prune the proxies
     ;; check circularity
     (WHEN (CLIENTP-INTERNAL PROXY CLIENT)
       (CERROR "Do it anyway."
	       "Making ~S a proxy of ~S will cause circularity."
	       PROXY
	       CLIENT))
     (ADD-PROXY CLIENT PROXY))
    ;; prune the proxies if necessary
    ((CLIENTP-INTERNAL CLIENT PROXY)
     ;; client already delegates to proxy
     (WHEN *LOAD-VERBOSE*		;this is a minor warning
       (WARN "~S is already a direct or indirect client of ~S.~
                 No new relation will be made."
	     CLIENT
	     PROXY))
     CLIENT)
    (T					;new proxy is not already there
     ;; check circularity
     (WHEN (CLIENTP-INTERNAL PROXY CLIENT)
       (CERROR "Do it anyway."
	       "Making ~S a proxy of ~S will cause circularity."
	       PROXY
	       CLIENT))
     ;; undo proxies of client which are all-proxies of new proxy
     (MAPCAR #'(LAMBDA (P)
		 (DELETE-PROXY CLIENT P))
	     (INTERSECTION (OBJECT-PROXIES CLIENT) (ALL-PROXIES PROXY)
			   :TEST #'OQL))
     ;; add proxy
     (ADD-PROXY CLIENT PROXY)))
  )

(DEFUN IS-CLIENT (CLIENT PROXY)
  "Establish a client-proxy relation. The client will then by default
   delegate all aspects to the proxy. Maybe prune the specialization
   hierarchy if *PRUNE-PROXIES* is on."
  (IS-CLIENT-INTERNAL (THE-OBJECT CLIENT) (THE-OBJECT PROXY)))

(DEFUN IS-PROXY (PROXY CLIENT)
  "Inverse of IS-CLIENT."
  (IS-CLIENT CLIENT PROXY))

(DEFUN IS-LIKE (CLIENT PROXY)
  "Synonym of IS-CLIENT."
  (IS-CLIENT CLIENT PROXY))

(DEFUN ISA (CLIENT PROXY)
  "Synonym of IS-CLIENT."
  (IS-CLIENT CLIENT PROXY))

(DEFUN IS-NO-CLIENT (CLIENT PROXY)
  "Delete proxy from this client's proxies. However, the client may still
   delegate to the proxy indirectly via another proxy."
  (AND (SETF CLIENT (OBJECT CLIENT))
       (SETF PROXY (OBJECT PROXY))
       (DELETE-PROXY CLIENT PROXY)))

(DEFUN IS-NO-PROXY (PROXY CLIENT)
  "Inverse of IS-NO-CLIENT."
  (IS-NO-CLIENT CLIENT PROXY))

;;; ----- Function composition and paths -----

(DEFMACRO COMPOSE-FUNCTIONS (OBJECT FUNCTIONS)
  "Function composition in reverse order. The function names are not
   evaluated."
  (DO ((F FUNCTIONS (REST F))
       (EXPANSION OBJECT `(,(FIRST F) ,EXPANSION)))
      ((ENDP F) EXPANSION)))

(DEFMACRO >> (OBJECT &REST FUNCTIONS)
  "Function composition in reverse order. The function names are not
   evaluated. Example:
     (>> 'PAUL WIFE OCCUPATION) == (OCCUPATION (WIFE 'PAUL))"
  `(COMPOSE-FUNCTIONS ,OBJECT ,FUNCTIONS))

(DEFUN THE-REFERENT (FUNCTION OBJECT)
  "Find or make the referent for one step in a path."
  (LET ((FOUND-REFERENT (IF (FBOUNDP FUNCTION)
			    ;; call the function on the object
			    (FUNCALL FUNCTION OBJECT)
			    ;; undefined function
			    UNDEFINED)))
    (COND ((UNDEFINEDP FOUND-REFERENT)	;make an object for the path
	   (LET ((NEW-ANON (MAKE-OBJECT)))
	     (SETASPECT FUNCTION OBJECT NEW-ANON :OBJECT)
	     NEW-ANON))
	  ;; there is something, it may be a value or the result of
	  ;; an application, check if it is an object and has the role
	  ((NOT (OBJECT FOUND-REFERENT))	;no object
	   (ERROR
	     "Path ~S in ~S evaluates to ~S, which is not an object."
	     FUNCTION OBJECT FOUND-REFERENT))
	  ((NOT (HAS-ROLE-P FOUND-REFERENT OBJECT FUNCTION))	;no role
	   ;; give it the role with setaspect
	   (SETASPECT FUNCTION OBJECT FOUND-REFERENT :OBJECT)
	   FOUND-REFERENT)
	  (T FOUND-REFERENT))))

(DEFMACRO PATH-REFERENT (OBJECT ASPECTS)
  "Find or make the objects in a path, return last object. The aspect
   names are not evaluated. The first argument may be an object or the
   name of an object."
  ;; (path-referent 'ria (neighbor cat)) ==>
  ;; (the-referent 'cat (the-referent 'neighbor (the-object 'ria)))
  (DO ((F ASPECTS (REST F))
       (EXPANSION `(THE-OBJECT ,OBJECT)
		  `(THE-REFERENT ',(FIRST F) ,EXPANSION)))
      ((ENDP F) EXPANSION)))

(DEFMACRO PATH (OBJECT &REST ASPECTS)
  "Find or make the objects in a path, return the last object. The aspect
   names are not evaluated. Example:
   (PATH 'MARIA NEIGHBOR CAT) is like (CAT (NEIGHBOR 'MARIA)) except that
   if any step in the path is UNDEFINED, an anonymous object is created
   and stored."
  ;; (path 'ria neighbor cat) ==
  ;; (the-referent 'cat (the-referent 'neighbor (the-object 'ria)))
  `(PATH-REFERENT ,OBJECT ,ASPECTS))

;;; ----- Retrieving the definition of an aspect for an object -----

(DEFUN GET-DEFINITION (ASPECT-NAME OBJECT)
  "Get the definition of an aspect for an object.
   Return three values:
   1. the aspect type (if found, otherwise NIL);
   2. the aspect filler (if found, otherwise NIL);
   3. the proxy providing the definition (if found AND delegated,
      otherwise NIL)."
  (PROG (OWN-DEFINITION)
	;; first see if the object has a definition of its own
	(IF (SETF OWN-DEFINITION
		  (ASPECT-DEFINITION (FIND-ASPECT OBJECT ASPECT-NAME)))
	    (RETURN (VALUES (ASPECT-TYPE OWN-DEFINITION)
			    (ASPECT-FILLER OWN-DEFINITION)
			    NIL))
	    ;; otherwise get the definition from the proxies
	    (RETURN (GET-DEFINITION-FROM-PROXIES ASPECT-NAME
						 (OBJECT-PROXIES OBJECT))))))

(DEFUN GET-DEFINITION-FROM-PROXIES (ASPECT-NAME LIST-OF-PROXIES)
  "Search the specialization hierarchy recursively for a definition,
   starting with the given list of proxies.
   Return three values as required by GET-DEFINITION."
  (COND ((ENDP LIST-OF-PROXIES) (VALUES NIL NIL NIL))
	((CHECK-CURRENT-PROXY (FIRST LIST-OF-PROXIES)
			      (REST LIST-OF-PROXIES))
	 ;; this proxy still has clients waiting, skip it
	 (GET-DEFINITION-FROM-PROXIES
	   ASPECT-NAME
	   (REST LIST-OF-PROXIES)))
	(T				;no clients waiting, try it
	 (LET ((ASPECT (FIND-ASPECT (FIRST LIST-OF-PROXIES)
				    ASPECT-NAME)))
	   (IF ASPECT
	       (VALUES (ASPECT-TYPE (ASPECT-DEFINITION ASPECT))
		       (ASPECT-FILLER (ASPECT-DEFINITION ASPECT))
		       (FIRST LIST-OF-PROXIES))
	       (GET-DEFINITION-FROM-PROXIES
		 ASPECT-NAME
		 ;; put this proxy's proxies first in the list
		 (APPEND (OBJECT-PROXIES (FIRST LIST-OF-PROXIES))
			 ;; remove this one from the waiting list
			 ;; in case of a redundant link
			 (REMOVE (FIRST LIST-OF-PROXIES)
				 (REST LIST-OF-PROXIES)))))))))

;;; ----- Intension and delegate -----

(DEFUN ASPECT-TO-CLOSURE (TYPE FILLER ASPECT-NAME SOURCE)
  "Convert aspect definition to a closure which is the intension."
  (ECASE TYPE
    ((:FUNCTION :IF-NEEDED)
     FILLER)
    (:OBJECT
      (IF (AND (ANONYMOUSP FILLER)
	       SOURCE)
	  ;; there is an inherited anonymous object
	  #'(LAMBDA (&REST ARGS)
	      (DECLARE (IGNORE ARGS))
	      (MAKE-ANONYMOUS-CLIENT-INTERNAL FILLER))
	  ;; an object which is named or not inherited
	  #'(LAMBDA (&REST ARGS)
	      (DECLARE (IGNORE ARGS))
	      FILLER)))
    ((:OBJECTS :VALUE)
      #'(LAMBDA (&REST ARGS)
	       (DECLARE (IGNORE ARGS))
	  FILLER))
    ((NIL)
      (OR (GLOBAL-DEFINITION ASPECT-NAME)
	  #'(LAMBDA (&REST ARGS)
	      (DECLARE (IGNORE ARGS))
	      UNDEFINED)))))

(DEFUN INTENSION (ASPECT-NAME OBJECT &REST ARGS)
  "Return two values:
   1. The definition of the aspect for the object as a closure. This
   closure can e.g. be applied to the object (and more arguments when
   appropriate).
   2. The proxy providing the definition, or NIL if the definition was not
   obtained by delegation.
   Any &REST arguments are ignored."
  (DECLARE (IGNORE ARGS))
  (SETF OBJECT (OBJECT OBJECT))
  (MULTIPLE-VALUE-BIND (TYPE FILLER SOURCE)
      (IF OBJECT			;is it really an object?
	  (GET-DEFINITION ASPECT-NAME OBJECT)
	  NIL)
    (VALUES (ASPECT-TO-CLOSURE TYPE FILLER ASPECT-NAME SOURCE) SOURCE)))

(DEFUN INTENSION-FROM-PROXIES (ASPECT-NAME OBJECT &REST ARGS)
  "Like INTENSION, but tries to find the definition only by delegation to
   the proxies."
  (DECLARE (IGNORE ARGS))
  (SETF OBJECT (OBJECT OBJECT))
  (MULTIPLE-VALUE-BIND (TYPE FILLER SOURCE)
      (IF OBJECT			;is it really an object?
	  (GET-DEFINITION-FROM-PROXIES ASPECT-NAME (OBJECT-PROXIES OBJECT))
	  NIL)
    (VALUES (ASPECT-TO-CLOSURE TYPE FILLER ASPECT-NAME SOURCE) SOURCE)))

(DEFMACRO DELEGATE ((ASPECT PROXY) &BODY ARGUMENTS)
  "Delegate an aspect to a proxy by looking up the intension of the aspect
   for the proxy and then applying that closure to the given arguments.
   The first of those arguments is normally an object, just like the
   arguments of a generic function application."
  `(FUNCALL (INTENSION ',ASPECT ,PROXY)
	    ,@ARGUMENTS))

(DEFMACRO INHERIT (&REST ARGS)
  "Synonym of DELEGATE."
  `(DELEGATE ,@ARGS))

;;; ----- Structured inheritance -----

;;; At the moment, structured inheritance is static: a delegation link
;;; between a filler in an object and a corresponding filler in the
;;; proxies is established during object creation.  This link is not
;;; undone when the filler in the proxies loses its role.

;;; This may need revision.

(DEFUN DO-STRUCTURED-DELEGATION (FILLER OBJECT ASPECT-NAME)
  "Make a delegation link between the filler for an aspect in an object
   and a possible filler for the given aspect name in the proxies of the
   object. Only one delegation link is made, because only one definition
   in the proxies is considered."
  (MULTIPLE-VALUE-BIND (TYPE FILLER-IN-PROXY)
      (GET-DEFINITION-FROM-PROXIES ASPECT-NAME (OBJECT-PROXIES OBJECT))
    (IF (AND (EQ :OBJECT TYPE)		;type ok
	     FILLER-IN-PROXY		;there is something
	     (ANONYMOUSP FILLER-IN-PROXY)	;it is anonymous
	     (NOT (OQL FILLER FILLER-IN-PROXY))	;not this filler
	     (NOT (CLIENTP-INTERNAL FILLER-IN-PROXY FILLER)))	;not circular
	;; add the proxy, but add it at the end, because it is less
	;; important than the proxies given explicitly in the definition
	(ADD-PROXY-AT-END FILLER FILLER-IN-PROXY))))

;;; Sorry, there is no structured delegation at all for type :OBJECTS.
;;; Here is a provisional hack:

(DEFUN INSTANTIATE-OBJECTS (OBJECTS)
  "Make a list of anonymous clients from a list of objects. Useful
   because there is no structured delegation yet for type :OBJECTS."
  (IF (UNDEFINEDP OBJECTS)
      NIL
      (ETYPECASE OBJECTS
	(OBJECT
	  ;; it is an object, presumably an anonymous client created
	  ;; by delegation, make a list for consistency
	  (LIST OBJECTS))
	(LIST
	  ;; assuming this list contains objects, instantiate them
	  (MAPCAR #'MAKE-ANONYMOUS-CLIENT OBJECTS)))))

;;; ----- Defining aspects -----

(DEFUN SETASPECT (ASPECT-NAME OBJECT FILLER &OPTIONAL TYPE)
  "Define an aspect in an object. The aspect name is evaluated.
   The object must be a real object, not a name.
   The type can be :VALUE, :OBJECT, :OBJECTS, or NIL (which defaults to
   the right thing)."
  ;; coerce filler, set default for type if necessary
  (INITIALIZE-ASPECT ASPECT-NAME)
  (SETASPECT-INTERNAL ASPECT-NAME OBJECT FILLER TYPE))

(DEFUN SETASPECT-INTERNAL (ASPECT-NAME OBJECT FILLER &OPTIONAL TYPE)
  "See SETASPECT. The aspect is not initialized."
  (TYPECASE FILLER
    (OBJECT (CASE TYPE
	      ((NIL) (SETF TYPE :OBJECT))
	      (:OBJECTS (SETF FILLER (LIST FILLER)))))
    (LIST (CASE TYPE
	    ((NIL) (IF (PLURALP ASPECT-NAME)
		       (SETF TYPE :OBJECTS)
		       (SETF TYPE :VALUE)))
	    (:OBJECT (CERROR "Make the type :VALUE instead of :OBJECT."
			     "In aspect ~S for ~S, ~
                              type :OBJECT cannot be given to ~S."
			     ASPECT-NAME OBJECT FILLER)
	     (SETF TYPE :VALUE))
	    (:OBJECTS (DOLIST (EL FILLER)
			;; check if each element is object or symbol
			(UNLESS (OR (OBJECTP EL) (AND EL (SYMBOLP EL)))
			  (CERROR "Make the aspect type :VALUE instead."
				  "Non-object ~S in aspect ~S for ~S."
				  EL ASPECT-NAME OBJECT)
			  (SETF TYPE :VALUE)
			  (RETURN T))))))
    (SYMBOL (CASE TYPE
	      ((NIL) (SETF TYPE :VALUE))
	      (:OBJECT (SETF FILLER (THE-NAMED-OBJECT FILLER)))
	      (:OBJECTS (SETF FILLER
			      (LIST (THE-NAMED-OBJECT FILLER))))))
    (OTHERWISE (CASE type
		 ((:OBJECT :OBJECTS)
		  (CERROR "Make the type :VALUE instead."
			  "In aspect ~S for ~S, ~
			  type ~S cannot be given to ~S."
			  ASPECT-NAME OBJECT TYPE FILLER)))
	       (SETF TYPE :VALUE)))
  ;; add role and do structured delegation
  (CASE TYPE
    (:OBJECT
      (ADD-ROLE FILLER OBJECT ASPECT-NAME)
      (DO-STRUCTURED-DELEGATION FILLER OBJECT ASPECT-NAME))
    (:OBJECTS
      (SETF FILLER
	    (MAPCAR
	      #'(LAMBDA (EL)
		  (TYPECASE EL
		    (OBJECT (ADD-ROLE EL OBJECT (SINGULAR ASPECT-NAME))
			    (DO-STRUCTURED-DELEGATION
			      EL OBJECT ASPECT-NAME))
		    (SYMBOL (SETF EL (THE-OBJECT EL))
			    (ADD-ROLE EL OBJECT (SINGULAR ASPECT-NAME))
			    (DO-STRUCTURED-DELEGATION
			      EL OBJECT ASPECT-NAME)))
		  EL)
	      FILLER))))
  ;; finally put aspect in the structure
  (ADD-ASPECT-INTERNAL OBJECT ASPECT-NAME FILLER TYPE))

(DEFMACRO DEFASPECT (ASPECT-NAME &OPTIONAL OBJECT &BODY DEFINITION)
  "Define an aspect. The aspect name is not evaluated. This macro is very
   flexible. The following variants are possible.
	DEFASPECT aspect
   No definition is associated with any object.
	DEFASPECT aspect :GLOBAL [([var ...]) form...]
   The definition is installed as a global definition (see DEFUN-GLOBAL).
	DEFASPECT aspect object [type] filler
   The aspect definition is associated with the given object.
        DEFASPECT (aspect ...) object [type] filler
   Same, but the aspect indicates a path.

   If the filler is a list and the aspect name ends on -S, The default
   aspect type is :OBJECTS. If the filler is an object, the default is
   :OBJECT. Otherwise it is :VALUE.
   The following keywords for explicit aspect types are possible:
	:VALUE filler
   The filler can be any Lisp object which is simply to be returned.
	:OBJECT filler
   The filler is coerced to an object. A role is stored.
	:OBJECTS filler
   The filler is coerced to a list of objects. Roles are made for each
   object.
	:FUNCTION filler
   or	:FUNCTION ([var ...]) form ...
   The filler is a function which is to be applied. The filler is a
   function with the given lambda list and forms.
	:IF-NEEDED filler
   or	:IF-NEEDED ([var ...]) form ...
   Like :FUNCTION but the result is to be memoized.
	:COPY other-object
   The aspect type and filler are copied from another object. It is an
   error if that object does not exist.
	DEFASPECT aspect filler :OF object
   The aspect type is :OBJECT. Note that filler and object are reversed.

   After :FUNCTION or :IF-NEEDED, a second keyword may be specified.
   Such a keyword may also be specified without :FUNCTION or :IF-NEEDED.
   In the latter case a default determines the real aspect type.
	:DELEGATE other-object
   The aspect is delegated to another object, which may or may not exist.
   (Default :FUNCTION).
	:ADJOIN value
   The value is added (with ADJOIN or UNION, depending on whether it is an
   atom or not) to the result of delegating this aspect to the proxies.
   (Default :IF-NEEDED).
	:REMOVE value
   The value is removed (with REMOVE or SET-DIFFERENCE, depending on
   whether it is an atom or not) to the result of delegating this aspect
   to the proxies.
   (Default :IF-NEEDED).
	:ASK [format-string] [read-form]
   The filler is a format-string which is used to query the user. This
   format string will be called with two arguments: the aspect name and
   the object. The default format-string is \"~&What is the ~A of ~A? \".
   The default form for reading input is (READ *QUERY-IO*).
   (Default :IF-NEEDED).
        :SHARE aspect ...
   The filler is a path which is followed from the object in which the
   definition occurs."
  (EXPAND-DEFASPECT ASPECT-NAME OBJECT DEFINITION))

;;; Macro expansion functions

(DEFUN EXPAND-DEFASPECT (ASPECT-NAME OBJECT DEFINITION
			 &OPTIONAL INTERNAL)
  "Expansion for DEFASPECT. Internal means that the call comes from
   DEFOBJECT, and so the object is a real object."
  (LET ((PATH NIL))
    (COND
      ((NULL OBJECT)			;no object
       `(INITIALIZE-ASPECT ',ASPECT-NAME))
      ((EQ OBJECT :GLOBAL)		;global definition
       `(DEFUN-GLOBAL ,ASPECT-NAME ,@DEFINITION))
      (T				;there is an object
       ;; adjust object and aspect if necessary
       (IF (TYPEP ASPECT-NAME 'CONS)	;cons means path
	   (SETF PATH ASPECT-NAME
		 OBJECT (MACROEXPAND
			  `(PATH-REFERENT
			     ,OBJECT ,(BUTLAST ASPECT-NAME)))
		 ASPECT-NAME (FIRST (LAST ASPECT-NAME)))
	   (UNLESS INTERNAL		;coerce
	     (SETF OBJECT `(THE-OBJECT ,OBJECT))))
       ;; now look at the definition
       (IF INTERNAL
	   (EXPAND-DEFASPECT-DEFINITION
	     ASPECT-NAME OBJECT DEFINITION PATH)
	   (LET ((OBJ (GENSYM)))
	     `(LET ((,OBJ ,OBJECT))	;eval object form only once
		,(EXPAND-DEFASPECT-DEFINITION
		   ASPECT-NAME OBJ DEFINITION PATH))))))))

(DEFUN EXPAND-DEFASPECT-DEFINITION (ASPECT-NAME OBJECT DEFINITION PATH)
  "Expansion for those parts in DEFASPECT which should go
   inside the LET which binds the object."
  (COND
    ((NULL DEFINITION)
     `(INITIALIZE-ASPECT ',ASPECT-NAME))
    (T
     (EXPAND-ASPECT-DEFINITION
       `',ASPECT-NAME OBJECT
       (REST DEFINITION) (FIRST DEFINITION)
       PATH))))

(DEFUN EXPAND-ASPECT-DEFINITION (ASPECT-NAME OBJECT FILLER-LIST TYPE PATH)
  "Expansion for definition in DEFASPECT."
  (IF FILLER-LIST			;is there a filler?
      (CASE TYPE
	(:VALUE				;skip SETASPECT
	  `(ADD-ASPECT
	     ,OBJECT ,ASPECT-NAME ,(FIRST FILLER-LIST) ,TYPE))
	((:OBJECT :OBJECTS)		;use SETASPECT to process roles
	 `(SETASPECT
	    ,ASPECT-NAME ,OBJECT ,(FIRST FILLER-LIST) ,TYPE))
	((:FUNCTION :IF-NEEDED)		;expand both the same
	 `(ADD-ASPECT
	    ,OBJECT ,ASPECT-NAME
	    ,(EXPAND-FUNCTION ASPECT-NAME OBJECT FILLER-LIST)
	    ,TYPE))
	(:OF				;inverse of :OBJECT
	  `(SETASPECT
	     ,ASPECT-NAME (THE-OBJECT ,(FIRST FILLER-LIST))
	     ,OBJECT :OBJECT))
	(:DELEGATE			;use :FUNCTION as default
	  `(ADD-ASPECT
	     ,OBJECT ,ASPECT-NAME
	     ,(EXPAND-DELEGATE ASPECT-NAME (FIRST FILLER-LIST))
	     :FUNCTION))
	((:ASK :ADJOIN :REMOVE :SHARE)	;use :IF-NEEDED as the default
	 `(ADD-ASPECT
	    ,OBJECT ,ASPECT-NAME
	    ,(EXPAND-FUNCTION ASPECT-NAME OBJECT (CONS TYPE FILLER-LIST) PATH)
	    :IF-NEEDED))
	(:COPY				;error if object does not exist
	  `(COPY-ASPECT
	     ,OBJECT (EOBJECT ,(FIRST FILLER-LIST)) ,ASPECT-NAME))
	(OTHERWISE			;type must be wrong
	  (ERROR "~S is an unknown aspect type (in ~A)"
		 ASPECT-NAME OBJECT)))
      ;; there is no filler
      (CASE TYPE
	(:ASK				;requires no filler
	  `(ADD-ASPECT
	     ,OBJECT ,ASPECT-NAME
	     ,(EXPAND-FUNCTION ASPECT-NAME OBJECT (CONS TYPE FILLER-LIST))
	     :IF-NEEDED))
	((:VALUE :OBJECT :OBJECTS :FUNCTION :IF-NEEDED :OF :DELEGATE
		 :ADJOIN :REMOVE :COPY :SHARE)
	 ;; these require a filler
	 (WARN "Missing filler in aspect ~A for ~A" ASPECT-NAME OBJECT)
	 `(INITIALIZE-ASPECT ',ASPECT-NAME))
	(OTHERWISE
	  ;; no known type, so the type is assumed to be the filler
	  `(SETASPECT ,ASPECT-NAME ,OBJECT ,TYPE)))))

(DEFUN EXPAND-FUNCTION (ASPECT-NAME OBJECT FILLER &OPTIONAL PATH)
  "Expansion for :FUNCTION in DEFASPECT."
  (CASE (FIRST FILLER)			;is there a second keyword?
    (:DELEGATE
      (EXPAND-DELEGATE ASPECT-NAME (SECOND FILLER)))
    (:ASK
      (EXPAND-ASK ASPECT-NAME (SECOND FILLER) (THIRD FILLER)))
    (:ADJOIN
      (EXPAND-ADJOIN ASPECT-NAME OBJECT (SECOND FILLER)))
    (:REMOVE
      (EXPAND-REMOVE ASPECT-NAME OBJECT (SECOND FILLER)))
    (:SHARE
      (EXPAND-SHARE ASPECT-NAME (REST FILLER) PATH))
    (OTHERWISE
      ;; no second keyword, make closure or compiled function
      (IF (ENDP (REST FILLER))		;just one element?
	  ;; assume it contains a function
	  (FUNCALL *LAMBDA-COMPILER* (FIRST FILLER))
	  ;; assume it contains a variable list and body
	  (LET ((ARGUMENT-LIST (FIRST FILLER)))
	    (CHECK-TYPE ARGUMENT-LIST CONS)
	    (FUNCALL *ASPECT-COMPILER* ARGUMENT-LIST (REST FILLER)))))))

(DEFUN EXPAND-DELEGATE (ASPECT-NAME PROXY)
  "Expansion for :DELEGATE in DEFASPECT."
  (LET ((ARGS (GENSYM)))
    `#'(LAMBDA (&REST ,ARGS)
	 (APPLY (INTENSION ,ASPECT-NAME ,PROXY)
		,ARGS))))

(DEFUN EXPAND-ASK (ASPECT-NAME FORMAT-STRING READ-FUNCTION)
  "Expansion for :ASK in DEFASPECT."
  (LET ((SELF (GENSYM)))
    `#'(LAMBDA (,SELF)
	 (FORMAT *QUERY-IO*
		 ,(OR FORMAT-STRING *ASK-FORMAT-STRING*)
		 ,ASPECT-NAME ,SELF)
	 ,(OR READ-FUNCTION '(READ *QUERY-IO*)))))

(DEFUN EXPAND-ADJOIN (ASPECT-NAME OBJECT TO-BE-ADDED)
  "Expansion for :ADJOIN in DEFASPECT."
  (LET ((ARGS (GENSYM)) (NEW (GENSYM)) (OLD (GENSYM)))
    `#'(LAMBDA (&REST ,ARGS)
	 (LET ((,NEW ,TO-BE-ADDED)
	       (,OLD
		 (DEFINEDP
		   (APPLY (INTENSION-FROM-PROXIES ,ASPECT-NAME ,OBJECT)
			  ,ARGS))))
	   (IF (CONSP ,NEW)
	       (UNION ,OLD ,NEW)
	       (ADJOIN ,NEW ,OLD))))))

(DEFUN EXPAND-REMOVE (ASPECT-NAME OBJECT TO-BE-DELETED)
  "Expansion for :REMOVE in DEFASPECT."
  (LET ((ARGS (GENSYM)) (NEW (GENSYM)) (OLD (GENSYM)))
    `#'(LAMBDA (&REST ,ARGS)
	 (LET ((,NEW ,TO-BE-DELETED)
	       (,OLD
		 (DEFINEDP
		   (APPLY (INTENSION-FROM-PROXIES ,ASPECT-NAME ,OBJECT)
			  ,ARGS))))
	   (IF (CONSP ,NEW)
	       (SET-DIFFERENCE ,OLD ,NEW)
	       (REMOVE ,NEW ,OLD))))))

(DEFUN EXPAND-SHARE (ASPECT-NAME SHARE-PATH OBJECT-PATH)
  "Expansion for :SHARE in DEFASPECT."
  (DECLARE (IGNORE OBJECT))
  `#'(LAMBDA (SELF)
       (PATH (WHERE SELF = ,@(BUTLAST OBJECT-PATH))
	     ,@SHARE-PATH ,(SECOND ASPECT-NAME))))

(DEFUN COPY-ASPECT (TO-OBJECT FROM-OBJECT ASPECT-NAME)
  "Copy an aspect to another object."
  (LET ((DEFINITION
	  (ASPECT-DEFINITION (FIND-ASPECT FROM-OBJECT ASPECT-NAME))))
    (COND
      ((NULL DEFINITION)
       (WHEN *LOAD-VERBOSE*
	 (WARN "Attempt to copy empty definition for aspect ~S ~
	 from ~S to ~S."
	       ASPECT-NAME
	       FROM-OBJECT
	       TO-OBJECT)))
      ((EQ (ASPECT-TYPE DEFINITION) :OBJECT)
       ;; it's an object so let's use SETASPECT
       (SETASPECT-INTERNAL ASPECT-NAME TO-OBJECT
			   (IF (ANONYMOUSP (ASPECT-FILLER DEFINITION))
			       ;; it's anonymous, make an client
			       (MAKE-ANONYMOUS-CLIENT-INTERNAL
				 (ASPECT-FILLER DEFINITION))
			       (ASPECT-FILLER DEFINITION))
			   (ASPECT-TYPE DEFINITION)))
      (T	;it is a value, function or if-needed
       (ADD-ASPECT TO-OBJECT ASPECT-NAME
		   (ASPECT-FILLER DEFINITION)
		   (ASPECT-TYPE DEFINITION))))))

(DEFUN UNDEFASPECT (ASPECT-NAME OBJECT)
  "Delete an aspect definition in an object."
  (DELETE-ASPECT (THE-OBJECT OBJECT) ASPECT-NAME)
  ASPECT-NAME)

;;; ----- Aspect compiler ----

;;; *ASPECT-COMPILER* is either bound to LEXICAL-CLOSURE-ASPECT
;;; or to COMPILE-ASPECT.
;;; *LAMBDA-COMPILER* is either bound to IDENTITY or to
;;; COMPILE-LAMBDA-EXPRESSION.

(DEFUN LEXICAL-CLOSURE-ASPECT (ARGS BODY)
  "Make a closure with ARGS and BODY."
  `#'(LAMBDA ,ARGS ,@BODY))

(DEFUN COMPILE-ASPECT (ARGS BODY)
  "Make a lambda with ARGS and BODY and compile it."
  (COMPILE NIL `(LAMBDA ,ARGS ,@BODY)))

(DEFUN COMPILE-LAMBDA-EXPRESSION (QUOTED-LAMBDA)
  "If the argument is a quoted lambda expression, compile the lambda
   expression, else return the argument unchanged."
  (IF (AND (CONSP QUOTED-LAMBDA)
	   (MEMBER (FIRST QUOTED-LAMBDA) '(QUOTE FUNCTION))
	   (CONSP (SECOND QUOTED-LAMBDA))
	   (FUNCTIONP (SECOND QUOTED-LAMBDA)))
      (COMPILE NIL (SECOND QUOTED-LAMBDA))
      QUOTED-LAMBDA))

;;; ----- Pushing and popping -----

(DEFMACRO PUSH-ASPECT (VALUE (ASPECT-NAME OBJECT)
		       &OPTIONAL TYPE)
  "Similar to PUSH. The value is added to the result of the evaluation of
   (ASPECT-NAME OBJECT), and this list is stored in the object. The aspect
   type may be :VALUE or :OBJECTS. It defaults to the right thing."
  `(SETASPECT ',ASPECT-NAME (THE-OBJECT ,OBJECT)
	      (CONS ,VALUE (IF (FBOUNDP ',ASPECT-NAME)
			       (DEFINEDP ,(LIST ASPECT-NAME OBJECT))))
	      ,TYPE))

(DEFMACRO PUSHNEW-ASPECT (VALUE (ASPECT-NAME OBJECT)
			  &OPTIONAL TYPE)
  "Similar to PUSHNEW. The value is added to the result of the evaluation
   of (ASPECT-NAME OBJECT), but only if not already a member, and the
   resulting list is stored in the object. The aspect type may be
   :VALUE or :OBJECTS. It defaults to the right thing."
  `(SETASPECT ',ASPECT-NAME (THE-OBJECT ,OBJECT)
	      (ADJOIN ,VALUE (IF (FBOUNDP ',ASPECT-NAME)
				 (DEFINEDP ,(LIST ASPECT-NAME OBJECT)))
		      :TEST #'OEQL)
	      ,TYPE))

(DEFMACRO POP-ASPECT ((ASPECT-NAME OBJECT) &OPTIONAL TYPE)
  "Similar to POP. The aspect filler becomes the REST of the evaluation
   of (ASPECT-NAME OBJECT). The aspect type may be :VALUE or :OBJECTS.
   It defaults to the right thing."
  `(SETASPECT ',ASPECT-NAME (THE-OBJECT ,OBJECT)
	      (IF (FBOUNDP ',ASPECT-NAME)
		  (REST (DEFINEDP ,(LIST ASPECT-NAME OBJECT))))
	      ,TYPE))

;;; ----- Adding and removing from aspect definitions -----

(DEFUN ADD-TO-ASPECT (ASPECT-NAME OBJECT OBJECTS &OPTIONAL TYPE)
  "Add the given objects in an aspect for an object. The aspect type
   may be :VALUE or :OBJECTS. It defaults to the right thing."
  (SETASPECT ASPECT-NAME (THE-OBJECT OBJECT)
	     (IF (NOT (FBOUNDP ASPECT-NAME))
		 (LIST-IF-NEEDED OBJECTS)
		 (UNION
		   (LIST-IF-NEEDED
		     (FUNCALL ASPECT-NAME (THE-OBJECT OBJECT)))
		   (LIST-IF-NEEDED OBJECTS)
		   :TEST #'OQL))
	     TYPE))

(DEFUN DELETE-FROM-ASPECT (ASPECT-NAME OBJECT OBJECTS &OPTIONAL TYPE)
  "Delete the given objects in an aspect for an object. The aspect
   type may be :VALUE or :OBJECTS. It defaults to the right thing."
  (SETASPECT ASPECT-NAME (THE-OBJECT OBJECT)
	     (IF (NOT (FBOUNDP ASPECT-NAME))
		 NIL
		 (SET-DIFFERENCE
		   (LIST-IF-NEEDED
		     (FUNCALL ASPECT-NAME (THE-OBJECT OBJECT)))
		   (LIST-IF-NEEDED OBJECTS)
		   :TEST #'OQL))
	     TYPE))

;;; ----- Defining objects -----

(DEFUN EXPAND-OBJECT-DEFINITION (FORMS OBJECT)
  "Expand proxies and aspect definitions."
  ;; used by A, AN and DEFOBJECT
  (LET ((PROXIES NIL) (ASPECTS NIL))
    ;; put the proxies in reverse order (by pushing),
    ;; give the aspects in original order
    (DOLIST (FORM FORMS (APPEND PROXIES (REVERSE ASPECTS)))
      (ETYPECASE FORM
	(SYMBOL				;a proxy
	  (PUSH `(IS-CLIENT-INTERNAL ,OBJECT (THE-NAMED-OBJECT ',FORM))
		PROXIES))
	(CONS				;an aspect definition
	  (PUSH (EXPAND-DEFASPECT (FIRST FORM) OBJECT (REST FORM) T)
		ASPECTS))
	(STRING				;put documentation with proxies
	  (PUSH `(SETF (OBJECT-DOCUMENTATION ,OBJECT) ,FORM)
		PROXIES))
	))))

(DEFMACRO A (&REST PROXIES-AND-ASPECTS)
  "Define an anonymous CommonORBIT object by assigning proxies and
   defining aspects. The arguments are not evaluated.
   Symbols are proxies, lists are aspect definitions, a string is
   documentation. Example:
     (A \"Female person\" PERSON (SEX 'FEMALE))
   Aspect definitions are processed as by DEFASPECT."
  (LET ((OBJECT (GENSYM)))
    `(LET ((,OBJECT (MAKE-OBJECT)))
       ,@(EXPAND-OBJECT-DEFINITION PROXIES-AND-ASPECTS OBJECT)
       ,OBJECT)))

(DEFMACRO AN (&REST PROXIES-AND-ASPECTS)
  "Synonym of A."
  `(A ,@PROXIES-AND-ASPECTS))

(DEFUN REINITIALIZE (OBJECT)
  "Delete aspects, proxies, dependencies, memos in object
   but keep name, documentation, roles and clients."
  ;; undefine aspects separately in order to delete roles
  ;; and dependencies in other objects; memos in this object
  ;; will also be undone
  (DOLIST (ASPECT (ASPECT-NAMES OBJECT))
    (UNDEFASPECT ASPECT OBJECT))
  ;; delete proxies
  (DOLIST (PROXY (OBJECT-PROXIES OBJECT))
    (DELETE-PROXY OBJECT PROXY))
  OBJECT)

(DEFUN MAYBE-REINITIALIZE (OBJECT)
  "Maybe reinitialize this object depending on the switch
   *REINITIALIZE-OBJECTS*."
  (COND ((NULL OBJECT) NIL)
	(*REINITIALIZE-OBJECTS* (REINITIALIZE OBJECT))
	(T OBJECT)))

(DEFMACRO DEFOBJECT (NAME &BODY PROXIES-AND-ASPECTS)
  "Define a named CommonORBIT object by assigning proxies and defining
   aspects. The arguments are not evaluated.
   Symbols are proxies, lists are aspect definitions, a string is
   documentation. Example:
     (DEFOBJECT WOMAN \"Female person\" PERSON (SEX 'FEMALE))
   Aspect definitions are processed as by DEFASPECT."
  (LET ((OBJECT (GENSYM)))
    `(LET ((,OBJECT (MAYBE-REINITIALIZE (THE-NAMED-OBJECT ',NAME))))
       ,@(EXPAND-OBJECT-DEFINITION PROXIES-AND-ASPECTS OBJECT)
       ,OBJECT)))

(DEFMACRO DEFFRAME (NAME &BODY PROXIES-AND-ASPECTS)
  "Synonym of DEFOBJECT."
  `(DEFOBJECT ,NAME ,@PROXIES-AND-ASPECTS))

(DEFMACRO DEFRAME (NAME &BODY PROXIES-AND-ASPECTS)
  "Synonym of DEFOBJECT."
  `(DEFOBJECT ,NAME ,@PROXIES-AND-ASPECTS))

(DEFUN KILL-OBJECT (OBJECT)
  "Undo all pointers to this object as far as possible in order to
   virtually annihilate it."
  (REINITIALIZE OBJECT)
  ;; delete documentation
  (SETF	(OBJECT-DOCUMENTATION OBJECT) "")
  ;; delete roles
  (DOLIST (ROLE (OBJECT-ROLES OBJECT))
    (DOLIST (ROLE-OBJECT (ROLE-OBJECTS ROLE))
      (ERASE-IN-ROLE-OBJECT ROLE-OBJECT (ROLE-NAME ROLE) OBJECT)))
  ;; delete clients
  (DOLIST (CLIENT (OBJECT-CLIENTS OBJECT))
    (DELETE-PROXY CLIENT OBJECT))
  ;; delete referent
  (SETF (OBJECT-REFERENT OBJECT) NIL)
  ;; delete name
  (WHEN (NAMED-OBJECT-P OBJECT)
    (SETF (OBJECT-STRUCTURE (OBJECT-NAME OBJECT)) NIL)
    (SETF (NAMED-OBJECT-NAME OBJECT) NIL))
  OBJECT)

;;; ----- The evaluator -----

(DEFUN GENERIC-APPLY (ASPECT-NAME ARGS)
  "Generic function application for object-oriented functions.
   Get the definition of the aspect for the object (the first argument)
   and apply that function to all the arguments."
  (LET ((OBJECT (OBJECT (FIRST ARGS))))
    (MULTIPLE-VALUE-BIND (TYPE FILLER SOURCE)
	(IF OBJECT
	    ;; object is really an object, get the definition
	    (GET-DEFINITION ASPECT-NAME OBJECT)
	    ;; no object, no definition
	    NIL)
      ;; perform action according to type
      (ECASE TYPE
	((:VALUE :OBJECTS)
	 (IF (AND *MEMOIZE-ASPECTS* SOURCE)
	     ;; memoize
	     (MEMOIZE-ASPECT ASPECT-NAME OBJECT FILLER TYPE))
	 FILLER)
	(:OBJECT
	  (IF SOURCE			;inherited
	      (COND ((ANONYMOUSP FILLER)
		     ;; make anonymous instantiation
		     ;; should we update?
		     (SETF FILLER (MAKE-ANONYMOUS-CLIENT-INTERNAL FILLER))
		     (SETASPECT-INTERNAL ASPECT-NAME OBJECT FILLER :OBJECT))
		    ;; named object, maybe memoize
		    (*MEMOIZE-ASPECTS*
		     (MEMOIZE-ASPECT ASPECT-NAME OBJECT FILLER :OBJECT))))
	  FILLER)
	((:FUNCTION :IF-NEEDED)
	 ;; first fix the arguments
	 (SETF ARGS
	       (CONS OBJECT
		     #+LISPM (SYS:COPY-IF-NECESSARY (REST ARGS))
		     #-LISPM (REST ARGS)))
	 ;; look for infinite loops if flag is on
	 (IF *DETECT-INFINITE-LOOPS*
	     (IF (MEMBER (CONS ASPECT-NAME ARGS) *STACK*
			 :TEST #'(LAMBDA (L1 L2)
				   (TREE-EQUAL L1 L2 :TEST #'OEQL)))
		 ;; there is an infinite loop
		 UNDEFINED
		 ;; no infinite loop
		 (UNWIND-PROTECT
		     (PROGN
		       (PUSH (CONS ASPECT-NAME ARGS) *STACK*)
		       ;; now apply function
		       (IF (EQ TYPE :IF-NEEDED)
			   (PROGN	;reuse the variable FILLER
			     (SETF FILLER (APPLY FILLER ARGS))
			     (IF (AND SOURCE	;inherited
				      (NOT (UNDEFINEDP FILLER)))
				 (MEMOIZE-ASPECT ASPECT-NAME OBJECT FILLER))
			     FILLER)
			   (APPLY FILLER ARGS)))
		   ;; popping from stack is protected
		   (POP *STACK*)))
	     ;; don't check for infinite loops
	     (IF (EQ TYPE :IF-NEEDED)
		 (PROGN			;reuse the variable FILLER
		   (SETF FILLER (APPLY FILLER ARGS))
		   (IF (AND SOURCE	;inherited
			    (NOT (UNDEFINEDP FILLER)))
		       (MEMOIZE-ASPECT ASPECT-NAME OBJECT FILLER))
		   FILLER)
		 (APPLY FILLER ARGS))))
	((NIL)
	  ;; no object-oriented definition
	  ;; look if there is a global definition and if so, apply it
	  (PROGN
	    (SETF FILLER (GLOBAL-DEFINITION ASPECT-NAME))
	    (IF FILLER
		(APPLY FILLER
		       #+LISPM (SYS:COPY-IF-NECESSARY ARGS)
		       #-LISPM ARGS)
		UNDEFINED)))))))

;;; ----- Reverse evaluation -----

(DEFUN REV-EVAL (ASPECT OBJECT)
  "Perform reverse evaluation. The argument may also be a name of an
   object or a list of objects."
  (COND ((NULL OBJECT) NIL)
	((CONSP OBJECT)
	 ;; a list of objects, recurse
	 (UNION (REV-EVAL ASPECT (FIRST OBJECT))
		(REV-EVAL ASPECT (REST OBJECT))
		:TEST #'OQL))
	(T
	 ;; no list, find aspect in roles, return associated objects
	 (ROLE-OBJECTS (FIND-ROLE (EOBJECT OBJECT) ASPECT)))))

(DEFUN WHOSE (ASPECT ASPECT-OR-OBJECT &REST MORE-ASPECTS-OR-OBJECT)
  "Perform reverse evaluation, return a list of those objects for which
   the aspect is defined as the given object (with type :OBJECT).
   If more than one aspect is given, they are interpreted as a path.
   Aspects are evaluated. Example:
   (DEFOBJECT PAUL (FRIEND :OBJECT 'MARY)) => #<object PAUL>
   (DEFOBJECT DAVID (FRIEND :OBJECT 'MARY)) => #<object DAVID>
   (WHOSE 'FRIEND 'MARY) => (#<object DAVID> #<object PAUL>)"
  (IF (NULL MORE-ASPECTS-OR-OBJECT)
      ;; (whose 'friend 'mary)
      (REV-EVAL ASPECT ASPECT-OR-OBJECT)
      ;; (whose 'friend 'pet 'fido)
      (REV-EVAL ASPECT
		(APPLY #'WHOSE
		       (CONS ASPECT-OR-OBJECT
			     MORE-ASPECTS-OR-OBJECT)))))

(DEFMACRO WITH (&REST ARGS)
  "Synonym of WHOSE."
  `(WHOSE ,@ARGS))

(DEFMACRO WHERE (OBJECT = &REST PATH)
  "Perform reverse evaluation, return the first object for which the
   aspect is defined as the given object with type :OBJECT.
   If more than one aspect is given, they are interpreted as a path.
   Aspects are not evaluated. Example:
   (WHERE SELF = FRIEND HUSBAND) == (FIRST (WHOSE 'FRIEND 'HUSBAND SELF))
   Note that the = is just syntactic sugar."
  #-PROCYON (DECLARE (IGNORE =))
  `(FIRST (WHOSE ,@(MAPCAR #'(LAMBDA (ASPECT)
			       (LIST 'QUOTE ASPECT))
			   PATH)
		 ,OBJECT)))

(DEFMACRO << (OBJECT = &REST PATH)
  "Synonym of WHERE."
  #-PROCYON (DECLARE (IGNORE =))
  `(FIRST (WHOSE ,@(MAPCAR #'(LAMBDA (ASPECT)
			       (LIST 'QUOTE ASPECT))
			   PATH)
		 ,OBJECT)))

;;; ----- Control structure -----

(DEFMACRO IF-DEFINED (CONDITION TRUECLAUSE
		      &OPTIONAL (FALSECLAUSE UNDEFINED))
  "Same as IF but treats UNDEFINED similar to NIL."
  `(IF (DEFINEDP ,CONDITION) ,TRUECLAUSE ,FALSECLAUSE))

(DEFMACRO OR-DEFINED (&REST ARGS)
  "Same as OR but treats UNDEFINED similar to NIL. If none of the
   arguments yields a non-NIL or non-UNDEFINED value, then UNDEFINED is
   returned."
  `(OR ,@(MAPCAR #'(LAMBDA (CLAUSE)
		     (LIST 'DEFINEDP CLAUSE))
		 ARGS)
       UNDEFINED))

(DEFMACRO AND-DEFINED (&REST ARGS)
  "Same as AND but treats UNDEFINED similar to NIL. If an argument yields
   NIL or UNDEFINED, then UNDEFINED is returned."
  `(OR (AND ,@(MAPCAR #'(LAMBDA (CLAUSE)
			  (LIST 'DEFINEDP CLAUSE))
		      ARGS))
       UNDEFINED))

(DEFMACRO NOT-DEFINED (FORM)
  "If form is NIL or UNDEFINED then true else NIL."
  `(NOT (DEFINEDP ,FORM)))

(DEFMACRO LET-DEFINED (BINDINGS &BODY BODY)
  "Like LET, but returns UNDEFINED without executing the body forms if one
   of the bindings is undefined."
  (LET ((VALUES (GENSYM)))
    `(LET ((,VALUES (LIST ,@(MAPCAR #'SECOND BINDINGS))))
       (IF (MEMBER UNDEFINED ,VALUES)
	   UNDEFINED
	   (APPLY
	     #'(LAMBDA ,(MAPCAR #'FIRST BINDINGS)
		 ,@BODY)
	     ,VALUES)))))

;;; ----- Selection -----

(DEFUN LIST-DEFINED (&REST REST)
  "Return a list of its arguments without any occurrences of UNDEFINED."
  (REMOVE-IF #'UNDEFINEDP REST))

(DEFMACRO SELECT-ALL-CLIENTS (PROXY &BODY TESTS)
  "Find all clients of a proxy which satisfy all given tests.
   Each test has the form
	(predicate aspect value [keyword ...])
   where the predicate is used to compare the aspect of each
   possible client with the given value. Example:
	(SELECT-ALL-CLIENTS 'WOMAN
	   (CLIENTP IDENTITY (OBJECT 'DOCTOR))
	   (MEMBER BIRTHYEAR '(1950 1960) :TEST #'=))
   selects all women whose birthyear is either 1950 or 1960.
   Paths of aspects are also possible:
	(MEMBER (WIFE BIRTHYEAR) '(1950 1960) :TEST #'=)
   The aspect (or path of aspects) is not evaluated."
  (LET ((CLIENT (GENSYM))
	(RESULT (GENSYM)))
    `(LET ((,RESULT NIL))
       (DOLIST (,CLIENT (ALL-CLIENTS ,PROXY) ,RESULT)
	 ;; iterating on all clients of the proxy
	 ;; collect those satisfying the tests
	 (WHEN (AND
		 ,@(MAPCAR
		     #'(LAMBDA (TEST) 
			 `(,(FIRST TEST)	;the test function
			   ;; the first argument
			   ,(IF (ATOM (SECOND TEST))	;not a path?
				(LIST (SECOND TEST) CLIENT)
				(MACROEXPAND
				  `(COMPOSE-FUNCTIONS ,CLIENT
						      ,(SECOND TEST))))
			   ;; the other arguments
			   ,@(REST (REST TEST))))
		     TESTS))
	   (PUSH ,CLIENT ,RESULT))))))

;;; ----- Switches -----

(DEFMACRO DEFSWITCH (SWITCH VARIABLE INITIAL-VALUE DOC-STRING
		     &KEY IF-ON IF-OFF)
  "Define a switch which turns the given variable on or off."
  `(PROGN
     (PUSHNEW ',VARIABLE *SWITCHES*)
     (DEFUN ,SWITCH (&OPTIONAL (BOOLEAN T))
       ,DOC-STRING
       (IF BOOLEAN
	   (PROGN ,@IF-ON (SETF ,VARIABLE T))
	   (PROGN ,@IF-OFF (SETF ,VARIABLE NIL))))
     (,SWITCH ,INITIAL-VALUE)))

(DEFUN SHOW-SWITCHES (&OPTIONAL (OUTPUT-STREAM *STANDARD-OUTPUT*))
  "Show the current settings of all CommonORBIT switches."
  (DOLIST (SWITCH *SWITCHES*)
    (FORMAT OUTPUT-STREAM "~&~A: ~A" SWITCH (SYMBOL-VALUE SWITCH))))

(DEFSWITCH MEMOIZE-ASPECTS *MEMOIZE-ASPECTS* NIL
  "Store values retrieved by delegation.")

(DEFSWITCH UPDATE-MEMOS *UPDATE-MEMOS* T
  "Update memoized information after change. Works for delegated
   information which is memoized as well as for stored results of
   :IF-NEEDED expressions.")

(DEFSWITCH PRUNE-PROXIES *PRUNE-PROXIES* NIL
  "Prune the specialization hierarchy whenever appropriate. More
   specifically, if an attempt is made to make an object P a proxy of an
   objerct C, but P is already an ancestor of C, then nothing is done;
   also, if C has a proxy which is an ancestor of P, then that proxy is
   deleted from C, so that superfluous relations in the specialization
   hierarchy are pruned. This enables a more efficient search. But note
   that it may cause unexpected behavior if proxy P is later removed,
   because once pruned, proxies are not automatically added again.")

(DEFSWITCH DETECT-INFINITE-LOOPS *DETECT-INFINITE-LOOPS* NIL
  "Keep a stack to detect infinite loops. If upon entry of a generic
   function application, an object and its arguments are already on the
   stack, then UNDEFINED is immediately returned. This allows working
   with circular definitions.")

(DEFMACRO DETECTING-INFINITE-LOOPS (&REST FORMS)
  "Evaluate the forms while temporarily detecting infinite loops. See the
   switch DETECT-INFINITE-LOOPS." 
  `(LET ((*DETECT-INFINITE-LOOPS* T))
     ,@FORMS))

(DEFSWITCH COMPILE-ASPECTS *COMPILE-ASPECTS* NIL
  "Compile lambdas in aspect definitions with type :FUNCTION or
   :IF-NEEDED, instead of making closures."
  :IF-ON
  ((SETF *ASPECT-COMPILER* #'COMPILE-ASPECT
	 *LAMBDA-COMPILER* #'COMPILE-LAMBDA-EXPRESSION))
  :IF-OFF
  ((SETF *ASPECT-COMPILER* #'LEXICAL-CLOSURE-ASPECT
	 *LAMBDA-COMPILER* #'IDENTITY)))

(DEFSWITCH REINITIALIZE-OBJECTS *REINITIALIZE-OBJECTS* T
  "Reinitialize a named object when processing a DEFOBJECT form, rather
   than just adding proxies and aspect definitions. This destroys the
   proxies, aspects, dependencies and memos of the object. The name,
   roles and clients are kept.")

(DEFSWITCH ONLY-NAME *ONLY-NAME* T
  "Print a named object by just printing its name.")

;;; end of file
