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

;;; This module provides facilities to present and manipulate
;;; graphical representations of CORBIT objects in Dynamic Windows
;;; under Genera 7.2.  This file redefines some standard CORBIT
;;; functions.  It uses facilities from the SCL package and conforms
;;; to CLtL 1.

(PROVIDE "CORBIT-PRESENTATION")

(IN-PACKAGE "CORBIT")
(EXPORT '(;; allow style to be redefined
	  *CORBIT-CHARACTER-STYLE*
	  GRAPH-NAMED-CLIENTS
	  GRAPH-PROXIES))

(REQUIRE "CORBIT" "corbit:corbit;corbit")

(DEFVAR *CORBIT-CHARACTER-STYLE* '(:SWISS :BOLD-CONDENSED-CAPS :NORMAL)
  "Character style for objects in presentations.")

;;; the presentation types

(SCL:DEFINE-PRESENTATION-TYPE OBJECT ()
   :DESCRIPTION "a CommonORBIT object"
   :NO-DEFTYPE T			;we already have a structure
   :HISTORY NIL
   :PRINTER ((OBJECT STREAM)
	     (IF (NAMED-OBJECT-P OBJECT)
		 (PRINT-NAMED-OBJECT (EOBJECT OBJECT) STREAM)
		 (PRINT-ANONYMOUS-OBJECT (EOBJECT OBJECT) STREAM)))
   :PARSER ((STREAM)			;accept new objects as well
	    (THE-OBJECT (SCL:ACCEPT 'SYMBOL
				    :STREAM STREAM
				    :PROVIDE-DEFAULT NIL))))

;;; unfortunately, there seems to be no way of including one presentation
;;; type in another

(SCL:DEFINE-PRESENTATION-TYPE NAMED-OBJECT ()
   :DESCRIPTION "a named CommonORBIT object"
   :NO-DEFTYPE T			;we already have a structure
   :HISTORY NIL
   :PRINTER ((OBJECT STREAM)
	     (PRINT-NAMED-OBJECT (EOBJECT OBJECT) STREAM))
   :PARSER ((STREAM)			;accept new objects as well
	    (THE-OBJECT (SCL:ACCEPT 'SYMBOL
				    :STREAM STREAM
				    :PROVIDE-DEFAULT NIL))))

;;; the structure printers

(SI:ALLOW-REDEFINITION 'PRESENT-NAMED-OBJECT)

(DEFUN PRESENT-NAMED-OBJECT (OBJECT &OPTIONAL
			     (OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Present object. This function is the DEFSTRUCT printer."
  (DECLARE (IGNORE MEMORY))
  (DW:WITH-OUTPUT-AS-PRESENTATION (:OBJECT OBJECT :TYPE 'NAMED-OBJECT
					   :STREAM OUTPUT-STREAM)
    (PRINT-NAMED-OBJECT OBJECT OUTPUT-STREAM)))

(SI:ALLOW-REDEFINITION 'PRESENT-ANONYMOUS-OBJECT)

(DEFUN PRESENT-ANONYMOUS-OBJECT (OBJECT &OPTIONAL
				 (OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Present object. This function is the DEFSTRUCT printer."
  (DW:WITH-OUTPUT-AS-PRESENTATION (:OBJECT OBJECT :TYPE 'OBJECT
					   :STREAM OUTPUT-STREAM)
    (PRINT-ANONYMOUS-OBJECT OBJECT OUTPUT-STREAM MEMORY)))

(SI:ALLOW-REDEFINITION 'PRESENT-OBJECT)

(DEFUN PRESENT-OBJECT (OBJECT &OPTIONAL
		       (OUTPUT-STREAM *STANDARD-OUTPUT*) MEMORY)
  "Present object. Dispatch according to named or not."
  (DW:WITH-OUTPUT-AS-PRESENTATION (:OBJECT OBJECT :TYPE 'OBJECT
					   :STREAM OUTPUT-STREAM)
    (IF (NAMED-OBJECT-P OBJECT)
	(PRINT-NAMED-OBJECT OBJECT OUTPUT-STREAM)
	(PRINT-ANONYMOUS-OBJECT OBJECT OUTPUT-STREAM MEMORY))))

;;; the presentation actions

;;; describe

(SCL:DEFINE-PRESENTATION-ACTION DESCRIBE-OBJECT
   (OBJECT T :DOCUMENTATION "Describe CommonORBIT object"
		  :GESTURE :HYPER-MIDDLE)
   (OBJECT &KEY WINDOW)
  (SHOW OBJECT WINDOW))

(SCL:DEFINE-PRESENTATION-ACTION DESCRIBE-NAMED-OBJECT
   (NAMED-OBJECT T :DOCUMENTATION "Describe CommonORBIT object"
		  :GESTURE :HYPER-MIDDLE)
   (OBJECT &KEY WINDOW)
  (SHOW OBJECT WINDOW))

;;; graph proxies

(SCL:DEFINE-PRESENTATION-ACTION GRAPH-PROXIES
   (OBJECT T :DOCUMENTATION "Graph proxies"
		  :GESTURE :HYPER-LEFT)
   (OBJECT &KEY WINDOW)
  (GRAPH-PROXIES OBJECT :STREAM WINDOW))

(SCL:DEFINE-PRESENTATION-ACTION NAMED-GRAPH-PROXIES
   (NAMED-OBJECT T :DOCUMENTATION "Graph proxies"
		  :GESTURE :HYPER-LEFT)
   (OBJECT &KEY WINDOW)
  (GRAPH-PROXIES OBJECT :STREAM WINDOW))

;;; graph clients

(SCL:DEFINE-PRESENTATION-ACTION GRAPH-NAMED-CLIENTS
   (OBJECT T :DOCUMENTATION "Graph named clients"
		  :GESTURE :HYPER-RIGHT)
   (OBJECT &KEY WINDOW)
  (GRAPH-NAMED-CLIENTS OBJECT :STREAM WINDOW))

(SCL:DEFINE-PRESENTATION-ACTION NAMED-GRAPH-NAMED-CLIENTS
   (NAMED-OBJECT T :DOCUMENTATION "Graph named clients"
		  :GESTURE :HYPER-RIGHT)
   (OBJECT &KEY WINDOW)
  (GRAPH-NAMED-CLIENTS OBJECT :STREAM WINDOW))

;;; erasing

(SCL:DEFINE-PRESENTATION-ACTION ERASE-THIS-OBJECT
   (OBJECT T :DOCUMENTATION "Erase this object"
		  :GESTURE :CONTROL-META-MIDDLE)
   (OBJECT)
  (ERASE-OBJECT OBJECT))

(SCL:DEFINE-PRESENTATION-ACTION NAMED-ERASE-THIS-OBJECT
   (NAMED-OBJECT T :DOCUMENTATION "Erase this object"
		  :GESTURE :CONTROL-META-MIDDLE)
   (OBJECT)
  (ERASE-OBJECT OBJECT))

;;; the graphs

(DEFUN GRAPH-PROXIES (OBJECT
		      &KEY (STREAM *STANDARD-OUTPUT*) (TEST #'OQL)
		      (ORIENTATION :VERTICAL))
  "Display a graph of all the proxies of an object."
  (FRESH-LINE)(TERPRI)
  (SCL:FORMAT-GRAPH-FROM-ROOT
    (EOBJECT OBJECT)			; root object
    #'(LAMBDA (OBJECT STREAM)		; object printer
	(SCL:WITH-CHARACTER-STYLE (*CORBIT-CHARACTER-STYLE*
				    STREAM :BIND-LINE-HEIGHT T)
	  (SCL:PRESENT OBJECT 'OBJECT
		       :STREAM STREAM)))
    #'(LAMBDA (OBJECT)			; inferior producer
	(OBJECT-PROXIES OBJECT))	; object-proxies is a macro
    :STREAM STREAM :DONT-DRAW-DUPLICATES T :TEST TEST
    :ORIENTATION ORIENTATION :DIRECTION :BEFORE
    :DEFAULT-DRAWING-MODE :ARROW :BORDER NIL
    :COLUMN-SPACING (ECASE ORIENTATION
		      (:VERTICAL 10)
		      (:HORIZONTAL 30)))
  OBJECT)

(DEFUN GRAPH-NAMED-CLIENTS (OBJECT
			    &KEY (STREAM *STANDARD-OUTPUT*) (TEST #'OQL)
			    (ORIENTATION :VERTICAL))
  "Display a graph of all the named clients of an object."
  (FRESH-LINE)(TERPRI)
  (SCL:FORMAT-GRAPH-FROM-ROOT
    (EOBJECT OBJECT)			; root object
    #'(LAMBDA (OBJECT STREAM)		; object printer
	(SCL:WITH-CHARACTER-STYLE (*CORBIT-CHARACTER-STYLE*
				    STREAM :BIND-LINE-HEIGHT T)
	  (SCL:PRESENT OBJECT 'OBJECT
		       :STREAM STREAM)))
    #'NAMED-CLIENTS			; inferior producer
    :STREAM STREAM :DONT-DRAW-DUPLICATES T :TEST TEST
    :ORIENTATION ORIENTATION :DIRECTION :AFTER
    :DEFAULT-DRAWING-MODE :ARROW :BORDER NIL
    :COLUMN-SPACING (ECASE ORIENTATION
		      (:VERTICAL 10)
		      (:HORIZONTAL 30)))
  OBJECT)
