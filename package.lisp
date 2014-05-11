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


(cl:defpackage "CORBIT-TEST"
  (:use "CL" "CORBIT"))
