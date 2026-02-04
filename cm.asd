;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.38 $
;;; $Date: 2007/01/05 21:46:36 $

(in-package :cl-user)

#-(or allegro clisp cmu lispworks openmcl sbcl ecl)
(error "Sorry, Common Music does not run in this Lisp.")

;;;
;;; system definition
;;;
(asdf:defsystem :cm
  :description "Common Music"
  :version "2.7.0"
  :author "Rick Taube <taube (at) uiuc.edu>"
  :licence "LLGPL"
  :depends-on ("alexandria" "serapeum" "trivial-do" "closer-mop")
  :components
  ((:module "src"
    :components (
		 (:file "package")
		 (:file "iter")
		 (:file "interface" :depends-on ("package"))
		 (:file "level1" 
		  :depends-on ("package" "interface"
				     "iter"))
		 (:file "clos" :depends-on ("level1"))
		 (:file "utils/utils" :depends-on ("level1"))
		 (:file "mop" :depends-on ("clos" "utils/utils"))
		 (:file "objects" :depends-on ("mop" "iter" "utils/utils"))
		 (:file "data" :depends-on ("utils/utils"))
		 (:file "scales" :depends-on ("data" "objects"))
		 (:file "spectral" :depends-on ("data"))
		 (:file "patterns" :depends-on ("scales"))))))
