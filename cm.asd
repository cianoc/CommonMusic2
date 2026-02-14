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

;;;
;;; system definition
;;;
(asdf:defsystem :cm
  :description "Common Music"
  :version "3.0"
  :author "Rick Taube <taube (at) uiuc.edu>"
  :licence "LLGPL"
  :depends-on ("alexandria" "serapeum" "trivial-do"
               "closer-mop" "transducers" "str"
               "trivia")
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "interface")
             (:file "objects")
             (:module "scales"
              :components
              ((:file "package")
               (:file "interface")
               (:file "utils")
               (:file "objects")
               (:file "intervals")
               (:file "tuning")
               (:file "scales")
               (:file "mode")))
             (:module "utils"
                       :components
                       ((:file "objects")))))))
