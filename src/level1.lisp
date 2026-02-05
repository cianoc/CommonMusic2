;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.28 $
;;; $Date: 2006/10/21 14:58:54 $

(in-package :cm)

;;;
;;; functionality i had to add to scheme that is not exactly
;;; defined in cltl either...
;;;

(defun log2 (n) (log n 2))

(defun log10 (n) (log n 10))

(defmacro define-list-struct (name &body slots)
  (let ((setters
         (loop for x in slots
               for s = (if (consp x) (car x) x)
               collect
               `(defun ,(intern (concatenate 'string 
                                             (string name)
                                             "-"
                                             (string s)
                                             "-"
                                             ;; case sensitivity
                                             (symbol-name 'set!)))
                       (,name value)
                  (setf (,(intern (concatenate 'string 
                                               (string name)
                                               "-"
                                               (string s)))
                         ,name)
                        value)))))
    `(progn
       (defstruct (,name (:type list))
         ,@slots)
       ,@setters)))

;;;
;;; readtable hackery
;;;

(defvar *cm-readtable* (copy-readtable))

(defun read-macro-set! (char func)
  (set-dispatch-macro-character 
   #\# char #'(lambda (stream a b) 
                (declare (ignore a b))
                (funcall func (read stream)))
   *cm-readtable*))

(read-macro-set! #\! (lambda (form) `(find-object ',form t)))

;;;
;;; hash-table
;;;

(defun hash-fold (func prev table)
  (maphash #'(lambda (key val)
               (setf prev (funcall func key val prev)))
           table)
  prev)

;;;
;;; symbols and keywords
;;;

(defun keyword? (x) (typep x 'keyword))

(defun symbol->keyword (sym)
  (let ((str (symbol-name sym)))
    (or (find-symbol str ':keyword)
        (intern str :keyword))))

(defun keyword->symbol (keyword)
  (let ((name (symbol-name keyword)))
    (or (find-symbol name)
        (intern name))))

(defun keyword->string (kw)
  (format nil "~(~A~)" kw))

(defun string->keyword (s)
  (intern s :keyword))

;;; strings

(defun strip-chars (str &optional (chars '(#\space #\tab #\return)))
  (string-trim chars str))

(defun string-read (str &optional (start 0) (eof ':eof))
  (read-from-string str nil eof :start start))

;;;
;;; filename twiddling. these always return strings.
;;;

(defun filename (file) (namestring file))

(defun filename-directory (file)
  (let ((dir (pathname-directory file)))
    (if dir
      (namestring (make-pathname :directory dir))
      nil)))

(defun filename-name (file) (pathname-name file))

(defun filename-type (file) (pathname-type file))


;;;
;;; file opening and closing.
;;;

(defun open-file (file direction &optional (type :char))
  (let ((etyp (ecase type
                ((:byte :byte8 ) '(unsigned-byte 8))
                ((:byte32 ) '(unsigned-byte 32))
                ((:char ) 'character))))
    (if (eq direction :output)
      (open file :direction :output
            :if-does-not-exist :create
            :if-exists :supersede
            :element-type etyp)
      (open file :direction :input
            :element-type etyp))))

(defun close-file (fp dir)
  (declare (ignore dir))
  (close fp))

(defvar .eofmarker. (gensym))

(defun file-eof? (x) (eq x .eofmarker.))

(defun file-form (fil)
  (read fil nil .eofmarker.))

(defun file-line (fil)
  (read-line fil nil .eofmarker.))

(defun file-byte (fil)
  (read-byte fil nil .eofmarker.))

;(defun open-output-file (file)
;  (open file :direction :output
;        :if-does-not-exist :create
;        :if-exists :supersed))
;
;(defun open-input-file (file)
;  (open file :direction :input))
;
;(defmacro with-open-output-file ((var file) &body body)
;  `(with-open-file (,var ,file :direction :output
;                         :if-does-not-exist :create
;                         :if-exists :supersed)
;     ,@body))
;
;(defmacro with-open-input-file ((var file) &body body)
;  `(with-open-file (,var ,file :direction :input)
;     ,@body))



;;;
;;; cltl expansion for make-load-form
;;;

(defun make-load-form-method (classname classvar)
  `(defmethod make-load-form ((obj ,classname))
     (list* 'make-instance ',classvar
            (slot-init-forms obj :eval t))))
