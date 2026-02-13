(in-package :cm)

(defparameter *dictionary* (make-hash-table :size 31 :test #'equal))

(defclass container ()
  ((name :initform nil :accessor object-name :initarg :name)))

(defmethod print-object ((obj container) port)
  (let ((name (object-name obj)) (*print-case* ':downcase))
    (if name
        (format port "#<~a \"~a\">" (class-name (class-of obj)) name)
      (call-next-method))))


