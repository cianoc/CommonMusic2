;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.40 $
;;; $Date: 2006/07/02 12:09:01 $

(in-package :cm)

(defgeneric fill-object (obj1 obj2))
(defgeneric rename-object (obj1 name &rest args))
(defgeneric object-name (obj)
  (:method ((object t)) (class-name (class-of obj))))
(defgeneric object-time (obj)
    (:method ((obj t)) obj 0))

(defgeneric copy-object (obj))

;;; Move these into a proper interfaces file, or something.

(defgeneric class-parameters (obj))
(defgeneric (setf class-parameters) (val obj))
(defgeneric io-class-file-types (obj))
(defgeneric (setf io-class-file-types) (val obj))
;(defgeneric io-class-mime-type (obj))
;(defgeneric (setf io-class-mime-type) (val obj))
(defgeneric io-class-output-hook (obj))
(defgeneric (setf io-class-output-hook) (val obj))
(defgeneric io-class-definer (obj))
(defgeneric (setf io-class-definer) (val obj))
(defgeneric io-class-file-versions (obj))
(defgeneric (setf io-class-file-versions) (val obj))
;(defgeneric validate-superclass (obj1 obj2))
(defgeneric make-load-form (obj))

(defgeneric subcontainers (obj))

(defgeneric insert-object (obj1 obj2))
(defgeneric append-object (obj1 obj2))
(defgeneric remove-object (obj1 obj2))
(defgeneric remove-subobjects (obj))
;(defgeneric list-subobjects (obj  &key start end start-time end-time))
(defgeneric amplitude (amp &optional softest loudest power))
(defgeneric rhythm (rhy &optional tempo beat))
(defgeneric tuning->mode (mode keynum force?))
(defgeneric mode->tuning (mode keynum return))
(defgeneric tuning-hertz->keynum (obj hz))
(defgeneric tuning-hertz->note (scale hz acci err?))
(defgeneric tuning-keynum->hertz (obj knum))
(defgeneric tuning-keynum->note (obj knum acci err?))
(defgeneric tuning-note->hertz (obj note err?))
(defgeneric tuning-note->keynum (obj note err?))
(defgeneric tuning-note->note (obj note acci err?))
(defgeneric scale-mod (freq modulus &key offset in accidental))
(defgeneric note-in-scale? (note scale))
(defgeneric transpose (note int &optional scale))
(defgeneric invert (ref &optional pc?))
(defgeneric interval (int &optional int2))

(defgeneric canonicalize-pattern-data (obj data parser inits))
(defgeneric pattern-external-inits (obj))
(defgeneric pattern-period-length (obj))
(defgeneric default-period-length (obj))
(defgeneric pattern? (obj))
(defgeneric eop? (obj))
(defgeneric eod? (obj))
(defgeneric next-1 (obj))
(defgeneric skip-datum? (obj))
(defgeneric reset-period (obj))
(defgeneric next-in-pattern (obj))
(defgeneric map-pattern-data (fn obj))


(defgeneric write-event (obj io time))
(defgeneric import-events (obj &rest args))
