(in-package :cm/scales)

(defstruct degree-note-name
  note
  base
  accidental)

(defstruct (scale-note-name (:include degree-note-name))
  octave)

(defun construct-note-names (lst)
  (coerce 
   (loop for val in lst
         collect (coerce
                  (loop with name with note with keys
                        with base-name with accidental
                        for x in val
                        do (setf note (a:ensure-list x))
                        do (setf name (car note))
                        do (setf keys (cdr note))
                        do (setf accidental (getf keys :accidental))
                        do (setf base-name
                                 (when accidental
                                   (intern (subseq (string name)
                                                   0
                                                   (- (length (string name))
                                                      (length (string accidental)))))))
                        collect (make-degree-note-name :note name
                                                       :base base-name
                                                       :accidental accidental))
                  'vector))
   'vector))

;; (defstruct interval
;;   size
;;   quality)

;; (defun interval-size-notes (arg1 arg2))

;; (defun interval-between (arg1 arg2))

;; (defun interval (arg1 arg2 &key (scale *default-scale*))
;;   (cond ((keywordp arg2)
;;          (interval-size arg1 arg2))
;;         ())
;;   )

;; (defmethod semitones ((interval interval)))
