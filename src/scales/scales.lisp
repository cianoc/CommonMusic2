(in-package :cm/scales)

(defvar *scales* (make-hash-table))

;; (defmethod degree-ratio ((scale-steps scale-degree-steps) degree-num)
;;   (with-slots (steps) scale-steps
;;     (* (expt 2 (/ degree-num 12)))))

;; (defmethod degree-ratio ((scale-cents scale-degree-cents) degree-num)
;;   (with-slots (table-cents) scale-cents
;;     (cents->scaler (gethash degree-num table-cents))))

;; (defmethod degree-ratio ((scale-ratio scale-degree-ratio) degree-num)
;;   (with-slots (table-ratio) scale-ratio
;;     (if (floatp degree-num)
;;         (let ((lo (gethash (floor degree-num) table-ratio))
;;               (hi (gethash (ceiling degree-num) table-ratio)))
;;           (- hi lo))
;;         (gethash degree-num table-ratio))))

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

                  
;; used to convert lists where the difference is provided
;; rather than exact values for each interval
(defun convert-ratios-list (fn init-value lst)
  (let ((vec (t:transduce (t:scan fn init-value)  #'t:vector lst)))
    (subseq vec 0 (- (length vec) 1))))

;; (convert-ratios-list #'+ 0 #(100 100 100 100 100))

;; Degree num is the number to check for the first degree to see
;; if it is relative to bottom, or ratio between degrees.
;; degree-fn does the conversion if it is ratios between degrees.

;; Allows us to cleanly handle ratios and cents in a single function
;; when creating a tuning.
(defun get-setup-data (lst degree-num degree-fn)
  (let ((idx) (names))
    (if (listp (car lst))
        (progn
          (setf idx (mapcar #'first lst))
          (setf names (mapcar #'cdr lst)))
        (setf idx lst))
    (setf idx
          (coerce
           (if (/= degree-num (car idx))
               (convert-ratios-list degree-fn degree-num idx)
               idx)
           'vector))
    (when names (setf names (construct-note-names names)))
    (values idx names)))

;; (get-note-details '((100 (c) (cn :accidental n) (bs :accidental s :octave -1)
;;                  (dff :accidental ff))
;;           (100 (cs :accidental s) (df :accidental f)
;;            (bss :accidental ss :octave -1))
;;           (100 (d) (dn :accidental n) (css :accidental ss)
;;            (eff :accidental ff))
;;           (100 (ef :accidental f) (ds :letter d :accidental s)
;;            (fff :letter f :accidental ff))
;;           (100 (e) (en :accidental n) (ff :accidental f)
;;            (dss :accidental ss))
;;           (100 (f) (fn :accidental n) (es :accidental s)
;;            (gff :accidental ff))
;;           (100 (fs :accidental s) (gf :accidental f)
;;            (ess :accidental ss))
;;           (100 (g) (gn :accidental n) (fss :accidental ss)
;;            (aff :accidental ff))
;;           (100 (af :accidental f) (gs :accidental s))
;;           (100 (a) (an :accidental n) (gss :accidental ss)
;;            (bff :accidental ff))
;;           (100 (bf :accidental f) (as :accidental s)
;;            (cff :accidental ff :octave 1))
;;           (100 (b) (bn :accidental n) (cf :accidental f :octave 1)
;;            (ass :accidental ss)))
;;               0 #'+)

(defstruct scale
  name ;;
  octaves ;;
  octave-start
  octave-end
  (octave-width 2 :type fixnum) ;;
  lowest-hz ;;
  scale-steps ;;
  (keynum-offset 0 :type fixnum) ;;
  (default-octave 4 :type fixnum) ;;
  degree-ratio ;;
  degree->degree-name ;; vector DONE
  degree-name->degree ;; hashmap DONE
  keynum->freq ;; vector DONE
  keynum->note-name ;; vector DONE
  note-name->hz ;; hashmap
  note-name->keynum ;; hashmap
  )

(defun cents->ratios (cents)
  (t:transduce (t:map #'cents->scaler) #'t:vector cents))

(defun steps->ratios (steps octave-width)
  (t:transduce (t:comp
                (t:take steps)
                (t:map (lambda (x) (expt octave-width
                                                 (/ x steps)))))
               #'t:vector
               (t:ints 0)))

(defun scale (&key name (lowest-hz 8.175) cents ratios steps
              (octave-width 2) (octaves 10) (keynum-offset 0)
              (default-octave 4))
  (let ((degree-ratio) (note-name-vec)
        (start-octave) (end-octave)
        (num-notes) 
        (keynum->freq) (keynum->note-name)
        (degree->degree-name) (degree-name->degree)
        (note-name->hz) (note-name->keynum))
    ;; get the configuration data from cents, ratios, etc
    (cond (cents
           (multiple-value-bind (x-cents x-notes)
               (get-setup-data cents 0 #'+)
             (setf degree-ratio (cents->ratios x-cents))
             (setf note-name-vec x-notes)))
          (ratios
           (multiple-value-bind (x-ratios x-notes)
               (get-setup-data ratios 0 #'+)
             (setf degree-ratio x-ratios)
             (setf note-name-vec x-notes))
           )
          (steps (setf degree-ratio
                       (steps->ratios steps octave-width))))

    ;; setup octave data
    (cond ((null octaves)
           (setf num-notes (length degree-ratio)))
          ((listp octaves)
           (progn
             (setf steps (length degree-ratio))
             (setf start-octave (car octaves))
             (setf end-octave (cadr octaves))
             (setf octaves (- end-octave start-octave))
             (setf num-notes (* steps
                                (- end-octave start-octave)))))
          (t
           (progn
             (setf steps (length degree-ratio))
             (setf start-octave 0)
             (setf end-octave octaves)
             (setf num-notes (* steps octaves)))))

    ;; setup keynum->freq    
    (setf keynum->freq (make-array num-notes))
    (if octaves
        (loop for i :from 0 :below num-notes
              do (multiple-value-bind (octave degree)
                     (floor i steps)
                   (setf (aref keynum->freq i)
                         (* lowest-hz
                            (* (expt octave-width octave)
                               (aref degree-ratio degree))))))
        (loop for i :from 0 :below num-notes
              do (setf (aref keynum->freq i)
                       (* lowest-hz
                          (aref degree-ratio i)))))

    ;; if note names were passed in, then set them up
    (when note-name-vec
      (setf degree->degree-name note-name-vec)

      (setf degree-name->degree (make-hash-table))
      (loop for ns across note-name-vec
            for i :from 0
            do (loop for n across ns
                     do (setf (gethash (degree-note-name-note n)
                                       degree-name->degree)
                              i)
                     ))

      (setf keynum->note-name (make-array num-notes))

      (if octaves
          (loop for i :from 0 :below num-notes
                do (setf
                    (aref keynum->note-name i)
                          (let ((oct (+ start-octave
                                        (floor i steps)))
                                (name
                                  (aref degree->degree-name
                                        (mod i steps))))
                            (t:transduce
                             (t:map (lambda (x)
                                      (make-scale-note-name
                                       :note
                                       (degree-note-name-note x)
                                       :base
                                       (degree-note-name-base x)
                                       :accidental
                                       (degree-note-name-accidental x)
                                       :octave oct
                                       )))
                             #'t:vector
                             name))))
          (setf keynum->note-name degree->degree-name))

      (setf note-name->keynum (make-hash-table))
      (setf note-name->hz (make-hash-table))
      (loop for ns across keynum->note-name
            for k from 0
            do (loop with note-str
                     for n :across ns
                     do (setf note-str
                              (if octaves
                                  (intern
                                   (str:concat
                                    (string (degree-note-name-note n))
                                    (write-to-string
                                     (scale-note-name-octave n))))
                                  (degree-note-name-note n)))
                     do (setf (gethash note-str note-name->keynum)
                              k)
                     do (setf (gethash note-str note-name->hz)
                              (aref keynum->freq k)))))
    (unless octaves
      (setf degree->degree-name nil)
      (setf degree-name->degree nil))
    (let ((scale
           (make-scale :name name
                       :octave-width octave-width
                       :octave-start start-octave
                       :octave-end end-octave
                       :octaves octaves
                       :lowest-hz lowest-hz
                       :scale-steps steps
                       :default-octave default-octave
                       :degree-ratio degree-ratio
                       :degree->degree-name degree->degree-name
                       :degree-name->degree degree-name->degree
                       :keynum->freq keynum->freq
                       :keynum->note-name keynum->note-name
                       :note-name->hz note-name->hz
                       :keynum-offset keynum-offset
                       :note-name->keynum note-name->keynum)))
      (when name
        (setf (gethash name *scales*)
              scale))
      scale)))


(defparameter *chromatic-scale*
  (scale
   :name "chromatic-scale"
   :octaves '(-1 10)
   :lowest-hz 8.18
   :keynum-offset 3
   :default-octave 4
   :cents
   '((100 (c) (cn :accidental n) (bs :accidental s :octave -1)
      (dff :accidental ff))
     (100 (cs :accidental s) (df :accidental f)
      (bss :accidental ss :octave -1))
     (100 (d) (dn :accidental n) (css :accidental ss)
      (eff :accidental ff))
     (100 (ef :accidental f) (ds :letter d :accidental s)
      (fff :letter f :accidental ff))
     (100 (e) (en :accidental n) (ff :accidental f)
      (dss :accidental ss))
     (100 (f) (fn :accidental n) (es :accidental s)
      (gff :accidental ff))
     (100 (fs :accidental s) (gf :accidental f)
      (ess :accidental ss))
     (100 (g) (gn :accidental n) (fss :accidental ss)
      (aff :accidental ff))
     (100 (af :accidental f) (gs :accidental s))
     (100 (a) (an :accidental n) (gss :accidental ss)
      (bff :accidental ff))
     (100 (bf :accidental f) (as :accidental s)
      (cff :accidental ff :octave 1))
     (100 (b) (bn :accidental n) (cf :accidental f :octave 1)
      (ass :accidental ss)))))
    
    


(defun hz->keynum (hz scale)
  (with-slots (lowest-hz octave-width keynum-offset degree-ratio) scale
    (let* ((octave (floor (log (/ hz lowest-hz) octave-width)))
           (deg-ratio (/ hz (expt octave octave-width) lowest-hz))
           (degree (loop with ratio = 1
                         for x across degree-ratio
                         if (< x deg-ratio)
                           do (loop-finish)
                         do (setf ratio x)
                         finally (return ratio))))
      (- (+ (* octave-width octave)
            degree)
         keynum-offset))))

(defun keynum->note-name (keynum scale)
  (with-slots (keynum-note-name) scale
    (aref keynum-note-name keynum)))

(defun hz->note-name (hz scale)
  (keynum->note-name (hz->keynum hz scale) scale))

(defun degree->keynum (degree octave scale)
  (with-slots (keynum-offset scale-steps) scale
    (+ (* octave scale-steps) (- degree keynum-offset))))

;; returns a value with 2 slots: octave & degree
(defun keynum->degree (keynum scale)
  (with-slots (keynum-offset scale-steps) scale
    (floor (- keynum keynum-offset) scale-steps)))

(defun keynum->hz (keynum scale)
  (with-slots (keynum-freq) scale
    (if (floatp keynum)
        (between-hz
         (aref keynum-freq (floor keynum))
         (aref keynum-freq (ceiling keynum)))
        (aref keynum-freq keynum))))

(defun degree->hz (degree octave scale)
  (keynum->hz (degree->keynum degree octave scale) scale))

(defun degree->note-name (degree octave scale)
  (keynum->note-name (degree->keynum degree octave scale) scale))

(defun degree->degree-name (degree scale)
  (with-slots (degree->degree-name) scale
    (aref degree->degree-name degree)))

(defun degree-name->degree (degree-name scale)
  (with-slots (degree-name->degree) scale
    (gethash degree-name degree-name->degree)))

(defun note-name->hz (note-name scale)
  (with-slots (note-name->hz) scale
    (gethash note-name scale)))

(defun note-name->keynum (note-name scale))

