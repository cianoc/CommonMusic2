(in-package :cm)

(defmethod copy-object ((obj t))
  (let ((new (make-instance (class-of obj))))
    (fill-object new obj)
    new))

(defmethod fill-object ((new t) (old t))
  (dolist (s (closer-mop:class-slots (class-of old)))
    (let ((n (closer-mop:slot-definition-name s)))1
      (when (and (slot-exists-p new n) (slot-boundp old n))
        (setf (slot-value new n) (slot-value old n))))))

(defun save-object (obj file)
  (let ((fp nil))
    (unwind-protect
        (progn (setf fp (open-file file :output))
               (if (consp obj)
                   (dolist (o obj)
                     (write (make-load-form o) :stream fp))
                   (write (make-load-form obj) :stream fp)))
      (if fp (close-file fp :output)))
    file))
