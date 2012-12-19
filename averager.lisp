;;; averager.lisp
;;; Takes a stream of values and keeps a running total.

(in-package #:brownian)

(defclass averager ()
  ((alpha :reader alpha :initarg :alpha :type 'float)
   (average :accessor average :initform 0.0)))

(defgeneric update (averager number)
  (:documentation "Adds NUMBER to the current running average and updates
  the average."))

(defmethod update ((a averager) number)
  (with-slots (alpha average) a
    (setf average (+ (* alpha number) (* (- 1 alpha) average)))))
