;;; color-utils.lisp

(in-package #:brownian)

(defun lighten-color (color factor)
  (declare (type sdl:color color))
  (declare (type float factor))
  (let ((color-array (sdl:fp color))
	(alpha 255))
    (when (typep color 'sdl:color-a)
      (setf alpha (aref color-array 3)))
    (let ((r (* (aref color-array 0) factor))
	  (g (* (aref color-array 1) factor))
	  (b (* (aref color-array 2) factor))
	  (a (* alpha factor)))
      (sdl:color :r r :g g :b b :a a))))

	 
  
