;;; particle.lisp

(in-package #:brownian)

;; Helper functions to provide randomness

(defun random-heading ()
  "Returns a random heading between 0 and 2*PI radians."
  (* 2 pi (random 1.0d0)))

(defun random-heading-change (&optional (range (/ pi 4)))
  (+ (- (/ range 2.0)) (* (random 1.0d0) range)))

(defun random-base-color ()
  (let ((color-index (random 8)))
    (cond
      ((= color-index 0) sdl:*black*)
      ((= color-index 1) sdl:*red*)
      ((= color-index 2) sdl:*green*)
      ((= color-index 3) sdl:*blue*)
      ((= color-index 4) sdl:*yellow*)
      ((= color-index 5) sdl:*cyan*)
      ((= color-index 6) sdl:*magenta*)
      ((= color-index 7) sdl:*white*))))


;; Particle class and methods

(defclass particle ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (radius :accessor radius :initarg :radius :initform 16)
   (heading :accessor heading :initarg :heading :initform (random-heading))
   (border-color :accessor border-color :initarg :border-color
		 :initform sdl:*white*)
   (fill-color :accessor fill-color :initarg :fill-color 
	       :initform sdl:*white*)
   (particle-speed :accessor particle-speed :initarg :particle-speed 
		   :initform 64.0)
   (world :reader world :initarg :world 
	  :initform (error "Must specify particle's world."))
   (averager :reader averager 
	     :initform (make-instance 'averager :alpha 0.25))))

(defun make-particle (x y radius particle-speed border-color world)
  (let* ((fill-color (lighten-color border-color 0.5))
	 (heading (random-heading))
	 (p (make-instance 'particle :x x :y y :radius radius 
			   :particle-speed particle-speed :heading heading 
			   :border-color border-color :fill-color fill-color
			   :world world)))
    (setf (average (averager p)) heading)
    p))

(defmethod update ((p particle) elapsed-time)
  (with-slots (x y radius heading particle-speed world) p
    (let ((dheading (random-heading-change)))
      (let* ((new-heading (+ heading dheading))
	     (dx (* particle-speed (cos new-heading) elapsed-time))
	     (dy (* particle-speed (sin new-heading) elapsed-time)))
	(if (use-averaging world)
	    (setf heading (update (averager p) new-heading))
	    (setf heading new-heading))
	(setf x (+ x dx))
	(setf y (+ y dy))
	(cond 
	  ((< x (- radius))
	   (setf x (1- (+ (window-width world) radius))))
	  ((>= x (+ (window-width world) radius))
	   (setf x (- radius))))
	(cond 
	  ((< y (- radius))
	   (setf y (1- (+ (window-height world) radius))))
	  ((>= y (+ (window-height world) radius))
	   (setf y (- radius))))))))

  (defmethod render ((p particle))
    (with-slots (x y radius heading fill-color border-color) p
      (let ((x-int (round x))
	    (y-int (round y))
	    (x1 (round (+ x (* radius (cos heading)))))
	    (y1 (round (+ y (* radius (sin heading))))))
	(sdl:draw-circle-* x-int y-int radius :color border-color :aa t)
	(sdl:draw-filled-circle-* x-int y-int radius :color fill-color)
	(sdl:draw-line-* x-int y-int x1 y1 :color border-color :aa t))))
  
  
