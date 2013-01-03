;;; brownian-loop.lisp

(in-package #:brownian)

(defclass brownian-loop (game-loop)
  ((particles :accessor particles :initform '())
   (particle-speed :accessor particle-speed :initarg :particle-speed 
		   :initform 64.0)
   (particle-radius :accessor particle-radius :initarg :particle-radius
		    :initform 16)
   (font :accessor font :initform nil)
   (text-color :accessor text-color :initarg :text-color :initform sdl:*white*)
   (use-averaging :accessor use-averaging :initarg :use-averaging :initform t)
   (averaging-alpha :accessor averaging-alpha :initarg :averaging-alpha 
		  :initform 0.25)
   (show-stats :accessor show-stats :initform nil)))

(defmethod initialize-data ((game brownian-loop))
  (setf *random-state* (make-random-state t))
  (sdl-ttf:init-ttf)
  (let* ((arial-path (merge-pathnames "arial.ttf"
				      (asdf:system-source-directory :brownian)))
	 (arial-16 (make-instance 'sdl:ttf-font-definition :size 16
				  :filename arial-path)))
    (setf (font game) (sdl:initialise-font arial-16))
    (if (font game)
	(sdl:set-font-style :style-bold :font (font game))
	(error "Can't load font."))))


(defmethod update ((game game-loop) elapsed-time)
  (mapc (lambda (p) (update p elapsed-time)) (particles game)))

(defmethod render ((game game-loop))
  (mapc #'render (particles game))
  (when (show-stats game)
    (render-statistics game)))

(defmethod end-loop ((game game-loop))
  (sdl::close-font (font game))
  (sdl-ttf:quit-ttf)
  (sdl:quit-sdl))

(defmethod window-active ((game game-loop))
  (declare (ignore game)))

(defmethod window-inactive ((game game-loop))
  (declare (ignore game)))

(defmethod key-down ((game game-loop) key-code)
  (with-slots (background-color text-color particles show-stats) game
    (cond
      ((sdl:key= key-code :sdl-key-a)
       (setf (use-averaging game) (not (use-averaging game))))
      ((sdl:key= key-code :sdl-key-b)
       (cond
	 ((eq background-color sdl:*black*)
	  (setf background-color sdl:*white*)
	  (setf text-color sdl:*black*))
	 (t
	  (setf background-color sdl:*black*)
	  (setf text-color sdl:*white*))))
      ((sdl:key= key-code :sdl-key-r)
       (setf particles '()))
      ((sdl:key= key-code :sdl-key-s)
       (setf show-stats (not show-stats)))
      ((sdl:key= key-code :sdl-key-1)
       (with-slots ((r particle-radius)) game
	 (decf r)
	 (when (< r 2)
	   (setf r 2))))
      ((sdl:key= key-code :sdl-key-2)
       (with-slots ((r particle-radius)) game
	 (incf r)
	 (when (> r 128)
	   (setf r 128))))
      ((sdl:key= key-code :sdl-key-3)
       (with-slots ((s particle-speed)) game
	 (decf s)
	 (when (< s 2.0)
	   (setf s 2.0))))
      ((sdl:key= key-code :sdl-key-4)
       (with-slots ((s particle-speed)) game
	 (incf s)
	 (when (> s 128.0)
	   (setf s 128.0))))
      ((sdl:key= key-code :sdl-key-5)
       (with-slots ((aa averaging-alpha)) game
	 (decf aa 0.01)
	 (when (< aa 0.0)
	   (setf aa 0.0))))
      ((sdl:key= key-code :sdl-key-6)
       (with-slots ((aa averaging-alpha)) game
	 (incf aa 0.01)
	 (when (> aa 1.0)
	   (setf aa 1.0))))
      (t nil))))

(defmethod key-up ((game game-loop) key-code)
  (with-slots ((aa averaging-alpha) particles) game
    (cond
      ((or (sdl:key= key-code :sdl-key-5)
	   (sdl:key= key-code :sdl-key-6))
       (mapc (lambda (p) (setf (alpha (averager p)) aa)) particles)))))

(defmethod mouse-moved ((game game-loop) button-state x y dx dy)
  (declare (ignore game button-state x y dx dy)))

(defmethod mouse-button-down ((game game-loop) button x y)
  (if (= button 1)
      (add-particle game x y)
      (dotimes (i 100)
	(add-particle game x y))))

(defmethod mouse-button-up ((game game-loop) button x y)
  (declare (ignore game button x y)))

(defun render-statistics (game)
  (declare (type brownian-loop game))
  (with-slots (particles particle-radius particle-speed use-averaging
			 averaging-alpha current-fps window-height font 
			 text-color) game
    (let ((stat-text 
	   (format nil 
		   "~a particles, radius ~a, speed ~a, averaging ~a, alpha ~,2f, ~a fps" 
		   (length particles) particle-radius particle-speed 
		   use-averaging averaging-alpha current-fps)))
      (sdl:draw-string-blended-* stat-text 16 (- window-height 20) 
				 :font font :color text-color))))

(defun add-particle (game x y)
  (declare (type brownian-loop game))
  (let ((border-color (random-base-color)))
    (with-slots ((r particle-radius) (s particle-speed)) game
      (pushnew (make-particle x y r s border-color game) (particles game)))))
