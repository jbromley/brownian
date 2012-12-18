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
   (show-stats :accessor show-stats :initform nil)))

(defmethod initialize-data ((game brownian-loop))
  (sdl-ttf:init-ttf)
  (let ((arial-16 (make-instance 'sdl:ttf-font-definition :size 16
				 :filename (merge-pathnames "arial.ttf" *default-pathname-defaults*))))
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
      ((sdl:key= key-code :sdl-key-b)
       (setf background-color sdl:*black*)
       (setf text-color sdl:*white*))
      ((sdl:key= key-code :sdl-key-r)
       (setf particles '()))
      ((sdl:key= key-code :sdl-key-s)
       (setf show-stats (not show-stats)))
      ((sdl:key= key-code :sdl-key-w)
       (setf background-color sdl:*white*)
       (setf text-color sdl:*black*))
      ((sdl:key= key-code :sdl-key-1)
       (with-slots ((r particle-radius)) game
	 (decf r)
	 (when (< r 4)
	   (setf r 4))))
      ((sdl:key= key-code :sdl-key-2)
       (with-slots ((r particle-radius)) game
	 (incf r)
	 (when (> r 64)
	   (setf r 64))))
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
      (t nil))))

(defmethod key-up ((game game-loop) key-code)
  (declare (ignore game key-code)))

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
  (with-slots (particles current-fps window-height font text-color) game
    (let ((stat-text (format nil "~a particles, radius ~a, speed ~a, ~a fps" 
			     (length particles) (particle-radius game)
			     (particle-speed game) current-fps)))
      (sdl:draw-string-blended-* stat-text 16 (- window-height 18) 
				 :font font :color text-color))))

(defun add-particle (game x y)
  (declare (type brownian-loop game))
  (let ((border-color (random-base-color)))
    (with-slots ((r particle-radius) (s particle-speed)) game
      (pushnew (make-particle x y r s border-color game) (particles game)))))

(defun font-example ()
  (sdl:with-init ()
    (sdl:window 320 320 
		:title-caption "SDL-TTF Font Example" 
		:icon-caption "SDL-TTF Font Example")
    (setf (sdl:frame-rate) 30)
    (sdl:fill-surface sdl:*black* :surface sdl:*default-display*)
    (let ((ttf-arial-16 (make-instance 'sdl:ttf-font-definition :size 16
				       :filename (merge-pathnames "arial.ttf" *default-pathname-defaults*))))
      (unless (sdl:initialise-default-font ttf-arial-16)
	  (error "FONT-EXAMPLE: Cannot initialize the default font.")))
    (sdl:set-font-style :style-bold :font sdl:*default-font*)
    (let ((orange (sdl:color :r 255 :g 165 :b 0)))
      (sdl:draw-string-solid-* "Text UTF8 - Solid" 0 50
                             :color orange)
      (sdl:draw-string-shaded-* "Text UTF8 - Shaded" 0 150 sdl:*black* 
				orange)
      (sdl:draw-string-blended-* "Text UTF8 - Blended" 0 250
				 :color orange))

    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event ()
       (when (sdl:key-down-p :sdl-key-escape)
         (sdl:push-quit-event))))))
