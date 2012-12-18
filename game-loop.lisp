;;; game-loop
;;; Base class for a simple SDL game loop

(in-package #:brownian)

(defclass game-loop ()
  ((window-width :accessor window-width :initarg :window-width :initform 0)
   (window-height :accessor window-height :initarg :window-height :initform 0)
   (background-color :accessor background-color :initarg :background-color 
		     :initform sdl:*black*)
   (window-title :accessor window-title :initarg :window-title :initform "")
   (minimized :accessor minimized :initform nil)
   (surface :reader surface :initarg :surface)
   (fps-tick-counter :accessor fps-tick-counter :initform 0)
   (fps-counter :accessor fps-counter :initform 0)
   (current-fps :reader current-fps :initform 0))
  (:documentation "Base glass for an SDL-based game."))

(defgeneric initialize-data (game-loop)
  (:documentation "Initializes any game data not loaded at class creation."))

(defgeneric update (game-loop elapsed-time)
  (:documentation "Updates game state once per loop
   iteration. ELAPSED-TIME is the time since the last frame."))

(defgeneric render (game-loop)
  (:documentation "Renders the current game state. SURFACE is the
   surface to which game objects are rendered."))

(defgeneric end-loop (game-loop)
  (:documentation "Cleans up allocated data and resources."))

(defgeneric window-active (game-loop)
  (:documentation "Handles when a window becomes active, i.e. it
   receives the focus."))

(defgeneric window-inactive (game-loop)
  (:documentation "Handles when a window becomes inactive, i.e. it is
   minimized or loses the focus."))

(defgeneric key-down (game-loop key-code)
  (:documentation "Handles when a keyboard key has been
   pressed. KEY-CODE gives the pressed key."))

(defgeneric key-up (game-loop key-code)
  (:documentation "Handles when a keyboard key has been
   released. KEY-CODE gives the released key."))

(defgeneric mouse-moved (game-loop button x y dx dy)
  (:documentation "Handles when the mouse has been moved. BUTTON
   indicates which, if any, mouse button has been pressed. X and Y give
   the current coordinates of the mouse. DX and DY give the change in
   mouse coordinates since the last mouse event."))

(defgeneric mouse-button-down (game-loop button x y)
  (:documentation "Handles when a mouse button has been pressed. BUTTON
   indicates which, if any, mouse button has been pressed. X and Y give
   the current coordinates of the mouse. DX and DY give the change in
   mouse coordinates since the last mouse event."))

(defgeneric mouse-button-up (game-loop button x y)
  (:documentation "Handles when a mouse button has been
   released. BUTTON indicates which, if any, mouse button has been
   released. X and Y give the current coordinates of the mouse. DX and
   DY give the change in mouse coordinates since the last mouse
   event."))


;;; Invariant functions for game-loop classes


(defun initialize (game)
  (declare (type game-loop game))
  (cond ((equal (sdl:init-sdl :video t :audio t) t)
	 (sdl:window (window-width game) (window-height game) 
		     :sw t :double-buffer t 
		     :title-caption (window-title game)
		     :fps (make-instance 'sdl:fps-fixed))
;	 (setf (slot-value game 'surface) sdl:*default-surface*)
	 (setf (sdl:frame-rate) nil)
	 (initialize-data game))
	(t
	 (error "Error initialized SDL")
	 nil)))

(defun do-update (game)
  (declare (type game-loop game))
  (let ((elapsed-time (sdl:dt)))
    (update game elapsed-time)
    (setf (fps-tick-counter game) (+ (fps-tick-counter game) elapsed-time))))

(defun do-render (game)
  (with-slots (fps-counter fps-tick-counter current-fps) game
    (incf fps-counter)
    (when (>= fps-tick-counter 1)
      (setf current-fps fps-counter)
      (setf fps-counter 0)
      (setf fps-tick-counter 0)))
  (sdl:fill-surface (background-color game))
  (render game)
  (sdl:update-display))

(defun run (game)
  (declare (type game-loop game))
  (sdl:update-display)
  (sdl:with-events ()
    (:quit-event () t)
    (:video-expose-event () (sdl:update-display))
    (:key-down-event (:key key-code)
		     (if (sdl:key= key-code :sdl-key-escape)
			 (sdl:push-quit-event)
			 (key-down game key-code)))
    (:key-up-event (:key key-code)
		   (key-up game key-code))
    (:mouse-motion-event (:state button-state :x x :y y :x-rel dx :y-rel dy)
			 (mouse-moved game button-state x y dx dy))
    (:mouse-button-down-event (:button button :x x :y y)
			      (mouse-button-down game button x y))
    (:mouse-button-up-event (:button button :x x :y y)
    			    (mouse-button-up game button x y))
    (:active-event (:state state :gain gain)
		   (when (sdl:active-p state)
		       (cond
			 ((= gain 1)
			  (setf (minimized game) nil)
			  (window-active game))
			 (t
			  (setf (minimized game) t)
			  (window-inactive game)))))
    (:idle () 
	   (unless (minimized game)
	     (do-update game)
	     (do-render game))))
  (end-loop game))
