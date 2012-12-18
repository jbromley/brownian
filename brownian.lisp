;;;; brownian.lisp

(in-package #:brownian)

(defun run-brownian (width height)
  (let ((game (make-instance 'brownian-loop :window-title "Brownian"
			     :window-width width :window-height height)))
    (initialize game)
    (run game)))
