;;;; brownian.asd

(asdf:defsystem #:brownian
  :serial t
  :description "Simple Brownian motion simulator using SDL."
  :author "Jay Bromley <jbromley@gmail.com>"
  :license "MIT"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx
               #:lispbuilder-sdl-ttf)
  :components ((:file "package")
	       (:file "color-utils")
	       (:file "game-loop")
	       (:file "particle" :depends-on ("color-utils"))
	       (:file "brownian-loop" 
		      :depends-on ("game-loop" "color-utils" "particle"))
               (:file "brownian" :depends-on ("brownian-loop"))))

