(ql:quickload :swank)

(in-package :stumpwm)

(swank-loader:init)

(defcommand swank () ()
	    (swank:create-server :port 4005
				 :style swank:*communication-style*
				 :dont-close t)
	    (echo-string
	     (current-screen)
	     "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

(defcommand rofi () ()
  (run-commands "exec rofi -show drun"))


(define-key *root-map* (kbd "C-s") "swank")
(define-key *top-map* (kbd "C-S-SPC") "rofi")
