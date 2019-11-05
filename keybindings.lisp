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

(define-key *root-map* (kbd "C-c") "exec urxvt")
(define-key *root-map* (kbd "c") "exec urxvt")
(define-key *root-map* (kbd "C-s") "swank")
(define-key *top-map* (kbd "s-SPC") "rofi")
(define-key *top-map* (kbd "Print") "activate-flameshot")
(define-key *top-map* (kbd "s-l") "xscreensaver-lock")
