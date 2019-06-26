(load "/home/evan/quicklisp/setup.lisp")
(ql:quickload :swank)

(in-package :stumpwm)

(defvar *config-dir* (make-pathname :directory '(:absolute "home" "evan")
				    :name ".stumpwm.d"))

(defvar *modules-dir* (make-pathname :directory '(:absolute "home" "evan" ".stumpwm.d")
				     :name "modules")
  "StumpWM official modules directory")
(defvar *user-modules-dir* (make-pathname :directory '(:absolute "home" "evan" ".stumpwm.d")
					  :name "user-modules")
  "User modules directory")

(init-load-path *config-dir*)

(load-module "backlight")

(run-commands "exec /home/evan/bin/ctrlcapson")
(run-commands "exec /home/evan/bin/touchpad.sh")
(run-commands "exec synapse -s")
(run-commands "exec /home/evan/Telegram/Telegram")
(run-commands "exec keepassxc")
(run-commands "exec xterm")

(setf *mouse-focus-policy* :click)

(setf stumpwm:*screen-mode-line-format*
      (list "%W | %b | "
	    '(:eval (stumpwm:run-shell-command "date '+%a %b %d %H:%M %Z %Y'" t))))

(setf stumpwm:*mode-line-timeout* 10)

(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))

(swank-loader:init)
(defcommand swank () ()
	    (swank:create-server :port 4005
				 :style swank:*communication-style*
				 :dont-close t)
	    (echo-string
	     (current-screen)
	     "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
(swank)


