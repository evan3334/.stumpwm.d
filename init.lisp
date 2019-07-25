(load "/home/evan/quicklisp/setup.lisp")
(ql:quickload :xembed)
(ql:quickload :zpng)

(in-package :stumpwm)

(defvar *config-dir* '(:absolute "home" "evan" ".stumpwm.d"))

(defvar *modules-dir* (make-pathname :directory *config-dir*
				     :name "modules")
  "StumpWM official modules directory")

(defvar *user-modules-dir* (make-pathname :directory *config-dir*
					  :name "user-modules")
  "User modules directory")

(defvar *module-paths* (init-load-path *modules-dir*))
(defvar *user-module-paths* (init-load-path *user-modules-dir*))

(defun load-local-file (name)
    (load (make-pathname :directory *config-dir*
			 :name name
			 :type "lisp")))

(map nil #'add-to-load-path *module-paths*)
(map nil #'add-to-load-path *user-module-paths*)

(load-module "backlight")

(load-module "cpu")
(load-module "mem")
(load-module "battery-portable")

(load-module "screenshot")

(run-commands "exec /home/evan/bin/ctrlcapson")
(run-commands "exec /home/evan/bin/touchpad.sh")
;;(run-commands "exec synapse -s")
(run-commands "exec /home/evan/Telegram/Telegram")
(run-commands "exec keepassxc")
(run-commands "exec xterm")

(setf *mouse-focus-policy* :click)

(setf stumpwm:*screen-mode-line-format*
      (list "%W"
	    '(:eval (stumpwm:run-shell-command "echo" t))
	    "| BRT: %b | %C | %M | BAT: %B | "
	    '(:eval (stumpwm:run-shell-command "date '+%a %b %d %H:%M %Z %Y'" t))))

(setf stumpwm:*mode-line-timeout* 2)

(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))

(load-module "stumptray")
(stumptray:stumptray)

(load-local-file "keybindings")
