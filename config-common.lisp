;; --------------------
;; Load libraries (required for stumptray)
;; --------------------

(ql:quickload :xembed)
(ql:quickload :zpng)

(in-package :stumpwm)

;; --------------------
;; Load modules
;; --------------------

(load-module "cpu")
(load-module "mem")
(load-module "screenshot")
(load-module "volume")
(load-module "rhythmbox")

;; --------------------
;; Startup programs
;; --------------------

(run-commands "exec /home/evan/bin/ctrlcapson")
(run-commands "exec /home/evan/Telegram/Telegram")
(run-commands "exec keepassxc")
(run-commands "exec xterm")

;; --------------------
;; Set mouse focus policy
;; --------------------

(setf *mouse-focus-policy* :click)

;; --------------------
;; Set up mode line
;; --------------------

(let ((status-format (if (eq *selected-config* :laptop)
			 "| BRT: %b | %C | %M | BAT: %B | %p | "
			 "| %C | %M | %p | ")))
  (setf stumpwm:*screen-mode-line-format*
	(list "[%n] %W"
	      '(:eval (stumpwm:run-shell-command "echo" t))
	      status-format
	      '(:eval (stumpwm:run-shell-command "date '+%a %b %d %H:%M %Z %Y'" t)))))

(setf stumpwm:*mode-line-timeout* 2)

(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))

;; --------------------
;; Load stumptray
;; (done later because it needs the mode line to be set up first)
;; --------------------

(load-module "stumptray")
(stumptray:stumptray)

;; --------------------
;; Load keybindings
;; --------------------

(load-local-file "keybindings")


