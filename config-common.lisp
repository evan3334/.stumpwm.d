;; --------------------
;; Load libraries (required for stumptray)
;; --------------------

(ql:quickload :xembed)
(ql:quickload :zpng)
(ql:quickload :clx-truetype)

(in-package :stumpwm)

;; --------------------
;; Set up fonts early
;; --------------------

(load-module "ttf-fonts")
(clx-truetype:cache-fonts)
(set-font (make-instance 'xft:font :family "Unifont" :subfamily "Medium" :size 13))

;; --------------------
;; Load modules
;; --------------------

(load-module "cpu")
(load-module "mem")
(load-module "mpd")
(load-module "volume")
;;(load-module "rhythmbox")

;; --------------------
;; Startup programs
;; --------------------

(run-shell-command "/home/evan/bin/ctrlcapson")
(run-shell-command "/home/evan/Telegram/Telegram")
(run-shell-command "keepassxc")
(run-shell-command "urxvt")
(run-shell-command "compton")

;; --------------------
;; Set mouse focus policy
;; --------------------

(setf *mouse-focus-policy* :click)

;; --------------------
;; Set up mode line
;; --------------------
(print "loading mode line")

(setf mpd:*mpd-current-song-fmt* "%a - %t (%e/%l)")
(setf mpd:*mpd-status-fmt* "")

(let ((status-format (if (eq *selected-config* :laptop)
			 "| %C | %M | BAT: %B | %m | VOL: %V | BRT: %b | "
			 "| %C | %M | %m | VOL: %V | "))
      (newline (format nil "~%")))
  (setf stumpwm:*screen-mode-line-format*
	(list "[%n] %W"
	      newline
	      status-format
	      '(:eval (async-run "date '+%a %b %d %Y %l:%M %p %Z'")))))
(setf stumpwm:*mode-line-timeout* 2)
(stumpwm:toggle-mode-line (stumpwm:current-screen)
			  (stumpwm:current-head))
(print "mode line loaded")
;; --------------------
;; Load stumptray
;; (done later because it needs the mode line to be set up first)
;; --------------------

;;(load-module "stumptray")
;;(stumptray:stumptray)

;; --------------------
;; Load keybindings
;; --------------------

(load-local-file "misc-commands")
(load-local-file "keybindings")
(load-local-file "mpd")
