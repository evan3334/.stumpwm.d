(in-package :stumpwm)

(defvar *screenshots-dir* "~/Pictures/screenshots/")

(defcommand activate-flameshot () ()
  (stumpwm:run-shell-command
   (format nil "flameshot gui -p ~a" *screenshots-dir*)))

(defcommand rofi () ()
  (run-commands "exec rofi -show drun"))
