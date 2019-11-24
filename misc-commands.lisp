(in-package :stumpwm)

(defvar *screenshots-dir* "~/Pictures/screenshots/")
(defvar *mic-mute* t)

(defcommand activate-flameshot () ()
  (run-shell-command
   (format nil "flameshot gui -p ~a" *screenshots-dir*)))

(defcommand rofi () ()
  (run-commands "exec rofi -show drun"))

(defcommand xscreensaver-lock () ()
  (run-shell-command "xscreensaver-command --lock"))

(defcommand suspend () ()
  (run-shell-command "systemctl suspend"))

(defcommand toggle-mic-feedback () ()
  (let ((mic-mode (if *mic-mute* "unmute" "mute")))
    (run-shell-command (format nil "amixer -c 1 sset Mic ~a" mic-mode)))
  (setf *mic-mute* (not *mic-mute*)))
