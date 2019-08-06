;;;; volume.lisp

(in-package #:volume)

(ql:quickload "cl-ppcre")

(defun get-default-sink ()
  (ppcre:register-groups-bind (index)
      ("\\* index: (\\d)" (stumpwm:run-shell-command "pacmd list-sinks" t))
    index))

(defun build-volume-command (sink volume)
  (let* ((operator (if (< volume 0)
		      "-"
		      "+"))
	 (absvol (abs volume))
	 (volstr (format nil "~a~a" operator absvol)))
    (format nil "pactl set-sink-volume ~a ~a%" sink volstr)))

(defun inc-volume (pct)
  (stumpwm:run-shell-command (build-volume-command "@DEFAULT_SINK@" pct)))

(defun dec-volume (pct)
  (inc-volume (* -1 pct)))

(defcommand volume-inc (val) ((:number "Enter volume percent: "))
  (inc-volume val))

(defcommand volume-dec (val) ((:number "Enter volume percent: "))
  (dec-volume val))

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-inc 5")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-dec 5")
