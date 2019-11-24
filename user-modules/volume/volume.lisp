;;;; volume.lisp

(in-package #:volume)

;; Credit to Solomon Bloch for this trick.
;; https://solb.io/blog/asynchronize-your-life%3A-shell-commands-10x-faster
(defparameter *async-shell*
  (uiop:launch-program "bash" :input :stream :output :stream))

(defun async-run (command)
  (write-line command (uiop:process-info-input *async-shell*))
  (force-output (uiop:process-info-input *async-shell*))
  (let* ((output-string (read-line (uiop:process-info-output *async-shell*)))
         (stream (uiop:process-info-output *async-shell*)))
    (if (listen stream)
        (loop while (listen stream)
           do (setf output-string (concatenate 'string
                                               output-string
                                               '(#\Newline)
                                               (read-line stream)))))
    output-string))

(ql:quickload "cl-ppcre")

(defun list-sinks ()
  (async-run "pacmd list-sinks"))

(defun get-default-sink ()
  (ppcre:register-groups-bind (index)
      ("\\* index: (\\d)" (list-sinks))
    index))

(defun pulsemixer-get-volume ()
  (async-run "pulsemixer --get-volume"))

(defvar *volume-scanner*
  (ppcre:create-scanner
   ;;"\\* index:.*[\\s\\S]*v.*:\\sf.*\\s([0-9]{1,3})%.*\\s([0-9]{1,3})%"
   "\\* index:(?:.*[\\S\\s]{2}){1,10}.*\\s(\\d{1,3})%.*\\s(\\d{1,3})%"
   :multi-line-mode t))

(defvar *mute-scanner*
  (ppcre:create-scanner
   "\\* index:(?:.*[\\s\\S]{2}){1,16}muted: ([a-z]{2,3})"
   :multi-line-mode t))

(defun parse-volume (str)
  (ppcre:register-groups-bind (lvolume rvolume)
       (*volume-scanner* str)
    (values (parse-integer lvolume) (parse-integer rvolume))))

(defun parse-mute (str)
  (ppcre:register-groups-bind (muted)
      (*mute-scanner* str)
    (string-equal muted "yes")))

(defun get-volume ()
  (parse-volume (list-sinks)))

(defun get-avg-volume (&key str)
  (multiple-value-bind (l r) (if str (parse-volume str) (get-volume))
    (multiple-value-bind (q) (floor (+ l r) 2) q)))

(defun build-volume-command (sink volume)
  (let* ((operator (if (< volume 0)
		       "-"
		       "+"))
	 (absvol (abs volume))
	 (volstr (format nil "~a~a" operator absvol)))
    (format nil "pactl set-sink-volume ~a ~a% && echo ok" sink volstr)))

(defun build-mute-command (sink)
  (format nil "pactl set-sink-mute ~a toggle && echo ok" sink))

(defun volume-pct-formatter (ml)
  (declare (ignore ml))
  (let* ((sinks (list-sinks))
	 (muted (parse-mute sinks))
	 (vol (get-avg-volume :str sinks))
	 (capvol (if (> vol 100) 100 vol))
	 (volstr (if muted "MUTE" (format nil "~a%" capvol)))
	 (volbar (stumpwm:bar (if muted 0 capvol) (if muted 9 10) #\= #\-)))
    (format nil "~a [~a]" volstr volbar)))
(add-screen-mode-line-formatter #\V #'volume-pct-formatter)

(defcommand volume-inc (pct) ((:number "Enter volume percent: "))
  (async-run (build-volume-command "@DEFAULT_SINK@" pct))
  (get-avg-volume))

(defcommand volume-dec (pct) ((:number "Enter volume percent: "))
  (volume-inc (* -1 pct)))

(defcommand volume-mute () ()
  (async-run (build-mute-command "@DEFAULT_SINK@"))
  (if (parse-mute (list-sinks)) "Muted" "Unmuted"))

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-inc 2")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-dec 2")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume") "volume-inc 5")
(define-key *top-map* (kbd "M-XF86AudioLowerVolume") "volume-dec 5")
(define-key *top-map* (kbd "XF86AudioMute") "volume-mute")

(export '(get-volume
	  get-avg-volume
	  volume-inc
	  volume-dec
	  volume-mute))
