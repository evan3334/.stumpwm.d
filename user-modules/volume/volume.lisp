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
   "\\*\\sindex:.*[\\s\\S]*v.*:\\sf.*\\s([0-9]{1,3})%.*\\s([0-9]{1,3})%"
   :multi-line-mode t))

(defun parse-volume (str)
  (ppcre:register-groups-bind (lvolume rvolume)
       (*volume-scanner* str)
    (values (parse-integer lvolume) (parse-integer rvolume))))

(defun get-volume ()
  (parse-volume (list-sinks)))

(defun get-avg-volume ()
  (multiple-value-bind (l r) (get-volume)
    (multiple-value-bind (q) (floor (+ l r) 2) q)))

(defun build-volume-command (sink volume)
  (let* ((operator (if (< volume 0)
		       "-"
		       "+"))
	 (absvol (abs volume))
	 (volstr (format nil "~a~a" operator absvol)))
    (format nil "pactl set-sink-volume ~a ~a% && echo ok" sink volstr)))

(defun volume-pct-formatter (ml)
  (declare (ignore ml))
  (let* ((vol (get-avg-volume))
	 (capvol (if (> vol 100) 100 vol))
	 (volbar (stumpwm:bar capvol 10 #\= #\-)))
    (format nil "~a% [~a]" vol volbar)))

(add-screen-mode-line-formatter #\V #'volume-pct-formatter)

(defun inc-volume (pct)
  (async-run (build-volume-command "@DEFAULT_SINK@" pct))
  (get-avg-volume))

(defun dec-volume (pct)
  (inc-volume (* -1 pct)))

(defcommand volume-inc (val) ((:number "Enter volume percent: "))
  (inc-volume val))

(defcommand volume-dec (val) ((:number "Enter volume percent: "))
  (dec-volume val))

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-inc 2")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-dec 2")
(define-key *top-map* (kbd "M-XF86AudioRaiseVolume") "volume-inc 5")
(define-key *top-map* (kbd "M-XF86AudioLowerVolume") "volume-dec 5")

(export '(get-volume
	  get-avg-volume
	  inc-volume
	  dec-volume))
