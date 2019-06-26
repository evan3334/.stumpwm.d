;;;; backlight.lisp

(in-package #:backlight)

(defvar *backlight-dir* "/sys/class/backlight/intel_backlight"
  "Directory for the backlight device")

(defvar *brightness-file* (make-pathname :directory *backlight-dir*
					 :name "brightness")
  "File to control brightness/query current brightness")

(defvar *max-brightness-file* (make-pathname :directory *backlight-dir*
					     :name "max_brightness")
  "File to query maximum brightness")

(defun get-current-brightness ()
  (with-open-file (br *brightness-file*)
    (parse-integer (read-line br nil))))

(defun get-max-brightness ()
  (with-open-file (mbr *max-brightness-file*)
    (parse-integer (read-line mbr nil))))

(defun get-brightness-pct ()
  (multiple-value-bind (n r)
      (round (* (/ (get-current-brightness) (get-max-brightness)) 100))
    n))

(defun pct-to-val (pct)
  (multiple-value-bind (n r)
      (round (* (/ pct 100) (get-max-brightness)))
    n))

(defun set-brightness (brightness)
  (let* ((max (get-max-brightness))
	 (b (cond ((< brightness 0) 0)
		  ((> brightness max) max)
		  (t brightness)))
	 (bstr (write-to-string b)))
    (with-open-file (br *brightness-file*
			:direction :output
			:if-exists :overwrite)
      (write-string bstr br))))

(defun set-brightness-pct (pct)
  (set-brightness (pct-to-val pct)))

(defun inc-brightness (amount)
  (let* ((current-b (get-current-brightness))
	 (new-b (+ current-b amount)))
    (set-brightness new-b)))

(defun inc-brightness-pct (pct)
  (inc-brightness (pct-to-val pct)))

(defun dec-brightness (amount)
  (let* ((current-b (get-current-brightness))
	 (new-b (- current-b amount)))
    (set-brightness new-b)))

(defun dec-brightness-pct (pct)
  (dec-brightness (pct-to-val pct)))

(defun brightness-pct-formatter (ml)
  (declare (ignore ml))
  (format nil "~a%" (get-brightness-pct)))

(add-screen-mode-line-formatter #\b #'brightness-pct-formatter)

(defcommand brightness-current (pct) ((:y-or-n "Return as percent? "))
  (if pct
      (message "~a%" (get-brightness-pct))
      (message "~a" (get-current-brightness))))

(defcommand brightness-set (val pct) ((:number "Enter brightness value: ")
				      (:y-or-n "Interpret as percent? "))
  (if pct
      (set-brightness-pct val)
      (set-brightness val)))

(defcommand brightness-inc (val pct) ((:number "Enter brightness value: ")
				      (:y-or-n "Interpret as percent? "))
  (if pct
      (inc-brightness-pct val)
      (inc-brightness val)))

(defcommand brightness-dec (val pct) ((:number "Enter brightness value: ")
				      (:y-or-n "Interpret as percent? "))
  (if pct
      (dec-brightness-pct val)
      (dec-brightness val)))

(define-key *top-map* (kbd "XF86MonBrightnessUp") "brightness-inc 5 y")
(define-key *top-map* (kbd "M-XF86MonBrightnessUp") "brightness-inc 10 y")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightness-dec 5 y")
(define-key *top-map* (kbd "M-XF86MonBrightnessDown") "brightness-dec 10 y")

(export '(get-current-brightness
	  get-max-brightness
	  get-brightness-pct
	  set-brightness
	  set-brightness-pct
	  inc-brightness
	  inc-brightness-pct
	  dec-brightness
	  dec-brightness-pct))
