(in-package :stumpwm)

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
    (with-open-file (br brightness-file
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

(set-brightness 100)
(inc-brightness 50)
(dec-brightness 50)
(get-brightness-pct)
(inc-brightness-pct 10)
(set-brightness-pct 5)
