;; --------------------
;; Load Libraries
;; --------------------

(load "/home/evan/quicklisp/setup.lisp")

(in-package :stumpwm)

;; --------------------
;; Setup directory variables
;; --------------------

(defvar *config-dir* '(:absolute "home" "evan" ".stumpwm.d"))

(defvar *modules-dir* (make-pathname :directory *config-dir*
				     :name "modules")
  "StumpWM official modules directory")

(defvar *user-modules-dir* (make-pathname :directory *config-dir*
					  :name "user-modules")
  "User modules directory")

(defvar *module-paths* (init-load-path *modules-dir*))
(defvar *user-module-paths* (init-load-path *user-modules-dir*))

;; --------------------
;; Build module load paths
;; --------------------

(map nil #'add-to-load-path *module-paths*)
(map nil #'add-to-load-path *user-module-paths*)

;; --------------------
;; Setup config selection and local file loading
;; --------------------

(defvar *selected-config* :none
  "Selected configuration, used for machine-specific config such as startup programs")

(defun make-local-pathname (name)
  (make-pathname :directory *config-dir*
		 :name name
		 :type "lisp"))

(defun load-local-file (name)
  (load (make-local-pathname name)))

(defun load-select-config ()
  (let* ((pathname (make-local-pathname "select-config"))
	 (file-path (probe-file pathname)))
    (if file-path
	(load file-path))))

(load-select-config)

;; --------------------
;; Set up shell for running shell commands
;; --------------------
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

;; --------------------
;; Load configuration
;; --------------------

(load-local-file "config-common")

(cond
  ((eq *selected-config* :desktop) (load-local-file "config-desktop"))
  ((eq *selected-config* :laptop) (load-local-file "config-laptop")))
