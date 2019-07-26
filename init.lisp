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

(defun load-select-config ()
  (let* ((pathname (make-local-pathname "select-config"))
	 (file-path (probe-file pathname)))
    (if file-path
	(load file-path))))

(defun make-local-pathname (name)
  (make-pathname :directory *config-dir*
		 :name name
		 :type "lisp"))

(defun load-local-file (name)
  (load (make-local-pathname name)))

(load-select-config)

;; --------------------
;; Load configuration
;; --------------------

(load-local-file "config-common")

(cond
  ((eq *selected-config* :desktop) (load-local-file "config-desktop"))
  ((eq *selected-config* :laptop) (load-local-file "config-laptop")))
