;;;; rhythmbox.lisp

(in-package #:rhythmbox)

(defconstant *now-playing-opts* '((album-title . "%at")
				  (album-artist . "%aa")
				  (album-artist-lower . "%aA")
				  (album-artist-sortname . "%aS")
				  (album-year . "%ay")
				  (album-genre . "%ag")
				  (album-genre-lower . "%aG")
				  (album-disc-num . "%an")
				  (album-disc-num-zero . "%aN")
				  (stream-title . "%st")
				  (track-num . "%tn")
				  (track-num-zero . "%tN")
				  (track-title . "%tt")
				  (track-artist . "%ta")
				  (track-artist-lower . "%tA")
				  (track-artist-sortname . "%ts")
				  (track-artist-sortname-lower . "%tS")
				  (track-duration . "%td")
				  (track-elapsed . "%te"))
  "An alist of all supported format options for Rhythmbox 'Now Playing' information.")

(defconstant *rhythmbox-client-command*
  "rhythmbox-client --no-start --print-playing-format \"~a\""
  "The command to run rhythmbox-client, as a format string.")

(defvar *now-playing-format* '(track-artist " - " track-title
			       " (" track-elapsed "/" track-duration ")")
  "The current format string for 'Now Playing' information")

(defun resolve-format (format-list)
  (defun resolve-format-inner (format-str format-list)
    (cond
      ((null format-list) format-str)
      ((stringp (car format-list))
       (resolve-format-inner
	(concatenate 'string format-str (car format-list))
	(cdr format-list)))
      ((symbolp (car format-list))	      
       (resolve-format-inner
	(format nil "~a~a" format-str (cdr (assoc (car format-list) *now-playing-opts*)))
	(cdr format-list)))
      (else
       (resolve-format-inner
	(format nil "~a~a" format-str (car format-list))
	(cdr format-list)))))
  (resolve-format-inner "" format-list))

(resolve-format *now-playing-format*)

(defun get-now-playing ()
  (string-right-trim '(#\Newline #\Linefeed #\Return)
		     (stumpwm:run-shell-command
		      (format nil *rhythmbox-client-command*
			      (resolve-format *now-playing-format*)) t)))

(defun now-playing-modeline-formatter (ml)
  (declare (ignore ml))
  (format nil "~a" (get-now-playing)))

(add-screen-mode-line-formatter #\p #'now-playing-modeline-formatter)

(export '(*now-playing-opts*
	  *now-playing-format*
	  get-now-playing))
