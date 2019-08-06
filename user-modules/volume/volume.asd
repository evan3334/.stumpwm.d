;;;; volume.asd

(asdf:defsystem #:volume
  :description "StumpWM module for controlling the volume of the current pulseaudio sink."
  :author "Evan Straw <evan.straw99@gmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "volume")))
