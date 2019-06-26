;;;; backlight.asd

(asdf:defsystem #:backlight
  :description "StumpWM module to control the screen backlight on Intel laptops"
  :author "Evan Straw <evan.straw99@gmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "backlight")))
