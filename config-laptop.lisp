(in-package :stumpwm)

(load-module "backlight")
(load-module "battery-portable")

(run-commands "exec /home/evan/bin/touchpad.sh")
(run-commands "exec compton")
