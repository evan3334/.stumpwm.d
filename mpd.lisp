(in-package :stumpwm)

(define-key *top-map* (kbd "XF86AudioPlay") "mpd-toggle-pause")
(define-key *top-map* (kbd "XF86AudioPrev") "mpd-prev")
(define-key *top-map* (kbd "XF86AudioNext") "mpd-next")
(define-key *top-map* (kbd "XF86AudioStop") "mpd-stop")

