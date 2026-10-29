(in-package #:autolith)

;;;; Xenolith look: composed terminal surface.
;;;;
;;;; Autolith's inline UI prints into the terminal's own scrollback, so per-row
;;;; chrome (block surfaces, diff rows, a pinned composer) has nowhere to live.
;;;; Xenolith selects the composed surface instead: the frame fills the
;;;; terminal, the composer sits at the bottom, and the transcript keeps its
;;;; own viewport. Autolith enters the alternate buffer for the session and
;;;; prints the retained transcript back to the normal screen on exit.

(defparameter *xenolith-install-fullscreen-p*
  (not (equal (uiop:getenv "XENOLITH_FULLSCREEN") "off"))
  "Whether the site layer selects the composed terminal surface.")

(defvar *xenolith-fullscreen-failure* nil
  "The most recent condition composed-surface selection caught, or NIL.")

(defun xenolith-fullscreen-enable (&optional (configuration *user-init-configuration*))
  "Select the composed surface on CONFIGURATION, returning true when it holds.

Noninteractive commands build an immutable configuration: its settings cannot
change, and nothing in such a run reads the surface, so they keep their value."
  (and *xenolith-install-fullscreen-p*
       configuration
       (not (config :immutable-p configuration))
       (handler-case
           (progn
             (setf (config :fullscreen-p configuration) t)
             t)
         (serious-condition (condition)
           (setf *xenolith-fullscreen-failure* condition)
           (xenolith-log "fullscreen: FAILED ~A" condition)
           nil))))

(xenolith-fullscreen-enable)
