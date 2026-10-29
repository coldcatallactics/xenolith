(in-package #:autolith)

;;;; Xenolith look: no key-hint legend above the composer.
;;;;
;;;; The tracked frame spends one row between the transcript and the status band
;;;; on the legend that carries the LIVE marker and the scrolling keys. The
;;;; distribution drops that row and gives the space back to the transcript,
;;;; leaving the status band and the composer exactly where they were.
;;;;
;;;; XENOLITH_LEGEND=on keeps the tracked legend. A serious condition falls back
;;;; to the tracked frame and is kept in *xenolith-legend-failure*.

(defparameter *xenolith-legend-p*
  (equal (uiop:getenv "XENOLITH_LEGEND") "on")
  "Whether the frame keeps the tracked legend row above the status band.")

(defvar *xenolith-original-frame* nil
  "The tracked fullscreen frame builder captured before the first wrap.")

(defvar *xenolith-frame-wrapper* nil
  "The frame wrapper this module installed, for reload detection.")

(defvar *xenolith-legend-failure* nil
  "The most recent condition the legendless frame caught, or NIL.")

(unless *xenolith-original-frame*
  (when (fboundp 'terminal-ui-fullscreen--frame)
    (setf *xenolith-original-frame*
          (symbol-function 'terminal-ui-fullscreen--frame))))

(defun xenolith--frame-rows (ui status-now)
  "Compose UI's fullscreen frame with the legend's row left to the transcript.

This mirrors terminal-ui-fullscreen--frame with two differences: the legend row
is not built, and the transcript window keeps the row the separator reserved,
so the frame still fills the terminal exactly."
  (let* ((terminal (terminal-ui-terminal ui))
         (width    (max 1 (terminal-columns terminal)))
         (height   (max 1 (terminal-rows terminal))))
    (terminal-ui-fullscreen--ensure-width ui width)
    (multiple-value-bind (composer cursor-row cursor-column)
        (terminal-ui-fullscreen--composer ui width height)
      (let* ((viewport  (fullscreen-terminal-ui-viewport ui))
             (available (max 0 (- height (length composer))))
             (prefix    (terminal-ui-fullscreen--prompt-prefix
                         ui :width width :limit (max 0 (1- available)) :now status-now))
             (space     (- available (length prefix)))
             (committed (clinedi:transcript-viewport-row-count viewport))
             (tail      (terminal-ui-stream-tail ui))
             (live      (coerce
                         (terminal-ui-fullscreen--display-rows
                          ui (when tail
                               (append (when (terminal-ui-compact-tail-group ui) (list nil))
                                       (terminal-ui--stream-tail-rows terminal tail width)))
                          width)
                         'vector))
             (total     (+ committed (length live)))
             (top       (clinedi:transcript-viewport-layout viewport space
                                                            :extra-rows (length live)))
             (visible   (if (and (zerop total) (terminal-ui-fullscreen-welcome-p ui))
                            (terminal-ui--welcome-rows ui space)
                            (loop for index from top below (min total (+ top space))
                                  collect (if (< index committed)
                                              (clinedi:transcript-viewport-row-display viewport index)
                                              (aref live (- index committed)))))))
        (values (append visible
                        (make-list (max 0 (- space (length visible))) :initial-element "")
                        prefix
                        composer)
                (+ space (length prefix) cursor-row)
                cursor-column)))))

(defun xenolith--frame (ui status-now)
  "Return UI's fullscreen frame, without the tracked legend row by default."
  (if *xenolith-legend-p*
      (funcall *xenolith-original-frame* ui status-now)
      (handler-case
          (xenolith--frame-rows ui status-now)
        (serious-condition (condition)
          (setf *xenolith-legend-failure* condition)
          (xenolith-log "legend: frame FAILED ~A" condition)
          (funcall *xenolith-original-frame* ui status-now)))))

(when *xenolith-original-frame*
  (xenolith--chain 'terminal-ui-fullscreen--frame
                   '*xenolith-original-frame*
                   '*xenolith-frame-wrapper*
                   #'xenolith--frame))
