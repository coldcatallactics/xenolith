(in-package #:autolith)

;;;; Xenolith look: chrome band.
;;;;
;;;; The status band and the legend row are the two full-bleed rows in the
;;;; composed frame. Both run edge to edge on the band's own background and keep
;;;; their content inset, so the band reads as a bar across the terminal rather
;;;; than a strip inside the frame.
;;;;
;;;; XENOLITH_BAND_PAD sets the content inset (default 2, 0 keeps the content at
;;;; the edge) and XENOLITH_BAND=off skips the wrap at load. A serious condition
;;;; falls back to the tracked builder and is kept in *xenolith-band-failure*.

(defparameter *xenolith-band-pad*
  (max 0 (xenolith--env-integer "XENOLITH_BAND_PAD" 2))
  "Columns of inset on each side of the status band and the legend row.")

(defparameter *xenolith-band-fill-style* :status-plain
  "The style whose background paints the band across the full row.

The theme gives the band styles the band background, so a run of this style
carries the same fill as the status and legend text beside it. A theme without
one renders the padding as plain spaces.")

(defparameter *xenolith-install-band-p*
  (not (equal (uiop:getenv "XENOLITH_BAND") "off"))
  "Whether the site layer paints the status band and the legend row.

XENOLITH_BAND_PAD sets only the content inset, so the band is painted edge to
edge whatever the inset.")

(defvar *xenolith-original-status-row-at* nil
  "The status row builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-band-status-wrapper* nil
  "The status row wrapper this module installed, for reload detection.")

(defvar *xenolith-original-legend-row* nil
  "The legend row builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-band-legend-wrapper* nil
  "The legend row wrapper this module installed, for reload detection.")

(defvar *xenolith-band-failure* nil
  "The most recent condition a band wrapper caught, or NIL.")

(defun xenolith--band-pad (width)
  "Return the inset for a row WIDTH cells wide, leaving a cell to render into."
  (max 0 (min *xenolith-band-pad* (floor (max 0 (1- width)) 2))))

(defun xenolith--band-fill (ui)
  "Return the sequence painting the band background on UI's terminal, or NIL.

An unstyled terminal carries no fill, so its band keeps plain rows."
  (let ((terminal (terminal-ui-terminal ui)))
    (and terminal
         (terminal-styled-p terminal)
         (terminal-style-sequence *xenolith-band-fill-style*))))

(defun xenolith--band-fill-spans (cells)
  "Return CELLS columns of band fill as spans, or no spans when empty."
  (when (plusp cells)
    (list (terminal-span *xenolith-band-fill-style*
                         (make-string cells :initial-element #\Space)))))

(defun xenolith--band-effort-spans (spans)
  "Return SPANS with the effort word wearing the effort's own style."
  (let ((style (and (fboundp 'xenolith-effort-style)
                    (xenolith-effort-style))))
    (if style
        (loop for span in spans
              collect (if (and (terminal-span-p span)
                               (eq (terminal-span-style span) ':status-effort))
                          (terminal-span style (terminal-span-text span))
                          span))
        spans)))

(defun xenolith--inset-status-row (ui now row-width)
  "Return the status row on the band, its content inset from both edges.

The row's own spans already carry the band background, so the inset columns are
filled with it too and the band reaches both edges of the terminal."
  (let ((previous (or *xenolith-original-status-row-at*
                      (symbol-function 'terminal-ui--status-row-at))))
    (handler-case
        (let* ((pad   (xenolith--band-pad row-width))
               (inner (max 1 (- row-width (* 2 pad))))
               (spans (xenolith--band-effort-spans
                       (funcall previous ui now inner))))
          (append (xenolith--band-fill-spans pad)
                  spans
                  (xenolith--band-fill-spans
                   (max 0 (- row-width pad (terminal--spans-width spans))))))
      (serious-condition (condition)
        (setf *xenolith-band-failure* condition)
        (xenolith-log "band: status row FAILED ~A" condition)
        (funcall previous ui now row-width)))))

(defun xenolith--inset-legend-row (ui &key top total space width)
  "Return the legend row on the band, its content inset like the status row."
  (let ((previous (or *xenolith-original-legend-row*
                      (symbol-function 'terminal-ui-fullscreen--legend-row))))
    (handler-case
        (let* ((pad   (xenolith--band-pad width))
               (inner (max 1 (- width (* 2 pad))))
               (row   (funcall previous ui
                               :top top :total total :space space :width inner))
               (fill  (xenolith--band-fill ui))
               (used  (text-cell-width (clinedi:ansi-strip row)))
               (tail  (max 0 (- width pad used))))
          (if fill
              (concatenate 'string
                             (xenolith--fill-after-resets
                            (concatenate 'string
                                         (make-string pad :initial-element #\Space)
                                         row)
                            fill)
                           (make-string tail :initial-element #\Space)
                           *terminal-style-reset*)
              (if (plusp pad)
                  (concatenate 'string (make-string pad :initial-element #\Space) row)
                  row)))
      (serious-condition (condition)
        (setf *xenolith-band-failure* condition)
        (xenolith-log "band: legend row FAILED ~A" condition)
        (funcall previous ui :top top :total total :space space :width width)))))

(when *xenolith-install-band-p*
  (when (fboundp 'terminal-ui--status-row-at)
    (xenolith--chain 'terminal-ui--status-row-at
                     '*xenolith-original-status-row-at*
                     '*xenolith-band-status-wrapper*
                     #'xenolith--inset-status-row))
  (when (fboundp 'terminal-ui-fullscreen--legend-row)
    (xenolith--chain 'terminal-ui-fullscreen--legend-row
                     '*xenolith-original-legend-row*
                     '*xenolith-band-legend-wrapper*
                     #'xenolith--inset-legend-row)))
