(in-package #:autolith)

;;;; Xenolith look: composer box.
;;;;
;;;; Wraps terminal-ui--composer-rows to give the prompt a filled box: a left
;;;; effort bar, one column of margin, and one blank row above and below.
;;;; Rows keep their representation, so span lists stay span lists and rendered
;;;; rows stay rendered rows; only the text and the cursor shift.

(defparameter *xenolith-composer-margin* 1
  "Columns of margin left of the composer bar.")

(defparameter *xenolith-composer-padding* 1
  "Blank rows above and below the composer content.")

(defparameter *xenolith-composer-bar* "▌"
  "The left bar glyph.")
(defparameter *xenolith-effort-styles*
  '(("none"  . :dim)
    ("low"   . :syntax-type)
    ("medium" . :plan-active)
    ("high"  . :agent-role)
    ("xhigh" . :status-effort)
    ("max"   . :notice)
    ("ultra" . :notice))
  "Reasoning effort names mapped to the bar style, one step per level.")

(defvar *xenolith-effort* nil
  "The reasoning effort the last status row reported, or NIL before the first.")

(defun xenolith-effort-style (&optional (effort *xenolith-effort*))
  "Return the composer bar style for EFFORT, or the band's own effort style."
  (or (and effort
           (cdr (assoc effort *xenolith-effort-styles* :test #'string=)))
      :status-effort))

(defvar *xenolith-original-composer-rows* nil
  "The composer row builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-composer-failure* nil
  "The most recent condition the composer box caught, or NIL.")

(defparameter *xenolith-install-composer-p*
  (not (equal (uiop:getenv "XENOLITH_COMPOSER") "off"))
  "Whether the site layer boxes the composer.")

(defvar *xenolith-composer-mode-label-function* nil
  "Function of the UI returning a mode label and its style for the composer's
last row, or NIL.

The label sits at the right edge of the last row, so it never moves the
draft or the cursor. Modules that own a mode set this to their own function;
the composer leaves it alone.")

(defun xenolith--composer-prefix (bar-p)
  "Return the margin and optional BAR-P bar as spans, and their text."
  (let ((margin *xenolith-composer-margin*))
    (if bar-p
        (values (append (when (plusp margin)
                          (list (terminal-span
                                 :plain
                                 (make-string margin :initial-element #\Space))))
                          (list (terminal-span (xenolith-effort-style)
                                               *xenolith-composer-bar*)
                              (terminal-span :plain " ")))
                (concatenate 'string
                             (make-string margin :initial-element #\Space)
                             *xenolith-composer-bar*
                             " "))
        (values (when (plusp margin)
                  (list (terminal-span
                         :plain
                         (make-string margin :initial-element #\Space))))
                (make-string margin :initial-element #\Space)))))

(defun xenolith--composer-mode-label (ui)
  "Return UI's composer mode label and style, or no values."
  (let ((function *xenolith-composer-mode-label-function*))
    (if function
        (handler-case
            (funcall function ui)
          (serious-condition (condition)
            (setf *xenolith-composer-failure* condition)
            (values nil nil)))
        (values nil nil))))

(defparameter *xenolith-composer-fill-p* t
  "Whether the composer box paints its rows with the =:surface= fill.

A caller that wants the box unfilled binds this to NIL; the margin, the bar, and
the blank rows stay.")

(defun xenolith--composer-boxed-line (ui prefix prefix-text text display)
  "Return one composer line carrying PREFIX, the accent bar, and the surface fill.

TEXT and DISPLAY are the line's plain and styled content without the bar."
  (let* ((terminal (terminal-ui-terminal ui))
         (plain    (concatenate 'string prefix-text text))
         (styled   (concatenate 'string
                                (terminal--render-spans terminal prefix)
                                display))
         (row      (terminal--make-rendered-row plain styled)))
    (if (and *xenolith-composer-fill-p*
             (fboundp 'xenolith-fill-rendered-row))
        (funcall 'xenolith-fill-rendered-row terminal row :surface :reserve 1)
        row)))

(defun xenolith--composer-footer (ui prefix prefix-text)
  "Return the composer footer: the mode label at the right edge of a filled row.

No row is returned when no module sets a mode label, so the box keeps three rows
for a one-line draft instead of spending one on an empty bar. The composer's
label seam is *xenolith-composer-mode-label-function*."
  (multiple-value-bind (label label-style)
      (xenolith--composer-mode-label ui)
    (when label
      (let* ((terminal    (terminal-ui-terminal ui))
             (width       (max 1 (terminal-columns terminal)))
             (label-width (text-cell-width label))
             (gap         (max 0 (- width (text-cell-width prefix-text) label-width)))
             (spaces      (make-string gap :initial-element #\Space))
             (label-display
               (terminal--render-spans
                terminal
                (list (terminal-span (or label-style :dim) label)))))
        (xenolith--composer-boxed-line
         ui prefix prefix-text
         (concatenate 'string spaces label)
         (concatenate 'string spaces label-display))))))

(defun xenolith--composer-places (text wrapped)
  "Return each of WRAPPED's rows as (START TEXT DISPLAY) with its start in TEXT.

The row builder breaks at a space or at a newline and drops what it broke at, so
a row starts at the first character after that separator; a hard break takes no
character away and the next row starts where the one before it ended."
  (let ((start  0)
        (places '()))
    (dolist (pair wrapped)
      (destructuring-bind (line-text line-display) pair
        (push (list start line-text line-display) places)
        (let ((end (+ start (length line-text))))
          (setf start
                (cond ((and (< end (length text))
                            (char= (char text end) #\Newline))
                       (1+ end))
                      (t
                       (loop while (and (< end (length text))
                                        (char= (char text end) #\Space))
                             do (incf end))
                       end))))))
    (nreverse places)))

(defun xenolith--composer-cursor-place (places offset)
  "Return the index of PLACES holding OFFSET and OFFSET's cell column there.

PLACES are (START TEXT DISPLAY) rows and the column counts cells, so a
double-width character advances the cursor by two. The offset belongs to the
last row that starts at or before it: one exactly at a row's end belongs to the
row starting there, so a hard break puts the cursor before the character that
opens the next row instead of past the last cell of the row above."
  (let ((index  0)
        (column 0))
    (loop for (start line-text) in places
          for position from 0
          do (when (<= start offset)
               (setf index  position
                     column (text-cell-width
                             (subseq line-text
                                     0
                                     (min (length line-text)
                                          (- offset start)))))))
    (values index column)))

(defun xenolith--composer-lines (ui prefix-width rows cursor-row cursor-offset)
  "Return ROWS as (ROW-INDEX TEXT DISPLAY) physical lines and the cursor place.

The tracked composer wraps the draft at the terminal width; the site box adds
its margin and bar, so each row is re-wrapped to the columns left over. Without
that, the frame wraps the boxed row and the continuation line loses the bar."
  (let* ((terminal     (terminal-ui-terminal ui))
         (columns      (max 1 (terminal-columns terminal)))
         (available    (max 1 (- columns prefix-width 1)))
         (lines        '())
         (cursor-line  0)
         (cursor-column cursor-offset))
    (loop for row in rows
          for row-index from 0
          do (multiple-value-bind (text display)
                 (terminal-ui--row-content terminal row)
               (let ((places
                       (xenolith--composer-places
                        text
                        (or (clinedi:wrap-styled-text text display available)
                            (list (list "" ""))))))
                 (when (= row-index cursor-row)
                   (multiple-value-bind (index column)
                       (xenolith--composer-cursor-place places cursor-offset)
                     (setf cursor-line   (+ (length lines) index)
                           cursor-column column)))
                 (dolist (place places)
                   (push (list row-index (second place) (third place)) lines)))))
        (values (nreverse lines) cursor-line cursor-column)))

(defun xenolith--composer-wrap-rows (ui rows cursor-row cursor-offset)
  "Return UI's composer ROWS boxed, with the cursor position preserved.

Every physical line of the draft carries the accent bar and the fill, and the box
pads above and below the input. The tracked builder ends its rows with an empty
one, and the padding already supplies that blank line, so the trailing row is
dropped. Without a mode label the box is three rows for a one-line draft and
grows only as the draft wraps."
  (multiple-value-bind (prefix prefix-text)
      (xenolith--composer-prefix t)
    (let ((shift (text-cell-width prefix-text))
          (draft (if (and rows (null (car (last rows))))
                     (butlast rows)
                     rows)))
      (multiple-value-bind (lines cursor-line cursor-column)
          (xenolith--composer-lines ui shift draft cursor-row cursor-offset)
        (let* ((boxed   (loop for (row text display) in lines
                              collect (xenolith--composer-boxed-line
                                       ui prefix prefix-text text display)))
               (blank   (xenolith--composer-boxed-line ui prefix prefix-text "" ""))
               (footer  (xenolith--composer-footer ui prefix prefix-text))
               (padding (make-list *xenolith-composer-padding* :initial-element blank)))
          (values (append padding boxed padding (when footer (list footer)))
                  (+ cursor-line *xenolith-composer-padding*)
                  (+ cursor-column shift)))))))

(defvar *xenolith-composer-rows-wrapper* nil
  "The composer row wrapper this module installed, for reload detection.")

(defun xenolith--composer-rows (ui)
  "Return UI's composer rows boxed, or the tracked rows on a terminal that cannot
paint fills."
  (multiple-value-bind (rows cursor-row cursor-offset)
      (funcall *xenolith-original-composer-rows* ui)
    (let ((terminal (terminal-ui-terminal ui)))
      (if (and terminal (terminal-styled-p terminal))
          (handler-case
              (xenolith--composer-wrap-rows ui rows cursor-row cursor-offset)
            (serious-condition (condition)
              (setf *xenolith-composer-failure* condition)
              (xenolith-log "composer: FAILED ~A" condition)
              (values rows cursor-row cursor-offset)))
          (values rows cursor-row cursor-offset)))))

(when (fboundp 'terminal-ui--composer-rows)
  (if *xenolith-install-composer-p*
      (xenolith--chain 'terminal-ui--composer-rows
                       '*xenolith-original-composer-rows*
                       '*xenolith-composer-rows-wrapper*
                       #'xenolith--composer-rows)
      (xenolith--unchain 'terminal-ui--composer-rows
                         '*xenolith-original-composer-rows*
                         '*xenolith-composer-rows-wrapper*)))
