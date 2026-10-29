(in-package #:autolith)

;;;; Xenolith dev: composed frame preview.
;;;;
;;;; Interactive verification needs a real terminal, and pty capture is
;;;; unreliable here. XENOLITH-PREVIEW composes the real fullscreen frame over a
;;;; recording terminal and prints its rows, so layout, gutters, and the
;;;; composer can be checked from a plain REPL. Sections are registered by name
;;;; so each look module contributes the rows it owns and reloading the site
;;;; replaces them instead of stacking duplicates.

(defclass xenolith-preview-terminal (terminal)
  ()
  (:documentation "A terminal that discards writes, for composed frame previews."))

(defmethod terminal--write ((terminal xenolith-preview-terminal) (text string))
  "Discard trusted TEXT written through TERMINAL."
  (declare (ignore terminal text))
  nil)

(defmethod terminal-flush ((terminal xenolith-preview-terminal))
  "Finish a recording TERMINAL write batch without external effects."
  (declare (ignore terminal))
  nil)

(defmethod terminal-start ((terminal xenolith-preview-terminal))
  "Start TERMINAL without touching a device."
  (unless (terminal-started-p terminal)
    (setf (terminal-started-p terminal) t
          (terminal-interactive-p terminal) t))
  terminal)

(defmethod terminal-stop ((terminal xenolith-preview-terminal))
  "Stop TERMINAL without touching a device."
  (setf (terminal-started-p terminal) nil
        (terminal-interactive-p terminal) nil)
  terminal)

(defmethod terminal-read-event ((terminal xenolith-preview-terminal))
  "Return end-of-input because a preview terminal has no input queue."
  (declare (ignore terminal))
  :end-of-input)

(defvar *xenolith-preview-sections* nil
  "Ordered (NAME . FUNCTION) preview sections.

A section function takes the row width and returns one span list holding every
row it draws, each row ended by a newline span.")

(defun xenolith-preview-register (name function)
  "Install FUNCTION as preview section NAME, replacing an earlier one in place."
  (let ((cell (assoc name *xenolith-preview-sections*)))
    (if cell
        (setf (cdr cell) function)
        (setf *xenolith-preview-sections*
              (append *xenolith-preview-sections* (list (cons name function)))))
    name))

(defun xenolith-preview-unregister (name)
  "Remove preview section NAME."
  (setf *xenolith-preview-sections*
        (remove name *xenolith-preview-sections* :key #'car))
  name)

(defun xenolith-preview-row (&rest spans)
  "Return SPANS as one preview row."
  (append spans (list (terminal-span :plain (string #\Newline)))))

(defun xenolith-preview-surface-row (width style &rest spans)
  "Return SPANS as one preview row filled with STYLE across WIDTH.

Rows fall back to plain preview rows when the site layer ships no surfaces."
  (let ((row (if (fboundp 'xenolith-surface-row-spans)
                 (xenolith-surface-row-spans spans width style)
                 spans)))
    (append row (list (terminal-span :plain (string #\Newline))))))

(defun xenolith-preview-block-row (width style &rest spans)
  "Return SPANS as one preview row filled like a transcript block.

A block row is inset by the site layer's block margin, so the preview shows the
same margins tool, diff, and reasoning rows carry in a session."
  (let ((row (if (fboundp 'xenolith-surface-row-spans)
                 (xenolith-surface-row-spans
                  spans width style
                  :inset (if (boundp '*xenolith-surface-inset*)
                             *xenolith-surface-inset*
                             0))
                 spans)))
    (append row (list (terminal-span :plain (string #\Newline))))))

(defun xenolith-preview-wrapped-rows (text width &key (style :plain) (prefix "")
                                                     (surface nil))
  "Return TEXT wrapped to WIDTH as rows, each line after PREFIX in STYLE.

SURFACE names a fill style for every row when the site layer ships one."
  (loop for line in (wrap-text text width)
        for row = (list (terminal-span style (concatenate 'string prefix line)))
        append (if surface
                   (apply #'xenolith-preview-surface-row width surface row)
                   (apply #'xenolith-preview-row row))))

(defun xenolith-preview-markdown-rows (text width &key (indent 3))
  "Return TEXT rendered as markdown rows sized to WIDTH, indented by INDENT."
  (let* ((renderer (termdown:markdown-renderer-create :width (max 24 width)))
         (trimmed  (string-right-trim '(#\Space #\Tab #\Newline #\Return) text))
         (margin   (when (plusp indent)
                     (terminal-span :plain
                                    (make-string indent :initial-element #\Space)))))
    (flet ((line-rows (line)
             (loop for row in (application--markdown-rows renderer line)
                   append (append (when margin (list margin))
                                  row
                                  (list (terminal-span :plain
                                                       (string #\Newline)))))))
      (append
       (loop for line in (uiop:split-string trimmed :separator '(#\Newline))
             append (line-rows line))
       (loop for row in (termdown:markdown-render-finish renderer)
             append (append (when margin (list margin))
                            row
                            (list (terminal-span :plain (string #\Newline)))))))))

(defun xenolith-preview (&key (columns 100) (rows 32) (styled-p t)
                              (input "Fix the look")
                              (stream *standard-output*))
  "Print the composed fullscreen frame at COLUMNS by ROWS.

Returns the painted rows, the cursor row, and the cursor column. With
STYLED-P the rows keep their ANSI controls; the printed copy is plain text."
  (let* ((terminal (make-instance 'xenolith-preview-terminal
                                  :columns columns :rows rows))
         (width    (max 20 columns))
         (ui       (terminal-ui-create :terminal terminal
                                       :fullscreen-p t
                                       :prompt "> "
                                       :placeholder *application-placeholder*)))
    (setf (terminal-styled-p terminal) styled-p)
    (with-terminal-ui (ui ui)
      (dolist (section *xenolith-preview-sections*)
        (let ((entry (funcall (cdr section) width)))
          (when entry
            (terminal-ui-append-finalized ui (list :preview (car section)) entry))))
      (terminal-ui-set-status ui "ready")
      (terminal-ui-set-input ui input)
      (multiple-value-bind (frame cursor-row cursor-column)
          (terminal-ui-fullscreen--frame ui nil)
        (format stream "~&xenolith preview ~Dx~D~2%" columns rows)
        (loop for row in frame
              for index from 0
              do (format stream "~2,'0D|~A~%"
                         index
                         (if styled-p (clinedi:ansi-strip row) row)))
        (format stream "~&cursor ~D/~D~%" cursor-row cursor-column)
        (values frame cursor-row cursor-column)))))
(xenolith-preview-register
 :user
 (lambda (width)
   (append (xenolith-preview-row
            (terminal-span :user "You")
            (terminal-span :dim "  2026-10-27 12:30"))
           (xenolith-preview-wrapped-rows
            "Fix the look of this distribution; it should read like a proper terminal agent."
            width
            :surface :surface)
           (xenolith-preview-row))))

(xenolith-preview-register
 :answer
 (lambda (width)
     (xenolith-preview-markdown-rows
      "## Cause

The inline UI prints into the terminal's own scrollback, so per-row chrome has nowhere to live.

- Blocks need a surface
- Diffs need row backgrounds

> Prose stays off the chrome.

---

```sh
ls -l site/modules/look
```

Run `(xenolith-preview)` to compose the frame."
      width)))

(xenolith-preview-register
 :tool
 (lambda (width)
   (append
     (xenolith-preview-block-row
      width :surface
      (terminal-span :plain "  ")
      (terminal-span :dim "· ")
      (terminal-span :success "✓ ")
      (terminal-span :tool "shell.run")
      (terminal-span :dim "  rg -n composer"))
     (xenolith-preview-block-row
      width :surface
      (terminal-span :plain "  ")
      (terminal-span :dim "site/modules/look/composer.lisp:86:"))
    (xenolith-preview-block-row width :surface))))

(xenolith-preview-register
 :diff
 (lambda (width)
   (append
     (xenolith-preview-block-row
      width :surface
      (terminal-span :plain "  ")
      (terminal-span :dim "← Edit site/modules/theme/gruvbox.lisp"))
     (xenolith-preview-block-row
      width :surface-diff-remove
      (terminal-span :plain "  ")
      (terminal-span :failure "- (:surface :bright-black)"))
     (xenolith-preview-block-row
      width :surface-diff-add
      (terminal-span :plain "  ")
      (terminal-span :success "+ (:surface :bg1)"))
    (xenolith-preview-block-row width :surface))))

(xenolith-preview-register
 :thinking
 (lambda (width)
   (append
    (xenolith-preview-block-row
     width :surface-code
     (terminal-span :notice "Thought: 1.4s"))
    (loop for line in (wrap-text
                       "Check whether the composed surface is the right seam before building the blocks."
                       (max 20 (- width 4)))
          append (xenolith-preview-block-row
                  width :surface-code
                  (terminal-span :dim " │ ")
                  (terminal-span :annotation line)))
    (xenolith-preview-block-row width :surface-code))))
