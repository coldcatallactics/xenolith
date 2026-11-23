(in-package #:autolith)

;;;; Xenolith look: markdown chrome.
;;;;
;;;; Blockquotes and rules are quieted. The renderer emits a quoted line and a
;;;; rule as plain text, so this module rewrites the spans it returns without
;;;; touching text or width: a quoted line takes the theme's grey and a rule the
;;;; dim tone.
;;;;
;;;; XENOLITH_MARKDOWN=off skips the wrap at load. A serious condition falls
;;;; back to the tracked renderer and is kept in *xenolith-markdown-failure*.

(defparameter *xenolith-markdown-quote-style* :timestamp-time
  "Style for a quoted line: the theme's grey.")

(defparameter *xenolith-markdown-rule-style* :dim
  "Style for a horizontal rule: the dim tone.")

(defvar *xenolith-original-markdown-render-line* nil
  "The markdown line renderer underneath this module's wrapper, or NIL.")

(defvar *xenolith-markdown-wrapper* nil
  "The markdown line wrapper this module installed, for reload detection.")

(defvar *xenolith-markdown-failure* nil
  "The most recent condition the markdown wrapper caught, or NIL.")

(defparameter *xenolith-install-markdown-p*
  (not (equal (uiop:getenv "XENOLITH_MARKDOWN") "off"))
  "Whether the site layer adds the markdown chrome.")

(defun xenolith--markdown-trimmed (text)
  "Return TEXT without surrounding space and tabs."
  (string-trim '(#\Space #\Tab) text))

(defun xenolith--markdown-quote-p (line)
  "Return true when LINE opens a blockquote."
  (let ((trimmed (xenolith--markdown-trimmed line)))
    (and (plusp (length trimmed))
         (char= (char trimmed 0) #\>))))

(defun xenolith--markdown-rule-p (line)
  "Return true when LINE is a horizontal rule."
  (let ((trimmed (xenolith--markdown-trimmed line)))
    (and (>= (length trimmed) 3)
         (let ((marker (char trimmed 0)))
           (and (member marker '(#\- #\* #\_))
                (every (lambda (character) (char= character marker)) trimmed))))))

(defun xenolith--markdown-recolour (row style)
  "Return ROW with its plain spans wearing STYLE."
  (mapcar (lambda (span)
            (if (and (terminal-span-p span)
                     (eq (terminal-span-style span) ':plain))
                (terminal-span style (terminal-span-text span))
                span))
          row))

(defun xenolith--markdown-render-line (renderer line)
  "Return LINE rendered with the look's quote and rule chrome."
  (let ((previous *xenolith-original-markdown-render-line*))
    (handler-case
        (let ((rows (funcall previous renderer line)))
          (cond
            ((xenolith--markdown-quote-p line)
             (mapcar (lambda (row)
                       (xenolith--markdown-recolour
                        row *xenolith-markdown-quote-style*))
                     rows))
            ((xenolith--markdown-rule-p line)
             (mapcar (lambda (row)
                       (xenolith--markdown-recolour
                        row *xenolith-markdown-rule-style*))
                     rows))
            (t rows)))
      (serious-condition (condition)
        (setf *xenolith-markdown-failure* condition)
        (xenolith-log "markdown: FAILED ~A" condition)
        (funcall previous renderer line)))))

(when *xenolith-install-markdown-p*
  (when (fboundp 'termdown:markdown-render-line)
    (xenolith--chain 'termdown:markdown-render-line
                     '*xenolith-original-markdown-render-line*
                     '*xenolith-markdown-wrapper*
                     #'xenolith--markdown-render-line)))
