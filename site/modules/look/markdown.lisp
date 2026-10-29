(in-package #:autolith)

;;;; Xenolith look: markdown chrome.
;;;;
;;;; Blockquotes and rules are quieted and fenced code blocks are bordered. The
;;;; pinned renderer emits a quoted line and a rule as plain text and indents a
;;;; fenced row by two spaces. This module rewrites the spans the renderer
;;;; returns without touching text or width: a quoted line takes the theme's
;;;; grey, a rule the dim tone, and a fenced row swaps its two-space indent for
;;;; a border glyph.
;;;;
;;;; XENOLITH_MARKDOWN=off skips the wrap at load. A serious condition falls
;;;; back to the tracked renderer and is kept in *xenolith-markdown-failure*.

(defparameter *xenolith-markdown-quote-style* :timestamp-time
  "Style for a quoted line: the theme's grey.")

(defparameter *xenolith-markdown-rule-style* :dim
  "Style for a horizontal rule: the dim tone.")

(defparameter *xenolith-markdown-border* "│ "
  "The left border a fenced code row takes in place of two spaces.")

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

(defun xenolith--markdown-border (row)
  "Return ROW with its two-space indent replaced by the code border."
  (let ((first (first row)))
    (if (and (terminal-span-p first)
             (>= (length (terminal-span-text first)) 2)
             (string= (subseq (terminal-span-text first) 0 2) "  "))
        (cons (terminal-span (terminal-span-style first)
                             (concatenate 'string
                                          *xenolith-markdown-border*
                                          (subseq (terminal-span-text first) 2)))
              (rest row))
        row)))

(defun xenolith--markdown-render-line (renderer line)
  "Return LINE rendered with the look's quote, rule, and code chrome."
  (let ((previous *xenolith-original-markdown-render-line*))
    (handler-case
        (let* ((code-before (termdown::markdown-renderer-code-open-p renderer))
               (rows        (funcall previous renderer line))
               (code-p      (or code-before
                                (termdown::markdown-renderer-code-open-p
                                 renderer))))
          (cond
            (code-p
             (mapcar #'xenolith--markdown-border rows))
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
  (when (fboundp 'terminal-markdown-render-line)
    (xenolith--chain 'terminal-markdown-render-line
                     '*xenolith-original-markdown-render-line*
                     '*xenolith-markdown-wrapper*
                     #'xenolith--markdown-render-line)))
