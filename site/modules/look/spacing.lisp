(in-package #:autolith)

;;;; Xenolith look: transcript spacing.
;;;;
;;;; Response text is padded with three columns on each side and tool bodies sit
;;;; two columns further in than that text. Autolith indents both by two columns
;;;; and wraps to the terminal width; this module widens the two tracked row
;;;; builders and the wrap widths that depend on them.

(defparameter *xenolith-assistant-indent* 3
  "Columns of margin on each side of transcript body text.")

(defparameter *xenolith-tool-indent* 5
  "Columns of margin left of tool body rows.")

(defvar *xenolith-original-indented-body* nil
  "The body builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-original-tool-row-spans* nil
  "The tool row builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-spacing-indented-body-wrapper* nil
  "The body wrapper this module installed, for reload detection.")

(defvar *xenolith-spacing-tool-row-wrapper* nil
  "The tool row wrapper this module installed, for reload detection.")

(defvar *xenolith-spacing-failure* nil
  "The most recent condition a spacing wrapper caught, or NIL.")

(defparameter *xenolith-install-spacing-p*
  (not (equal (uiop:getenv "XENOLITH_SPACING") "off"))
  "Whether the site layer widens transcript spacing.")

(defun xenolith--spacing-columns (application)
  "Return APPLICATION's terminal width, or NIL when it has no terminal."
  (let ((ui (application-ui application)))
    (and ui (terminal-ui-terminal ui)
         (terminal-columns (terminal-ui-terminal ui)))))

(defun xenolith--assistant-indented-body (application text)
  "Return TEXT wrapped with the configured margin on both sides."
  (handler-case
      (let* ((columns (or (xenolith--spacing-columns application) 80))
             (width   (max 20 (- columns (* 2 *xenolith-assistant-indent*))))
             (margin  (make-string *xenolith-assistant-indent*
                                   :initial-element #\Space))
             (rows    (wrap-text
                       (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                          (sanitize-text text))
                       width)))
        (format nil "~{~A~^~%~}"
                (mapcar (lambda (line)
                          (concatenate 'string margin line))
                        rows)))
    (serious-condition (condition)
      (setf *xenolith-spacing-failure* condition)
      (xenolith-log "spacing: assistant FAILED ~A" condition)
      (funcall *xenolith-original-indented-body* application text))))

(defun xenolith--tool-row-spans (application row)
  "Return ROW indented for the tool rail and clipped to the terminal."
  (handler-case
      (let* ((columns       (or (xenolith--spacing-columns application) 80))
             (maximum-width (max 1 (- columns *xenolith-tool-indent*)))
             (indented      (append
                             (list (terminal-span
                                    :plain
                                    (make-string *xenolith-tool-indent*
                                                 :initial-element #\Space)))
                             row)))
        (if (<= (terminal--spans-width indented) maximum-width)
            indented
            (append (terminal--clip-spans indented (max 0 (1- maximum-width)))
                    (list (terminal-span :dim "…")))))
    (serious-condition (condition)
      (setf *xenolith-spacing-failure* condition)
      (xenolith-log "spacing: tool FAILED ~A" condition)
      (funcall *xenolith-original-tool-row-spans* application row))))

(when (fboundp 'application--indented-body)
  (if *xenolith-install-spacing-p*
      (xenolith--chain 'application--indented-body
                       '*xenolith-original-indented-body*
                       '*xenolith-spacing-indented-body-wrapper*
                       #'xenolith--assistant-indented-body)
      (xenolith--unchain 'application--indented-body
                         '*xenolith-original-indented-body*
                         '*xenolith-spacing-indented-body-wrapper*)))

(when (fboundp 'application--tool-row-spans)
  (if *xenolith-install-spacing-p*
      (xenolith--chain 'application--tool-row-spans
                       '*xenolith-original-tool-row-spans*
                       '*xenolith-spacing-tool-row-wrapper*
                       #'xenolith--tool-row-spans)
      (xenolith--unchain 'application--tool-row-spans
                         '*xenolith-original-tool-row-spans*
                         '*xenolith-spacing-tool-row-wrapper*)))
