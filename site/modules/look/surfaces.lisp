(in-package #:autolith)

;;;; Xenolith look: filled surfaces.
;;;;
;;;; Tool output, diffs, code, and reasoning sit on filled
;;;; bands instead of bare text. A transcript row renders from semantic spans, so
;;;; the fill is decided at paint time: a row states its fill by leading with a
;;;; zero-cell span whose style names the surface, and the renderer paints that
;;;; run with the style's background. The row keeps its span representation, so
;;;; width, cursor, and click paths still see plain text.
;;;;
;;;; Builders pad a row to the terminal width while they know the width, because
;;;; plain and styled text must agree. The renderer only paints.
;;;;
;;;; Disable with XENOLITH_SURFACES=off. Verify with (xenolith-report), which
;;;; lists look/surfaces, and (xenolith-preview), which draws the bands.

(defparameter *xenolith-surface-styles*
  '(:surface :surface-code :surface-diff-add :surface-diff-remove)
  "The styles that name a row fill.")

(defvar *xenolith-original-render-spans* nil
  "The span renderer underneath this module's wrapper, or NIL.")

(defvar *xenolith-surfaces-render-wrapper* nil
  "The span renderer wrapper this module installed, for reload detection.")

(defvar *xenolith-surfaces-previous-tool-row-spans* nil
  "The tool row builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-surfaces-tool-row-wrapper* nil
  "The tool row wrapper this module installed, for reload detection.")

(defvar *xenolith-surfaces-previous-reasoning-rows* nil
  "The reasoning row builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-surfaces-reasoning-wrapper* nil
  "The reasoning row wrapper this module installed, for reload detection.")

(defvar *xenolith-surfaces-previous-user-entry* nil
  "The user message entry builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-surfaces-user-entry-wrapper* nil
  "The user message entry wrapper this module installed, for reload detection.")

(defvar *xenolith-surfaces-previous-tool-entry* nil
  "The tool entry builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-surfaces-tool-entry-wrapper* nil
  "The tool entry wrapper this module installed, for reload detection.")

(defvar *xenolith-surfaces-previous-syntax-span* nil
  "The syntax span builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-surfaces-syntax-wrapper* nil
  "The syntax span wrapper this module installed, for reload detection.")

(defvar *xenolith-surfaces-failure* nil
  "The most recent condition a surfaces wrapper caught, or NIL.")

(defparameter *xenolith-install-surfaces-p*
  (not (equal (uiop:getenv "XENOLITH_SURFACES") "off"))
  "Whether the site layer paints row surfaces.")

(defparameter *xenolith-surface-inset* 5
  "Columns of margin on each side of a block surface.")

(defparameter *xenolith-surface-padding* 2
  "Columns of padding inside a block surface whose row arrived indented.")

(defun xenolith--split-indent (spans limit)
  "Return SPANS with up to LIMIT leading plain-space cells removed.

The second value reports whether a cell was removed."
  (let ((remaining (copy-list spans))
        (left      limit)
        (removed-p nil))
    (loop while (and remaining (plusp left))
          for span = (first remaining)
          do (cond
               ((and (terminal-span-p span)
                     (eq (terminal-span-style span) ':plain)
                     (plusp (length (terminal-span-text span)))
                     (every (lambda (character) (char= character #\Space))
                            (terminal-span-text span)))
                (let* ((text (terminal-span-text span))
                       (take (min left (length text))))
                  (setf removed-p t)
                  (if (= take (length text))
                      (pop remaining)
                      (setf (first remaining)
                            (terminal-span ':plain (subseq text take))))
                  (decf left take)))
               (t
                (return))))
    (values remaining removed-p)))

(defun xenolith-surface-row-spans (row width style &key (inset 0))
  "Return ROW as a row filled with STYLE across WIDTH, inset by INSET columns.

The row's own leading plain spaces cover the margin when they are long enough,
so an indented body keeps its text position while the fill starts at the margin.
The fill spans the columns between the two margins; the marker stays an empty
span, so the row's plain text is unchanged."
  (let* ((width   (max 1 width))
         (inset   (max 0 (min inset (floor (1- width) 2))))
         (content (or row nil)))
      (multiple-value-bind (body)
          (if (plusp inset)
              (xenolith--split-indent content inset)
              content)
        (let* ((padding (if (plusp inset) *xenolith-surface-padding* 0))
               (inner   (max 1 (- width (* 2 inset) padding))))
        (append
         (when (plusp inset)
           (list (terminal-span :plain
                                (make-string inset :initial-element #\Space))))
         (list (terminal-span style ""))
         (when (plusp padding)
           (list (terminal-span style
                                (make-string padding :initial-element #\Space))))
         (terminal--pad-spans body inner :style style))))))

(defun xenolith-surface-marker-p (span)
  "Return true when SPAN states a row fill."
  (and (terminal-span-p span)
       (zerop (length (terminal-span-text span)))
       (member (terminal-span-style span) *xenolith-surface-styles*)))

(defun xenolith--blank-span-p (span)
  "Return true when SPAN holds only padding."
  (and (terminal-span-p span)
       (every (lambda (character)
                (member character '(#\Space #\Tab) :test #'char=))
              (terminal-span-text span))))

(defun xenolith-surface-row-p (row)
  "Return true when ROW states a fill before its first visible cell.

A filled row keeps its inset as plain spaces ahead of the fill marker, so the
margin stays unpainted; checking only the first span misses every inset row.
Missing the marker makes the block builder fill the row a second time, and the
second surface hides the first one behind the body text."
  (loop for span in row
        do (cond
             ((xenolith-surface-marker-p span)
              (return t))
             ((xenolith--blank-span-p span))
             (t
              (return nil)))))

(defun xenolith--newline-span-p (span)
  "Return true when SPAN separates two transcript rows."
  (and (terminal-span-p span)
       (let ((text (terminal-span-text span)))
         (and (= (length text) 1)
              (char= (char text 0) #\Newline)))))

(defun xenolith-fill-rendered-row (terminal row style &key (reserve 0))
  "Return rendered ROW filled with STYLE across TERMINAL's width.

RESERVE columns stay unpainted so a caller whose frame re-wraps the row keeps
it on one line."
  (let* ((width   (max 1 (- (terminal-columns terminal) reserve)))
         (plain   (terminal-rendered-row-text row))
         (padding (max 0 (- width (text-cell-width plain))))
         (fill    (or (terminal-style-sequence style) ""))
         (spaces  (make-string padding :initial-element #\Space)))
    (terminal--make-rendered-row
     (concatenate 'string plain spaces)
     (concatenate 'string
                  fill
                    (xenolith--fill-after-resets (terminal-rendered-row-display row) fill)
                  spaces
                  *terminal-style-reset*))))

(defun xenolith-diff-row-style (spans)
  "Return the fill for tool body SPANS.

A line whose first character is a diff sign followed by a space, or a bare
sign, is an added or removed line. Numbered output such as +12 is not a diff,
and the +++/--- file headers take the code surface."
  (let* ((text   (string-left-trim '(#\Space #\Tab) (terminal--spans-text spans)))
         (length (length text))
         (first  (and (plusp length) (char text 0)))
         (second (and (> length 1) (char text 1)))
         (tail-p (or (null second) (member second '(#\Space #\Tab)))))
    (cond
      ((null first) ':surface)
      ((and (char= first #\+) (eq second #\+)) ':surface-code)
      ((and (char= first #\-) (eq second #\-)) ':surface-code)
      ((and (char= first #\+) tail-p) ':surface-diff-add)
      ((and (char= first #\-) tail-p) ':surface-diff-remove)
      (t ':surface))))

(defun xenolith--render-surfaced-spans (terminal spans &key background)
  "Render SPANS, painting every run that states a fill with its background.

Runs end at the next marker or newline, so several rows in one entry keep their
own fills. SPANS without a marker render exactly as the tracked function does."
  (let ((original *xenolith-original-render-spans*))
    (if (not (some #'xenolith-surface-marker-p spans))
        (funcall original terminal spans :background background)
        (with-output-to-string (out)
          (let ((run nil)
                (fill nil))
            (labels ((flush ()
                       (when run
                         (write-string
                          (funcall original terminal (nreverse run)
                                   :background fill)
                          out)
                         (setf run nil))))
              (dolist (span spans)
                (cond
                  ((xenolith-surface-marker-p span)
                   (flush)
                   (setf fill (terminal-span-style span)))
                    ((xenolith--newline-span-p span)
                   (flush)
                   (setf fill nil)
                   (write-string (funcall original terminal (list span)) out))
                  (t
                   (push span run))))
              (flush)))))))

(defun xenolith--surface-columns (application &key (reserve 1))
  "Return APPLICATION's row width for a filled transcript row.

One column stays unpainted so a streamed row never lands on the terminal's
automatic wrap cell."
  (let ((terminal (terminal-ui-terminal (application-ui application))))
    (max 1 (- (terminal-columns terminal) reserve))))

(defparameter *xenolith-surfaces-role-styles*
  '((:added-gutter   . :surface-diff-add)
    (:removed-gutter . :surface-diff-remove)
    (:variable       . :syntax-property)
    (:operator       . :syntax-escape)
    (:punctuation    . :plain))
  "Colourlisp and Colordiff roles that take another style.

A diff sign takes the surface its row already carries, so an added or removed
line reads in the theme's plain green or red. The three syntax roles take the
style the closed set carries; the bundled grammars rarely emit them.")

(defun xenolith--toned-spans (spans &optional (tone :annotation))
  "Return SPANS with plain text carrying the body TONE.

The fill paints the row; the tone separates body text from prose without naming
a style the closed set does not carry. A NIL TONE keeps plain runs as they are,
so the fill's own foreground reaches them instead of the quiet tone."
  (if (null tone)
      spans
      (loop for span in spans
            collect (if (and (terminal-span-p span)
                             (eq (terminal-span-style span) ':plain))
                        (terminal-span tone (terminal-span-text span))
                        span))))

(defparameter *xenolith-surfaces-diff-fills*
  '(:surface-diff-add :surface-diff-remove)
  "Fills whose row already carries the added or removed foreground.")

(defun xenolith--surfaced-tool-row-spans (application row)
  "Return tool body ROW filled as a surface, tinted when it is a diff line.

An added or removed line keeps its plain runs plain, so the fill's green or red
foreground reaches the text beside the sign; every other row takes the quiet
body tone."
  (let ((previous *xenolith-surfaces-previous-tool-row-spans*))
    (handler-case
        (let* ((spans   (if previous (funcall previous application row) row))
               (columns (xenolith--surface-columns application))
               (style   (xenolith-diff-row-style spans))
               (spans   (xenolith--toned-spans
                         spans
                         (unless (member style *xenolith-surfaces-diff-fills*)
                           :annotation))))
          (xenolith-surface-row-spans spans columns style
                                      :inset *xenolith-surface-inset*))
      (serious-condition (condition)
        (setf *xenolith-surfaces-failure* condition)
        (xenolith-log "surfaces: tool row FAILED ~A" condition)
        (if previous (funcall previous application row) row)))))

(defun xenolith--surfaced-reasoning-rows (application summary)
  "Return SUMMARY as code-surfaced reasoning rows."
  (let ((previous *xenolith-surfaces-previous-reasoning-rows*))
    (handler-case
        (let ((columns (xenolith--surface-columns application))
              (rows    (if previous
                           (funcall previous application summary)
                           (list (list (terminal-span :plain summary))))))
            (mapcar (lambda (row)
                      (xenolith-surface-row-spans
                       (xenolith--toned-spans row) columns :surface-code
                       :inset *xenolith-surface-inset*))
                    rows))
      (serious-condition (condition)
        (setf *xenolith-surfaces-failure* condition)
        (xenolith-log "surfaces: reasoning rows FAILED ~A" condition)
        (if previous
            (funcall previous application summary)
            (list (list (terminal-span :plain summary))))))))

(defun xenolith--user-message-body (application text)
  "Return TEXT as user message body rows filled with the user surface.

The fill hugs the message instead of running to the frame edge: one blank
surface row above and below, and a panel as wide as the longest line plus two
columns, so the fill stops just past the text and grows with wrapped lines."
  (let* ((limit (xenolith--surface-columns application))
         (lines (uiop:split-string (application--indented-body application text)
                                   :separator (list #\Newline))))
    (when (and lines (string= (car (last lines)) ""))
      (setf lines (butlast lines)))
    (let* ((used  (loop for line in lines maximize (text-cell-width line)))
           (width (max 1 (min limit (+ used 2)))))
      (labels ((filled (line-text)
                 ;; A fresh row each call: NCONC links into the last cell, so a
                 ;; shared blank row would come to point into the next row.
                 (xenolith-surface-row-spans
                  (when line-text (list (terminal-span ':plain line-text)))
                  width ':surface)))
        (loop for line in (append (list nil) lines (list nil))
              for first-p = t then nil
              nconc (append (unless first-p
                              (list (terminal-span ':plain (string #\Newline))))
                            (filled line)))))))

(defun xenolith--filled-user-message-entry (application text timestamp)
  "Return the user message entry with its rows filled like userMessageBg."
  (let ((previous *xenolith-surfaces-previous-user-entry*))
    (handler-case
        (application--message-entry
         application
         :role      ':user
         :timestamp timestamp
         :body      (xenolith--user-message-body application text))
      (serious-condition (condition)
        (setf *xenolith-surfaces-failure* condition)
        (xenolith-log "surfaces: user entry FAILED ~A" condition)
        (if previous
            (funcall previous application text timestamp)
            (list (terminal-span ':plain
                                 (application--indented-body application text))))))))

(defun xenolith--mapped-role-span (role text)
  "Return one span for ROLE and TEXT, preferring the styles the look wants."
  (funcall *xenolith-surfaces-previous-syntax-span*
           (or (cdr (assoc role *xenolith-surfaces-role-styles*)) role)
           text))

(defun xenolith--split-rows (spans)
  "Return SPANS split at newline spans into per-row span lists."
  (let ((rows '())
        (row  '()))
    (dolist (span spans)
        (if (xenolith--newline-span-p span)
          (progn (push (nreverse row) rows)
                 (setf row '()))
          (push span row)))
    (push (nreverse row) rows)
    (nreverse rows)))

(defun xenolith--filled-block-entry (application entry)
  "Return ENTRY with every unfilled row inset and filled like a block.

Rows a builder already marked keep their fill; the tool header and its detail
rows take the same inset, so the block reads as one panel. A row that cannot be
padded stays as the builder left it."
  (let* ((width (xenolith--surface-columns application))
         (rows  (xenolith--split-rows entry))
         (rows  (if (and rows (null (car (last rows))))
                    (butlast rows)
                    rows)))
    (loop for row in rows
          for first-p = t then nil
          nconc (append (unless first-p
                          (list (terminal-span ':plain (string #\Newline))))
                        (cond
                          ((xenolith-surface-row-p row) row)
                          (t (handler-case
                                 (xenolith-surface-row-spans
                                  row width ':surface
                                  :inset *xenolith-surface-inset*)
                               (serious-condition () row))))))))

(defun xenolith--filled-tool-entry (application &rest arguments)
  "Return the tool entry with its header and detail rows inside the block."
  (let ((previous *xenolith-surfaces-previous-tool-entry*))
    (handler-case
        (xenolith--filled-block-entry
         application
         (apply previous application arguments))
      (serious-condition (condition)
        (setf *xenolith-surfaces-failure* condition)
        (xenolith-log "surfaces: tool entry FAILED ~A" condition)
        (apply previous application arguments)))))
(defun xenolith--install-surfaces ()
  "Wrap the tracked row builders and renderer with the surface treatment."
  (when (fboundp 'terminal--render-spans)
    (xenolith--chain 'terminal--render-spans
                     '*xenolith-original-render-spans*
                     '*xenolith-surfaces-render-wrapper*
                     #'xenolith--render-surfaced-spans))
  (when (fboundp 'application--tool-row-spans)
    (xenolith--chain 'application--tool-row-spans
                     '*xenolith-surfaces-previous-tool-row-spans*
                     '*xenolith-surfaces-tool-row-wrapper*
                     #'xenolith--surfaced-tool-row-spans))
  (when (fboundp 'application--reasoning-summary-rows)
    (xenolith--chain 'application--reasoning-summary-rows
                     '*xenolith-surfaces-previous-reasoning-rows*
                     '*xenolith-surfaces-reasoning-wrapper*
                     #'xenolith--surfaced-reasoning-rows))
  (when (fboundp 'application--user-message-entry)
    (xenolith--chain 'application--user-message-entry
                     '*xenolith-surfaces-previous-user-entry*
                     '*xenolith-surfaces-user-entry-wrapper*
                     #'xenolith--filled-user-message-entry))
  (when (fboundp 'application--tool-entry)
    (xenolith--chain 'application--tool-entry
                     '*xenolith-surfaces-previous-tool-entry*
                     '*xenolith-surfaces-tool-entry-wrapper*
                     #'xenolith--filled-tool-entry))
  (when (fboundp 'syntax--terminal-span)
    (xenolith--chain 'syntax--terminal-span
                     '*xenolith-surfaces-previous-syntax-span*
                     '*xenolith-surfaces-syntax-wrapper*
                     #'xenolith--mapped-role-span))
  (values))

(defun xenolith--uninstall-surfaces ()
  "Restore the definitions the surface treatment wrapped."
  (xenolith--unchain 'terminal--render-spans
                     '*xenolith-original-render-spans*
                     '*xenolith-surfaces-render-wrapper*)
  (xenolith--unchain 'application--tool-row-spans
                     '*xenolith-surfaces-previous-tool-row-spans*
                     '*xenolith-surfaces-tool-row-wrapper*)
  (xenolith--unchain 'application--reasoning-summary-rows
                     '*xenolith-surfaces-previous-reasoning-rows*
                     '*xenolith-surfaces-reasoning-wrapper*)
  (xenolith--unchain 'application--user-message-entry
                     '*xenolith-surfaces-previous-user-entry*
                     '*xenolith-surfaces-user-entry-wrapper*)
  (xenolith--unchain 'application--tool-entry
                     '*xenolith-surfaces-previous-tool-entry*
                     '*xenolith-surfaces-tool-entry-wrapper*)
  (xenolith--unchain 'syntax--terminal-span
                     '*xenolith-surfaces-previous-syntax-span*
                     '*xenolith-surfaces-syntax-wrapper*)
  (values))

(if *xenolith-install-surfaces-p*
    (xenolith--install-surfaces)
    (xenolith--uninstall-surfaces))
