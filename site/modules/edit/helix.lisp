(in-package #:autolith)

;;;; Xenolith edit: Helix modality over the composer line editor.
;;;;
;;;; The site layer owns the editor keymap in normal and command modes and
;;;; leaves insert mode on the tracked Clinedi bindings. Normal mode follows
;;;; Helix's selection-first model: an anchor and a cursor span a selection that
;;;; motions extend, x selects the current line, d, c, and y act on the
;;;; selection, and p pastes after it. Command mode writes a line beginning with
;;;; ":" and insert mode opens it on a colon that starts an empty prompt; the
;;;; line submits through the ordinary slash-command path, so ":settings" and
;;;; "/settings" are the same command, and its candidates come from the same
;;;; registry with the colon standing in for the slash. Insert is the startup
;;;; mode.
;;;;
;;;; Mouse reporting is released outside text entry by default, so a plain drag
;;;; in normal or select mode runs the terminal's own selection;
;;;; XENOLITH_HELIX_MOUSE=scroll keeps the reports there for the wheel.

(defparameter *xenolith-helix-bound-events*
  '(:escape :insert :insert-newline :paste :submit
    :left :right :word-left :word-right :home :end :up :down
    :backspace :delete
    :history-previous :history-next
    :kill-to-end :kill-line :kill-word :kill-word-right
    :line :toggle-word-delimiter-mode)
  "Editor events a mode may override, restored from the tracked bindings.

Every event the tracked keymap binds that can move the cursor or change the
draft is claimed in normal and select mode. An event left on its tracked
binding would move the cursor behind the selection's back and paint a range
nothing had selected.")

(defvar *xenolith-helix-default-bindings* nil
  "The tracked default line-editor bindings, captured once for restoration.")

(defvar *xenolith-original-terminal-ui--matching-completions* nil
  "The tracked terminal-ui--matching-completions captured before the first wrap.")

(defvar *xenolith-helix-states* (make-hash-table :test #'eq)
  "Editor state for every composer the site layer installed modality into.")

(defvar *xenolith-helix-failure* nil
  "The most recent condition a Helix hook caught, or NIL.")

(defparameter *xenolith-install-helix-p*
  (not (equal (uiop:getenv "XENOLITH_HELIX") "off"))
  "Whether the site layer installs Helix modality.")

(defparameter *xenolith-helix-mouse-scroll-p*
  (equal (uiop:getenv "XENOLITH_HELIX_MOUSE") "scroll")
  "Whether normal and select mode keep the viewport's mouse reports.

The wheel is a mouse report, so keeping it in those modes also takes the drag
away from the terminal: text selection then needs the terminal's Shift bypass.
Released by default, a plain drag selects, and PageUp and PageDown scroll the
transcript. Set XENOLITH_HELIX_MOUSE=scroll to keep the wheel in those modes.")

(defparameter *xenolith-helix-motion-events*
  '((:left . #\h) (:right . #\l) (:up . #\k) (:down . #\j)
    (:word-left . #\b) (:word-right . #\w))
  "Tracked movement events and the Helix key each stands for in normal mode.

The tracked editor moves its cursor for these events without touching the
selection, which left a stale anchor behind and painted a range nothing had
selected.")

(defstruct (xenolith-helix-state (:constructor xenolith-helix-state-create)
                                (:copier nil))
  (ui nil)
  (editor nil)
  (mode :insert)
  (count nil)
  (operator nil)
  (pending-prefix nil)
  (anchor nil)
  (kill "")
  (command-history '())
  (command-index nil)
  (command-draft ""))


;;; These three are defined after the code that calls them; the declarations
;;; keep the compiler from reporting them as undefined functions.
(declaim (ftype (function (t) *) xenolith-helix--invalidate-prompt))
(declaim (ftype (function (t) *) xenolith-helix--refresh-status))
(declaim (ftype (function (t) *) xenolith-helix--paint-mode-presentation))
(unless *xenolith-helix-default-bindings*
  (when (fboundp 'clinedi:default-line-editor-keymap)
    (setf *xenolith-helix-default-bindings*
          (clinedi:keymap-bindings (clinedi:default-line-editor-keymap)))))

;;;; -- Mode bindings --

(defun xenolith-helix--mode-bindings (mode)
  "Return MODE's local event bindings."
  (case mode
    (:insert
     '((:escape . xenolith-helix--insert-escape)))
      ((:normal :select)
       '((:escape . xenolith-helix--normal-escape)
         (:insert . xenolith-helix--normal-key)
         (:paste . xenolith-helix--normal-paste)
         (:submit . xenolith-helix--normal-enter)
         (:backspace . xenolith-helix--normal-ignore)
         (:delete . xenolith-helix--normal-delete)
         (:left . xenolith-helix--normal-motion-key)
         (:right . xenolith-helix--normal-motion-key)
         (:up . xenolith-helix--normal-motion-key)
         (:down . xenolith-helix--normal-motion-key)
         (:word-left . xenolith-helix--normal-motion-key)
         (:word-right . xenolith-helix--normal-motion-key)
         (:home . xenolith-helix--normal-line-key)
         (:end . xenolith-helix--normal-line-key)
         (:history-previous . xenolith-helix--normal-ignore)
         (:history-next . xenolith-helix--normal-ignore)
         (:insert-newline . xenolith-helix--normal-ignore)
         (:kill-to-end . xenolith-helix--normal-ignore)
         (:kill-line . xenolith-helix--normal-ignore)
         (:kill-word . xenolith-helix--normal-ignore)
         (:kill-word-right . xenolith-helix--normal-ignore)
         (:line . xenolith-helix--normal-ignore)
         (:toggle-word-delimiter-mode . xenolith-helix--normal-ignore)))
    (:command
     '((:escape . xenolith-helix--command-escape)
       (:submit . xenolith-helix--command-submit)
       (:up . xenolith-helix--command-up)
       (:down . xenolith-helix--command-down)
       (:backspace . xenolith-helix--command-backspace)))
    (t nil)))

(defun xenolith-helix--restore-defaults (keymap)
  "Restore every event in *XENOLITH-HELIX-BOUND-EVENTS* to its tracked binding."
  (dolist (event *xenolith-helix-bound-events*)
    (let ((entry (assoc event *xenolith-helix-default-bindings* :test #'equal)))
      (if entry
          (clinedi:keymap-bind keymap event (cdr entry))
          (clinedi:keymap-unbind keymap event))))
  keymap)

(defun xenolith-helix--apply-mode (state)
  "Reinstall STATE's mode bindings on its editor keymap."
  (let ((editor (xenolith-helix-state-editor state)))
    (when (and editor (fboundp 'clinedi:line-editor-keymap))
      (let ((keymap (clinedi:line-editor-keymap editor)))
        (xenolith-helix--restore-defaults keymap)
        (loop for (event . command) in (xenolith-helix--mode-bindings
                                        (xenolith-helix-state-mode state))
              do (clinedi:keymap-bind keymap event command))))
    (values :continue nil)))

(defun xenolith-helix--normal-cursor (text position)
  "Return POSITION clamped to a valid gap in TEXT, including its end."
  (max 0 (min position (length text))))

(defun xenolith-helix--block-position (text anchor head)
  "Return the left edge of the block cursor for ANCHOR and HEAD in TEXT."
  (let ((anchor (xenolith-helix--normal-cursor text anchor))
        (head   (xenolith-helix--normal-cursor text head)))
    (if (> head anchor)
        (max 0 (1- head))
        head)))

(defun xenolith-helix--put-cursor-extend (text anchor head position)
  "Extend the range ANCHOR-HEAD to POSITION as Helix's put_cursor does.

POSITION is the left edge of the destination block cursor.  The original
anchor is kept unless the head crosses it, and a destination at or right
of the anchor gains a one-character head block."
  (let* ((length   (length text))
         (position (max 0 (min position (max 0 (1- length)))))
         (anchor   (xenolith-helix--normal-cursor text anchor))
         (head     (xenolith-helix--normal-cursor text head))
         (anchor   (cond
                     ((and (>= head anchor) (< position anchor))
                      (min length (1+ anchor)))
                     ((and (< head anchor) (>= position anchor))
                      (max 0 (1- anchor)))
                     (t anchor))))
    (if (<= anchor position)
        (values anchor (min length (1+ position)))
        (values anchor position))))

(defun xenolith-helix--range (state expand-empty-p)
  "Return STATE's selection as start and exclusive end.

Helix ranges are gap indices: the anchor and head may lie in either order,
and the range covers the gaps between them.  An empty range covers the
character at its head when EXPAND-EMPTY-P is true, matching Helix's block
cursor and minimum-width editing operations."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor))
         (length (length text))
         (head   (xenolith-helix--normal-cursor
                  text (line-editor-cursor editor)))
         (anchor (xenolith-helix--normal-cursor
                  text (or (xenolith-helix-state-anchor state) head))))
    (cond
      ((< head anchor) (values head anchor))
      ((> head anchor) (values anchor head))
      (expand-empty-p  (values head (min length (1+ head))))
      (t               (values head head)))))

(defun xenolith-helix--selection (state)
  "Return the start and exclusive end of STATE's editable selection."
  (xenolith-helix--range state t))

(defun xenolith-helix--highlight-selection (state)
  "Return the start and exclusive end of STATE's visible selection."
  (xenolith-helix--range state nil))

(defun xenolith-helix--set-mode (state mode)
  "Switch STATE to MODE, install its bindings, and refresh its presentation.

Entering normal mode from select collapses to the block cursor, as Helix's
normal_mode command does; entering it from insert keeps the insertion point."
  (let ((previous (xenolith-helix-state-mode state)))
    (setf (xenolith-helix-state-mode state) mode
          (xenolith-helix-state-count state) nil
          (xenolith-helix-state-operator state) nil
          (xenolith-helix-state-pending-prefix state) nil)
    (let* ((editor (xenolith-helix-state-editor state))
           (text   (line-editor-text editor))
           (head   (xenolith-helix--normal-cursor
                    text (line-editor-cursor editor)))
           (anchor (xenolith-helix--normal-cursor
                    text (or (xenolith-helix-state-anchor state) head))))
      (cond
        ((eq mode :normal)
         (let ((point (if (eq previous :select)
                          (xenolith-helix--block-position text anchor head)
                          head)))
           (line-editor-set-text editor text :cursor point)
           (setf (xenolith-helix-state-anchor state) point)))
        ((eq mode :select)
         (line-editor-set-text editor text :cursor head)
         (setf (xenolith-helix-state-anchor state) anchor))
        (t
         (setf (xenolith-helix-state-anchor state) nil)))))
  (xenolith-helix--apply-mode state)
  (xenolith-helix--invalidate-prompt state)
  (xenolith-helix--refresh-status state)
  (values :continue nil))
(defun xenolith-helix--exit-select-mode (state)
  "Return STATE to normal mode without collapsing its selection."
  (when (eq (xenolith-helix-state-mode state) :select)
    (setf (xenolith-helix-state-mode state) :normal
          (xenolith-helix-state-count state) nil
          (xenolith-helix-state-pending-prefix state) nil)
    (xenolith-helix--apply-mode state)
    (xenolith-helix--invalidate-prompt state)
    (xenolith-helix--refresh-status state))
  (values :continue nil))

;;;; -- Installation --

(defun xenolith-helix-install (ui)
  "Install Helix modality on UI's composer editor."
  (let* ((editor (terminal-ui-editor ui))
         (state  (gethash editor *xenolith-helix-states*)))
    (unless state
      (setf state (xenolith-helix-state-create :ui ui :editor editor))
      (setf (gethash editor *xenolith-helix-states*) state))
    (setf (xenolith-helix-state-ui state) ui)
    (xenolith-helix--set-mode state :insert)
    state))

(defun xenolith-helix--state-for-editor (editor)
  "Return the Helix state of EDITOR, or NIL."
  (gethash editor *xenolith-helix-states*))

(defun xenolith-helix--state-for-ui (ui)
  "Return the Helix state of UI's composer, or NIL."
  (when (typep ui 'terminal-ui)
    (xenolith-helix--state-for-editor (terminal-ui-editor ui))))

(defun xenolith-helix-mode (ui)
  "Return UI's current Helix mode, or NIL."
  (let ((state (xenolith-helix--state-for-ui ui)))
    (and state (xenolith-helix-state-mode state))))

(defun xenolith-helix--mode-label (ui)
  "Return UI's status band mode label and style."
  (case (xenolith-helix-mode ui)
    (:normal  (values "NOR" :status-accent))
    (:select  (values "SEL" :status-model))
    (:command (values ":" :status-effort))
    (:insert  (values "INS" :status-dim))
    (t        (values nil nil))))

;;;; -- Motions --

(defun xenolith-helix--line-start (text position)
  "Return the start of the line containing POSITION in TEXT."
  (let ((newline (position #\Newline text
                           :end (min position (length text))
                           :from-end t)))
    (if newline (1+ newline) 0)))

(defun xenolith-helix--line-end (text position)
  "Return the end of the line containing POSITION in TEXT."
  (or (position #\Newline text :start (min position (length text)))
      (length text)))
(defun xenolith-helix--line-first-nonwhitespace (text position)
  "Return the first non-whitespace character of POSITION's line."
  (let* ((start (xenolith-helix--line-start text position))
         (end   (xenolith-helix--line-end text position)))
    (or (position-if-not
         (lambda (character)
           (member character '(#\Space #\Tab) :test #'char=))
         text :start start :end end)
        start)))
(defun xenolith-helix--line-number-start (text number)
  "Return the start of 1-based line NUMBER in TEXT, clamped to the last line."
  (let ((position 0)
        (line     1)
        (target   (max 1 number)))
    (loop while (< line target)
          do (let ((newline (position #\Newline text :start position)))
               (if newline
                   (setf position (1+ newline)
                         line     (1+ line))
                   (return position))))
    (min position (length text))))

(defun xenolith-helix--last-line-start (text)
  "Return the start of the last non-empty line in TEXT."
  (let ((length (length text)))
    (if (zerop length)
        0
        (let ((position (1- length)))
          (loop while (and (plusp position)
                           (char= (char text position) #\Newline))
                do (decf position))
          (xenolith-helix--line-start text position)))))

(defun xenolith-helix--char-category (character)
  "Return CHARACTER's Helix category: :eol, :whitespace, :word, or :punctuation."
  (case character
    (#\Newline :eol)
    ((#\Space #\Tab #\Return #\Page) :whitespace)
    (t (if (or (alphanumericp character) (char= character #\_))
           :word
           :punctuation))))

(defun xenolith-helix--blank-category-p (category)
  "Return whether CATEGORY separates words."
  (member category '(:eol :whitespace) :test #'eq))

(defun xenolith-helix--word-boundary-p (left right)
  "Return whether LEFT and RIGHT belong to different Helix word categories."
  (not (eq (xenolith-helix--char-category left)
           (xenolith-helix--char-category right))))

(defun xenolith-helix--reached-word-target-p (target previous next)
  "Return whether the scan from PREVIOUS to NEXT reaches TARGET."
  (and (xenolith-helix--word-boundary-p previous next)
       (case target
         ((:next-word-start :prev-word-end)
          (or (eq (xenolith-helix--char-category next) :eol)
              (not (xenolith-helix--blank-category-p
                    (xenolith-helix--char-category next)))))
         ((:next-word-end :prev-word-start)
          (or (not (xenolith-helix--blank-category-p
                    (xenolith-helix--char-category previous)))
              (eq (xenolith-helix--char-category next) :eol))))))

(defun xenolith-helix--word-move-once (text anchor head target)
  "Return the anchor and head after one Helix word motion of TARGET."
  (let* ((length     (length text))
         (head       (xenolith-helix--normal-cursor text head))
         (anchor     (xenolith-helix--normal-cursor text anchor))
         (backward-p (member target '(:prev-word-start :prev-word-end)
                             :test #'eq)))
    (if (or (and backward-p (zerop head))
            (and (not backward-p) (= head length)))
        (values anchor head)
        (let ((scan-anchor
                (if backward-p
                    (if (< anchor head) head (min length (1+ head)))
                    (if (< anchor head) (max 0 (1- head)) head)))
              (scan-head
                (if backward-p
                    (if (< anchor head) (max 0 (1- head)) head)
                    (if (< anchor head) head (min length (1+ head))))))
          (setf anchor scan-anchor
                head   scan-head)
          (let ((previous (if backward-p
                              (and (< head length) (char text head))
                              (and (plusp head) (char text (1- head))))))
            (if backward-p
                (loop while (and (plusp head)
                                 (eq (xenolith-helix--char-category
                                      (char text (1- head)))
                                     :eol))
                      do (setf previous (char text (1- head)))
                         (decf head))
                (loop while (and (< head length)
                                 (eq (xenolith-helix--char-category
                                      (char text head))
                                     :eol))
                      do (setf previous (char text head))
                         (incf head)))
            (when (and previous
                       (eq (xenolith-helix--char-category previous) :eol))
              (setf anchor head))
            (let ((head-start head))
              (loop
                (let ((next (if backward-p
                                (and (plusp head) (char text (1- head)))
                                (and (< head length) (char text head)))))
                  (when (null next)
                    (return))
                  (if (or (null previous)
                          (xenolith-helix--reached-word-target-p
                           target previous next))
                      (if (= head head-start)
                          (progn
                            (setf anchor head
                                  previous next)
                            (if backward-p (decf head) (incf head)))
                          (return))
                      (progn
                        (setf previous next)
                        (if backward-p (decf head) (incf head))))))
              (values anchor head)))))))

(defun xenolith-helix--word-move (text anchor head count target)
  "Return the anchor and head after COUNT Helix word motions of TARGET.

TARGET is one of :next-word-start, :next-word-end, :prev-word-start, or
:prev-word-end.  The scan follows Helix's movement.rs, including its
block-cursor start range and newline bridging, so selections and chained
motions match the reference implementation."
  (dotimes (index (max 1 count) (values anchor head))
    (multiple-value-bind (next-anchor next-head)
        (xenolith-helix--word-move-once text anchor head target)
      (if (and (= next-anchor anchor) (= next-head head))
          (return (values anchor head))
          (setf anchor next-anchor
                head   next-head)))))

(defun xenolith-helix--vertical-target (text position direction count)
  "Return POSITION moved COUNT lines in DIRECTION, keeping its column."
  (let* ((start  (xenolith-helix--line-start text position))
         (column (- position start)))
    (dotimes (index count position)
      (let ((end (xenolith-helix--line-end text position)))
        (cond
          ((plusp direction)
           (if (< end (length text))
               (let ((next (1+ end)))
                 (setf position (min (+ next column)
                                     (xenolith-helix--line-end text next))
                       start next))
               (return position)))
          ((plusp start)
           (let* ((previous-end   (1- start))
                  (previous-start (xenolith-helix--line-start text previous-end)))
             (setf position (min (+ previous-start column)
                                 (xenolith-helix--line-end text previous-start))
                   start previous-start)))
          (t
           (return position)))))))

(defun xenolith-helix--line-last-character (text position)
  "Return the last character position of POSITION's line in TEXT."
  (let ((start (xenolith-helix--line-start text position))
        (end   (xenolith-helix--line-end text position)))
    (if (< start end) (1- end) start)))

(defun xenolith-helix--motion-target (editor motion count position)
  "Return EDITOR's target for MOTION repeated COUNT times from POSITION.

Word motions are computed by XENOLITH-HELIX--WORD-MOVE; the remaining
motions start from the block cursor's left edge, as Helix's movement
functions do."
  (let* ((text     (line-editor-text editor))
         (position (xenolith-helix--normal-cursor text position))
         (count    (max 1 (or count 1)))
         (target
           (case motion
             (:left       (- position count))
             (:right      (+ position count))
             (:line-start (xenolith-helix--line-start text position))
             (:line-end   (xenolith-helix--line-last-character text position))
             (:first-nonwhitespace
              (xenolith-helix--line-first-nonwhitespace text position))
             (:last-line-start
              (xenolith-helix--last-line-start text))
             (:line-number
              (xenolith-helix--line-number-start text count))
             (:buffer-start 0)
             (:buffer-end (max 0 (1- (length text))))
             (:down       (xenolith-helix--vertical-target text position 1 count))
             (:up         (xenolith-helix--vertical-target text position -1 count))
             (t           position))))
    (xenolith-helix--normal-cursor text target)))

;;;; -- Edits --

(defun xenolith-helix--set-position (editor position)
  "Place EDITOR's cursor at POSITION without changing its text."
  (let ((text (line-editor-text editor)))
    (line-editor-set-text editor text
                          :cursor (max 0 (min position (length text))))
    (values :continue nil)))

(defun xenolith-helix--move (state motion count)
  "Move STATE by MOTION repeated COUNT times.

Normal mode replaces the selection with the range the motion crosses;
select mode extends the current selection, matching Helix's move and
extend command families."
  (let* ((editor   (xenolith-helix-state-editor state))
         (text     (line-editor-text editor))
         (head     (xenolith-helix--normal-cursor
                    text (line-editor-cursor editor)))
         (anchor   (xenolith-helix--normal-cursor
                    text (or (xenolith-helix-state-anchor state) head)))
         (select-p (eq (xenolith-helix-state-mode state) :select))
         (word-p   (member motion '(:word-right :word-left :word-end)
                           :test #'eq))
         (word-target (case motion
                        (:word-right :next-word-start)
                        (:word-left  :prev-word-start)
                        (:word-end   :next-word-end))))
    (setf (xenolith-helix-state-count state) nil)
    (cond
      (word-p
       (multiple-value-bind (word-anchor word-head)
           (xenolith-helix--word-move text anchor head count word-target)
         (if select-p
             (multiple-value-bind (new-anchor new-head)
                 (xenolith-helix--put-cursor-extend
                  text anchor head
                  (xenolith-helix--block-position text word-anchor word-head))
               (setf (xenolith-helix-state-anchor state) new-anchor)
               (xenolith-helix--set-position editor new-head))
             (progn
               (setf (xenolith-helix-state-anchor state) word-anchor)
               (xenolith-helix--set-position editor word-head)))))
      (select-p
       (let ((target (xenolith-helix--motion-target
                      editor motion count
                      (xenolith-helix--block-position text anchor head))))
         (multiple-value-bind (new-anchor new-head)
             (xenolith-helix--put-cursor-extend text anchor head target)
           (setf (xenolith-helix-state-anchor state) new-anchor)
           (xenolith-helix--set-position editor new-head))))
      (t
       (let ((target (xenolith-helix--motion-target
                      editor motion count
                      (xenolith-helix--block-position text anchor head))))
         (setf (xenolith-helix-state-anchor state) target)
         (xenolith-helix--set-position editor target))))
    (xenolith-helix--invalidate-prompt state)
    (values :continue nil)))

(defun xenolith-helix--delete-range (state start end)
  "Delete START to END, keeping the removed text as STATE's kill."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor))
         (start  (max 0 (min start (length text))))
         (end    (max start (min end (length text)))))
    (setf (xenolith-helix-state-kill state) (subseq text start end))
    (line-editor-set-text
     editor
     (concatenate 'string (subseq text 0 start) (subseq text end))
     :cursor start)
    (setf (xenolith-helix-state-anchor state) start)
    (values :continue nil)))

(defun xenolith-helix--yank-range (state start end)
  "Copy START to END into STATE's kill, leaving its selection in place."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor))
         (start  (max 0 (min start (length text))))
         (end    (max start (min end (length text)))))
    (setf (xenolith-helix-state-kill state) (subseq text start end))
    (values :continue nil)))

(defun xenolith-helix--line-end-inclusive (text position)
  "Return the exclusive end of POSITION's line, including its line break."
  (let ((end (xenolith-helix--line-end text position)))
    (if (< end (length text)) (1+ end) end)))

(defun xenolith-helix--advance-lines (text position count)
  "Return the position after COUNT line breaks from POSITION."
  (let ((length (length text))
        (end    position))
    (if (zerop length)
        0
        (dotimes (index (max 1 count) end)
          (let ((line-end (xenolith-helix--line-end
                           text (min end (1- length)))))
            (setf end (if (< line-end length) (1+ line-end) length)))))))

(defun xenolith-helix--select-lines (state count)
  "Select COUNT lines, or extend a linewise selection by COUNT lines."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor))
         (length (length text)))
    (setf (xenolith-helix-state-count state) nil)
    (cond
      ((zerop length)
       (setf (xenolith-helix-state-anchor state) 0)
       (xenolith-helix--set-position editor 0))
      (t
       (multiple-value-bind (start end) (xenolith-helix--selection state)
         (let* ((linewise-p
                  (and (> end start)
                       (= start (xenolith-helix--line-start text start))
                       (or (= end length)
                           (char= (char text (1- end)) #\Newline))))
                (cursor (xenolith-helix--block-position
                         text
                           (or (xenolith-helix-state-anchor state)
                               (line-editor-cursor editor))
                         (line-editor-cursor editor)))
                (new-start (if linewise-p
                               start
                               (xenolith-helix--line-start text cursor)))
                (new-end (if linewise-p
                             (xenolith-helix--advance-lines
                              text end (max 1 count))
                             (xenolith-helix--advance-lines
                              text new-start (max 1 count)))))
           (setf (xenolith-helix-state-anchor state) new-start)
           (xenolith-helix--set-position editor new-end)))))
    (xenolith-helix--invalidate-prompt state)
    (values :continue nil)))

(defun xenolith-helix--select-to-line-bounds (state)
  "Select the current line bounds and line break, preserving direction."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor))
         (length (length text)))
    (when (plusp length)
      (let* ((head   (xenolith-helix--normal-cursor
                      text (line-editor-cursor editor)))
             (anchor (xenolith-helix--normal-cursor
                      text (or (xenolith-helix-state-anchor state) head)))
             (cursor (xenolith-helix--block-position text anchor head))
             (start  (xenolith-helix--line-start text cursor))
             (end    (xenolith-helix--line-end text cursor)))
        (when (< start end)
          (let ((line-end (xenolith-helix--line-end-inclusive text cursor)))
            (if (< head anchor)
                (progn
                  (setf (xenolith-helix-state-anchor state) line-end)
                  (xenolith-helix--set-position editor start))
                (progn
                  (setf (xenolith-helix-state-anchor state) start)
                  (xenolith-helix--set-position editor line-end)))))))
    (xenolith-helix--invalidate-prompt state)
    (values :continue nil)))

(defun xenolith-helix--collapse-selection (state)
  "Collapse STATE's selection to its block cursor."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor))
         (head   (xenolith-helix--normal-cursor
                  text (line-editor-cursor editor)))
         (anchor (xenolith-helix--normal-cursor
                  text (or (xenolith-helix-state-anchor state) head)))
         (point  (xenolith-helix--block-position text anchor head)))
    (setf (xenolith-helix-state-anchor state) point)
    (xenolith-helix--set-position editor point)
    (xenolith-helix--invalidate-prompt state)
    (values :continue nil)))

(defun xenolith-helix--select-buffer (state)
  "Select the whole editor text."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor))
         (length (length text)))
    (when (plusp length)
      (setf (xenolith-helix-state-anchor state) 0)
      (xenolith-helix--set-position editor length))
    (xenolith-helix--invalidate-prompt state)
    (values :continue nil)))

(defun xenolith-helix--apply-selection (state operator)
  "Apply OPERATOR to STATE's selection."
  (let ((editor (xenolith-helix-state-editor state)))
    (setf (xenolith-helix-state-count state) nil)
    (when (plusp (length (line-editor-text editor)))
      (multiple-value-bind (start end) (xenolith-helix--selection state)
        (case operator
          (:yank   (xenolith-helix--yank-range state start end)
                   (xenolith-helix--exit-select-mode state))
          (:delete (xenolith-helix--delete-range state start end)
                   (xenolith-helix--set-mode state :normal))
            (:change
             (let* ((text       (line-editor-text editor))
                    (linewise-p
                      (and (> end start)
                           (= start (xenolith-helix--line-start text start))
                           (or (= end (length text))
                               (char= (char text (1- end)) #\Newline)))))
               (xenolith-helix--delete-range state start end)
               (when linewise-p
                 (let ((new-text (line-editor-text editor)))
                   (line-editor-set-text
                    editor
                    (concatenate 'string
                                 (subseq new-text 0 start)
                                 (string #\Newline)
                                 (subseq new-text start))
                    :cursor start)
                     (setf (xenolith-helix-state-anchor state) start)))
               (xenolith-helix--set-mode state :insert))))))
    (xenolith-helix--invalidate-prompt state)
    (values :continue nil)))

(defun xenolith-helix--delete-at-cursor (state count)
  "Delete STATE's selection, or COUNT characters forward when it is empty."
  (if (member (xenolith-helix-state-mode state) '(:normal :select) :test #'eq)
        (xenolith-helix--apply-selection state :delete)
      (let* ((editor (xenolith-helix-state-editor state))
             (cursor (line-editor-cursor editor))
             (text   (line-editor-text editor))
             (end    (min (length text) (+ cursor (max 1 count)))))
        (setf (xenolith-helix-state-count state) nil)
        (if (= cursor end)
            (values :continue nil)
            (xenolith-helix--delete-range state cursor end)))))

(defun xenolith-helix--paste (state &optional count)
  "Insert STATE's kill after its selection, repeating it COUNT times."
  (let* ((editor      (xenolith-helix-state-editor state))
         (kill        (xenolith-helix-state-kill state))
         (text        (line-editor-text editor))
         (selection-p (member (xenolith-helix-state-mode state)
                              '(:normal :select) :test #'eq))
         (linewise-p  (and kill (find #\Newline kill)))
         (repeat      (max 1 (or count 1)))
         (payload     (if (or (null kill) (zerop (length kill)))
                          ""
                            (with-output-to-string (out)
                              (loop repeat repeat
                                    do (write-string kill out))))))
    (setf (xenolith-helix-state-count state) nil)
    (cond
      ((zerop (length payload))
       (values :continue nil))
      (t
         (multiple-value-bind (start end)
             (if selection-p
                 (xenolith-helix--selection state)
                 (values (line-editor-cursor editor)
                         (line-editor-cursor editor)))
           (let* ((insert-at
                    (cond
                      ((not selection-p)
                       (line-editor-cursor editor))
                      (linewise-p
                       (let ((probe (if (> end start) (1- end) (line-editor-cursor editor))))
                         (xenolith-helix--line-end-inclusive text probe)))
                      (t end)))
                  (separator
                    (if (and linewise-p
                             (= insert-at (length text))
                             (plusp insert-at)
                             (not (char= (char text (1- insert-at)) #\Newline)))
                        (string #\Newline)
                        ""))
                  (full-payload (concatenate 'string separator payload))
                  (new-text  (concatenate 'string
                                          (subseq text 0 insert-at)
                                          full-payload
                                          (subseq text insert-at)))
                    (end       (+ insert-at (length full-payload))))
             (line-editor-set-text editor new-text :cursor end)
             (when selection-p
               (setf (xenolith-helix-state-anchor state) insert-at)
               (xenolith-helix--exit-select-mode state))
           (xenolith-helix--invalidate-prompt state)
           (values :continue nil)))))))

(defun xenolith-helix--insert-at (state position)
  "Place the cursor at POSITION and enter insert mode."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor)))
    (line-editor-set-text editor text :cursor (max 0 (min position (length text))))
    (xenolith-helix--set-mode state :insert)))

(defun xenolith-helix--open-line (state direction count)
  "Insert COUNT blank lines DIRECTION of the selection and enter insert mode."
  (let* ((editor   (xenolith-helix-state-editor state))
         (text     (line-editor-text editor))
         (repeat   (max 1 (or count 1)))
            (newlines (make-string repeat :initial-element #\Newline)))
    (multiple-value-bind (start end) (xenolith-helix--selection state)
      (let* ((line-position
               (if (eq direction :below)
                   (if (> end start) (1- end) (line-editor-cursor editor))
                   start))
             (position
               (if (eq direction :below)
                   (xenolith-helix--line-end-inclusive text line-position)
                   (xenolith-helix--line-start text line-position))))
          (line-editor-set-text
           editor
           (concatenate 'string
                        (subseq text 0 position)
                        newlines
                        (subseq text position))
           :cursor (if (eq direction :below)
                       (+ position (length newlines))
                       position))
        (xenolith-helix--set-mode state :insert)))))

(defun xenolith-helix--enter-command (state)
  "Open a command line in STATE's editor."
  (let ((editor (xenolith-helix-state-editor state))
        (ui     (xenolith-helix-state-ui state)))
    (setf (xenolith-helix-state-command-index state) nil
          (xenolith-helix-state-command-draft state) "")
    (when (and ui (fboundp 'terminal-ui-completion-dismissed-p))
      (setf (terminal-ui-completion-dismissed-p ui) nil))
    (line-editor-set-text editor ":" :cursor 1)
    (xenolith-helix--set-mode state :command)))

;;;; -- Normal-mode commands --

(defun xenolith-helix--accumulate-count (state character)
  "Add CHARACTER as the next digit of STATE's count."
  (setf (xenolith-helix-state-count state)
        (+ (* 10 (or (xenolith-helix-state-count state) 0))
           (digit-char-p character)))
  (values :continue nil))

(defun xenolith-helix--normal-simple (state character count)
  "Apply normal-mode CHARACTER with COUNT."
  (let* ((editor (xenolith-helix-state-editor state))
         (text   (line-editor-text editor)))
    (case character
      (#\h (xenolith-helix--move state :left count))
      (#\l (xenolith-helix--move state :right count))
      (#\j (xenolith-helix--move state :down count))
      (#\k (xenolith-helix--move state :up count))
      (#\w (xenolith-helix--move state :word-right count))
      (#\b (xenolith-helix--move state :word-left count))
      (#\e (xenolith-helix--move state :word-end count))
        (#\G (if (xenolith-helix-state-count state)
                 (xenolith-helix--move state :line-number count)
                 (values :continue nil)))
        (#\v (xenolith-helix--set-mode
              state
              (if (eq (xenolith-helix-state-mode state) :select)
                  :normal
                  :select)))
        (#\% (xenolith-helix--select-buffer state))
      (#\g (setf (xenolith-helix-state-pending-prefix state) :g)
           (values :continue nil))
      (#\x (xenolith-helix--select-lines state count))
      (#\X (xenolith-helix--select-to-line-bounds state))
      (#\; (xenolith-helix--collapse-selection state))
        (#\d (xenolith-helix--apply-selection state :delete))
        (#\c (xenolith-helix--apply-selection state :change))
        (#\y (xenolith-helix--apply-selection state :yank))
        (#\p (xenolith-helix--paste state count))
      (#\i (multiple-value-bind (start end)
               (xenolith-helix--selection state)
             (declare (ignore end))
             (xenolith-helix--insert-at state start)))
      (#\a (multiple-value-bind (start end)
               (xenolith-helix--selection state)
             (declare (ignore start))
             (xenolith-helix--insert-at state end)))
        (#\I (xenolith-helix--insert-at
              state
              (xenolith-helix--line-first-nonwhitespace
               text
               (xenolith-helix--normal-cursor text (line-editor-cursor editor)))))
      (#\A (xenolith-helix--insert-at
            state
            (xenolith-helix--line-end
             text
             (xenolith-helix--normal-cursor text (line-editor-cursor editor)))))
        (#\o (xenolith-helix--open-line state :below count))
        (#\O (xenolith-helix--open-line state :above count))
      (#\: (xenolith-helix--enter-command state))
      (t (setf (xenolith-helix-state-count state) nil)
         (values :continue nil)))))

(defun xenolith-helix--normal-command (state character)
  "Apply one normal-mode CHARACTER to STATE."
  (let ((prefix (xenolith-helix-state-pending-prefix state))
        (count  (max 1 (or (xenolith-helix-state-count state) 1))))
    (cond
        ((eq prefix :g)
         (setf (xenolith-helix-state-pending-prefix state) nil)
         (case character
           (#\g (xenolith-helix--move state :buffer-start 1))
             (#\e (xenolith-helix--move state :last-line-start 1))
           (#\h (xenolith-helix--move state :line-start 1))
           (#\l (xenolith-helix--move state :line-end 1))
           (#\s (xenolith-helix--move state :first-nonwhitespace 1))
           (t
            (setf (xenolith-helix-state-count state) nil)
            (values :continue nil))))
      ((and (digit-char-p character)
            (or (not (char= character #\0))
                (xenolith-helix-state-count state)))
       (xenolith-helix--accumulate-count state character))
      (t
       (xenolith-helix--normal-simple state character count)))))

;;;; -- Key entry points --

(defun xenolith-helix--normal-key (editor event)
  "Apply a normal-mode key EVENT to EDITOR."
  (let ((state (xenolith-helix--state-for-editor editor))
        (text  (and (consp event) (stringp (second event)) (second event))))
    (if (and state text (= 1 (length text)))
        (xenolith-helix--normal-command state (char text 0))
        (values :continue nil))))

(defun xenolith-helix--insert-escape (editor event)
  "Leave insert mode for normal mode."
  (declare (ignore event))
  (let ((state (xenolith-helix--state-for-editor editor)))
    (if state
        (xenolith-helix--set-mode state :normal)
        (values :ignored nil))))

(defun xenolith-helix--normal-escape (editor event)
  "Leave select mode, or collapse the selection, without leaving normal mode."
  (declare (ignore event))
  (let ((state (xenolith-helix--state-for-editor editor)))
    (when state
      (if (eq (xenolith-helix-state-mode state) :select)
          (xenolith-helix--set-mode state :normal)
          (progn
            (setf (xenolith-helix-state-count state) nil
                  (xenolith-helix-state-pending-prefix state) nil)
            (xenolith-helix--collapse-selection state))))
    (values :continue nil)))

(defun xenolith-helix--normal-ignore (editor event)
  "Ignore an editing event in normal mode."
  (declare (ignore editor event))
  (values :ignored nil))
(defun xenolith-helix--normal-motion-key (editor event)
  "Apply normal-mode EVENT to EDITOR as its Helix key would."
  (let* ((state     (xenolith-helix--state-for-editor editor))
         (character (cdr (assoc event *xenolith-helix-motion-events*
                                :test #'eq))))
    (if (and state character)
        (xenolith-helix--normal-command state character)
        (values :ignored nil))))

(defun xenolith-helix--normal-line-key (editor event)
  "Move EDITOR's selection to its line start or end for Home or End."
  (let ((state (xenolith-helix--state-for-editor editor)))
    (if state
        (xenolith-helix--move state (if (eq event :home) :line-start :line-end) 1)
        (values :ignored nil))))

(defun xenolith-helix--normal-delete (editor event)
  "Delete forward in normal mode."
  (declare (ignore event))
  (let ((state (xenolith-helix--state-for-editor editor)))
    (if state
        (xenolith-helix--delete-at-cursor
         state (or (xenolith-helix-state-count state) 1))
        (values :ignored nil))))

(defun xenolith-helix--normal-enter (editor event)
  "Insert a line break at the cursor and keep a one-character selection."
  (declare (ignore event))
  (let* ((state  (xenolith-helix--state-for-editor editor))
         (text   (line-editor-text editor))
         (cursor (line-editor-cursor editor)))
    (if state
        (let* ((new-text (concatenate 'string (subseq text 0 cursor)
                                      (string #\Newline)
                                      (subseq text cursor)))
               (position (xenolith-helix--normal-cursor new-text (1+ cursor))))
          (line-editor-set-text editor new-text :cursor position)
              (setf (xenolith-helix-state-anchor state) position
                    (xenolith-helix-state-count state) nil)
          (xenolith-helix--invalidate-prompt state)
          (values :continue nil))
        (values :ignored nil))))

(defun xenolith-helix--normal-paste (editor event)
  "Insert a bracketed paste as text and restore the previous mode."
  (let* ((state   (xenolith-helix--state-for-editor editor))
         (payload (and (consp event) (stringp (second event)) (second event))))
    (if (and state payload)
        (let ((previous (xenolith-helix-state-mode state)))
          (setf (xenolith-helix-state-mode state) :insert)
          (clinedi:line-editor-execute-command editor :paste event)
          (setf (xenolith-helix-state-mode state) previous)
          (xenolith-helix--apply-mode state)
            (when (member previous '(:normal :select) :test #'eq)
            (let* ((text   (line-editor-text editor))
                   (cursor (xenolith-helix--normal-cursor
                            text (line-editor-cursor editor))))
                (line-editor-set-text editor text :cursor cursor)
                  (setf (xenolith-helix-state-anchor state) cursor)))
          (xenolith-helix--invalidate-prompt state)
          (values :continue nil))
        (values :ignored nil))))

;;;; -- Command mode --

(defun xenolith-helix--command-text (editor)
  "Return EDITOR's command text without its leading colon."
  (let ((text (line-editor-text editor)))
    (if (and (plusp (length text)) (char= (char text 0) #\:))
        (subseq text 1)
        text)))

(defun xenolith-helix--command-escape (editor event)
  "Leave command mode, discarding the command line."
  (declare (ignore event))
  (let ((state (xenolith-helix--state-for-editor editor)))
    (if state
        (progn
          (line-editor-set-text editor "")
          (setf (xenolith-helix-state-command-index state) nil)
          (xenolith-helix--set-mode state :normal))
        (values :ignored nil))))

(defun xenolith-helix--command-backspace (editor event)
  "Delete one command character, or leave command mode at the marker."
  (let ((state (xenolith-helix--state-for-editor editor)))
    (cond
      ((null state) (values :ignored nil))
      ((<= (length (line-editor-text editor)) 1)
       (xenolith-helix--command-escape editor event))
      (t
       (clinedi:line-editor-execute-command editor :backspace event)
       (values :continue nil)))))

(defun xenolith-helix--command-name (text)
  "Return the command name at the start of colon command TEXT, or NIL."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) text)))
    (when (plusp (length trimmed))
      (string-downcase
       (subseq trimmed
               0
               (or (position-if (lambda (character)
                                  (find character '(#\Space #\Tab)))
                                trimmed)
                   (length trimmed)))))))

(defun xenolith-helix--command-submit (editor event)
  "Submit the command line through the slash-command path.

A line that names no registered command is submitted as ordinary input, so a
prompt that merely begins with a colon still reaches the model."
  (let* ((state   (xenolith-helix--state-for-editor editor))
         (command (xenolith-helix--command-text editor))
         (name    (and state (xenolith-helix--command-name command))))
    (cond
      ((null state) (values :ignored nil))
      ((null name)
       (line-editor-set-text editor "")
       (xenolith-helix--set-mode state :normal))
      ((null (application-command-find (concatenate 'string "/" name)))
       (line-editor-set-text editor (concatenate 'string ":" command))
       (xenolith-helix--set-mode state :normal)
       (clinedi:line-editor-execute-command editor :submit event))
      (t
       (push command (xenolith-helix-state-command-history state))
       (setf (xenolith-helix-state-command-index state) nil)
       (line-editor-set-text editor (concatenate 'string "/" command))
       (xenolith-helix--set-mode state :normal)
       (clinedi:line-editor-execute-command editor :submit event)))))

(defun xenolith-helix--colon-name (name)
  "Return NAME with a leading slash written as a colon, or NAME unchanged."
  (if (and (stringp name)
           (plusp (length name))
           (char= (char name 0) #\/))
      (concatenate 'string ":" (subseq name 1))
      name))

(defun xenolith-helix--colon-entry (entry)
  "Return completion ENTRY with its slash names written as colon names."
  (let ((copy (copy-list entry)))
    (setf (getf copy :name)    (xenolith-helix--colon-name (getf copy :name))
          (getf copy :primary) (xenolith-helix--colon-name
                                (getf copy :primary)))
    copy))

(defun xenolith-helix--command-line-p (text)
  "Return true when TEXT is a single-line colon command line."
  (and (plusp (length text))
       (char= (char text 0) #\:)
       (not (find-if (lambda (character)
                       (find character '(#\Newline #\Return)))
                     text))))

(defun xenolith-helix--command-entries (ui text)
  "Return the command candidates whose slash name starts with colon TEXT.

The colon maps onto the slash form the registry stores, so command mode
completes from the same provider as the slash and parenthesized lines."
  (let ((prefix (concatenate 'string "/" (subseq text 1))))
    (loop for entry in (terminal-ui--current-completions ui)
          for name = (getf entry :name)
          when (and (stringp name)
                    (uiop:string-prefix-p (string-downcase prefix)
                                          (string-downcase name)))
            collect entry)))

(defun xenolith-helix--matching-completions (ui)
  "Return UI's completion candidates, naming colon commands in command mode.

Command mode reads like a slash command line, so its menu lists the same
commands with the colon standing in for the slash. Every other mode, and a
command line past the name, keeps the tracked candidates."
  (let* ((state   (xenolith-helix--state-for-ui ui))
         (text    (and state
                       (eq (xenolith-helix-state-mode state) :command)
                       (terminal-interactive-p (terminal-ui-terminal ui))
                       (terminal-ui--completion-text
                        (terminal-ui-editor ui))))
         (entries (and text
                       (xenolith-helix--command-line-p text)
                       (xenolith-helix--command-entries ui text))))
    (if entries
        (mapcar #'xenolith-helix--colon-entry
                (terminal-ui--shadowed-completions entries))
        (funcall *xenolith-original-terminal-ui--matching-completions* ui))))

(defun xenolith-helix--command-up (editor event)
  "Recall the previous command line."
  (declare (ignore event))
  (let* ((state   (xenolith-helix--state-for-editor editor))
         (history (and state (xenolith-helix-state-command-history state))))
    (if (null history)
        (values :ignored nil)
        (let ((index (xenolith-helix-state-command-index state)))
          (if (null index)
              (progn
                (setf (xenolith-helix-state-command-draft state)
                      (xenolith-helix--command-text editor))
                (setf index 0))
              (setf index (min (1- (length history)) (1+ index))))
          (setf (xenolith-helix-state-command-index state) index)
          (let ((entry (nth index history)))
            (line-editor-set-text editor (concatenate 'string ":" entry)
                                  :cursor (1+ (length entry))))
          (values :continue nil)))))

(defun xenolith-helix--command-down (editor event)
  "Recall the next command line, or the draft."
  (declare (ignore event))
  (let* ((state (xenolith-helix--state-for-editor editor))
         (index (and state (xenolith-helix-state-command-index state))))
    (if (null index)
        (values :ignored nil)
        (let ((next (1- index)))
          (if (minusp next)
              (let ((draft (xenolith-helix-state-command-draft state)))
                (setf (xenolith-helix-state-command-index state) nil)
                (line-editor-set-text editor (concatenate 'string ":" draft)
                                      :cursor (1+ (length draft))))
              (let ((entry (nth next (xenolith-helix-state-command-history state))))
                (setf (xenolith-helix-state-command-index state) next)
                (line-editor-set-text editor (concatenate 'string ":" entry)
                                      :cursor (1+ (length entry)))))
          (values :continue nil)))))

;;;; -- Presentation --

(defun xenolith-helix--invalidate-prompt (state)
  "Drop STATE's memoized prompt render after its selection changed."
  (let ((ui (xenolith-helix-state-ui state)))
    (when (and ui (fboundp 'terminal-ui-prompt-render-cache))
      (setf (terminal-ui-prompt-render-cache ui) nil))))

(defun xenolith-helix--select-spans (spans start end)
  "Return SPANS with the [START, END) range painted as a selection."
  (if (or (null spans) (null start) (null end) (<= end start))
      spans
      (loop with offset = 0
            for span in spans
            append (let* ((text  (terminal-span-text span))
                          (style (terminal-span-style span))
                          (from  offset)
                          (to    (+ offset (length text))))
                     (incf offset (length text))
                     (cond
                       ((or (<= to start) (<= end from))
                        (list span))
                       (t
                        (let ((local-start (max 0 (- start from)))
                              (local-end   (min (length text) (- end from))))
                          (append
                           (when (plusp local-start)
                             (list (terminal-span style
                                                  (subseq text 0 local-start))))
                           (when (< local-start local-end)
                             (list (terminal-span :selected
                                                  (subseq text local-start local-end))))
                           (when (< local-end (length text))
                             (list (terminal-span style
                                                  (subseq text local-end))))))))))))

(defun xenolith-helix--content-selection (ui content-length async-prefix-length)
  "Return UI's visible selection in display content as (START . END)."
  (let ((state (xenolith-helix--state-for-ui ui)))
    (when (and state
               (member (xenolith-helix-state-mode state)
                       '(:normal :select) :test #'eq))
      (let ((prefix (or async-prefix-length 0)))
        (multiple-value-bind (start end)
            (xenolith-helix--highlight-selection state)
          (let ((start (max 0 (min (- start prefix) content-length)))
                (end   (max 0 (min (- end prefix) content-length))))
            (when (> end start)
              (cons start end))))))))

(defun xenolith-helix--block-cursor-p (state)
  "Return whether STATE's mode shows its head as a block cursor."
  (and state
       (member (xenolith-helix-state-mode state) '(:normal :select) :test #'eq)))

(defun xenolith-helix--cursor-cell (content cursor)
  "Return CONTENT's cursor cell at CURSOR as (START . END), or NIL.

A Helix cursor is a single-width selection, so normal and select mode paint
the head character as a block. Empty content and a cursor on a line break have
no cell to paint."
  (when (plusp (length content))
    (let ((cursor (max 0 (min cursor (1- (length content))))))
      (unless (char= (char content cursor) #\Newline)
        (cons cursor (1+ cursor))))))

(defun xenolith-helix--selected-render-prompt-content
    (ui lisp-draft-p async-lisp-draft-p)
  "Render UI's prompt with its block cursor and selection painted."
  (let* ((terminal        (terminal-ui-terminal ui))
         (columns         (terminal-columns terminal))
         (editor          (terminal-ui-editor ui))
         (state           (xenolith-helix--state-for-ui ui))
         (block-p         (xenolith-helix--block-cursor-p state))
         (raw-content     (line-editor-text editor))
         (async-prefix-length
           (and async-lisp-draft-p
                (terminal-ui--async-lisp-prefix-length raw-content)))
         (display-content (if async-prefix-length
                              (subseq raw-content async-prefix-length)
                              raw-content))
         (safe-prompt     (sanitize-text (terminal-ui-prompt ui)
                                         :single-line-p t)))
    (multiple-value-bind (prompt-text prompt-spans)
        (terminal-ui--prompt-spans safe-prompt lisp-draft-p async-lisp-draft-p)
      (if (and (zerop (length display-content))
               (non-empty-string-p (terminal-ui-placeholder ui)))
          (let ((spans
                  (terminal--clip-spans
                   (append prompt-spans
                           (when block-p
                             (list (terminal-span :selected " ")))
                           (list (terminal-span :hint
                                                (terminal-ui-placeholder ui))))
                   columns)))
            (values spans
                    (min (length prompt-text)
                         (length (terminal--spans-text spans)))))
          (let* ((visible         (sanitize-text display-content))
                 (visible-cursor
                   (length
                    (sanitize-text
                     (subseq display-content
                             0
                             (max 0 (- (line-editor-cursor editor)
                                       (or async-prefix-length 0)))))))
                 (empty-p         (zerop (length visible)))
                 (content         (if (and empty-p block-p) " " visible))
                 (content-spans
                   (or (and (or lisp-draft-p async-lisp-draft-p)
                            (not empty-p)
                            (syntax--highlight-spans
                             content
                             :language (language-find ':common-lisp :errorp nil)))
                       (list (terminal-span
                              (if (uiop:string-prefix-p "/" content)
                                  ':user
                                  ':plain)
                              content))))
                 (selection
                   (xenolith-helix--content-selection
                    ui (length content) async-prefix-length))
                 (cursor-cell
                   (if (or selection (not block-p))
                       nil
                       (xenolith-helix--cursor-cell content visible-cursor)))
                  (painted-spans
                    (cond
                      (selection
                       (xenolith-helix--select-spans
                        content-spans (car selection) (cdr selection)))
                      (cursor-cell
                       (xenolith-helix--select-spans
                        content-spans (car cursor-cell) (cdr cursor-cell)))
                      (t content-spans)))
                  (prompt-display  (terminal--render-spans terminal prompt-spans))
                  (content-display (terminal--render-spans terminal painted-spans)))
            (multiple-value-bind (wrapped-content wrapped-display wrapped-cursor)
                (wrap-styled-editor-text content content-display
                                         :cursor       visible-cursor
                                         :columns      columns
                                         :prompt-width (text-cell-width prompt-text))
              (values
               (terminal--make-rendered-row
                (concatenate 'string prompt-text wrapped-content)
                (concatenate 'string prompt-display wrapped-display))
               (+ (length prompt-text) wrapped-cursor))))))))

(defvar *xenolith-original-terminal-ui--render-prompt-content* nil
  "The prompt renderer underneath this module's wrapper, or NIL.")

(defun xenolith-helix--render-prompt-content-hook
    (ui lisp-draft-p async-lisp-draft-p)
  "Render UI's prompt through the selection-aware renderer in normal or select mode."
  (let ((state (xenolith-helix--state-for-ui ui)))
    (if (and state
             (member (xenolith-helix-state-mode state)
                     '(:normal :select) :test #'eq))
        (xenolith-helix--selected-render-prompt-content
         ui lisp-draft-p async-lisp-draft-p)
        (funcall *xenolith-original-terminal-ui--render-prompt-content*
                 ui lisp-draft-p async-lisp-draft-p))))

;;;; -- Status line --

(defun xenolith-helix--status-mode-spans (ui)
  "Return UI's mode indicator spans for the status band."
  (multiple-value-bind (label style) (xenolith-helix--mode-label ui)
    (when label
      (list (terminal-span (or style :status-accent) label)
            (terminal-span :status-dim " · ")))))

(defvar *xenolith-helix-status-bases* (make-hash-table :test #'eq)
  "Status details of each UI before the mode indicator is prepended.")

(defvar *xenolith-original-application--status-details* nil
  "The status detail builder underneath this module's wrapper, or NIL.")

(defun xenolith-helix--status-details (application)
  "Return APPLICATION's status details with the active mode before the model."
  (let* ((base (funcall *xenolith-original-application--status-details*
                        application))
         (ui   (ignore-errors (application-ui application))))
    (when ui
      (setf (gethash ui *xenolith-helix-status-bases*) base))
    (if (and ui (xenolith-helix-mode ui))
        (append (xenolith-helix--status-mode-spans ui) base)
        base)))

(defun xenolith-helix--refresh-status (state)
  "Repaint STATE's status details with its current mode."
  (let* ((ui   (xenolith-helix-state-ui state))
         (base (and ui (gethash ui *xenolith-helix-status-bases*))))
    (when ui
      (when base
        (let ((details (append (xenolith-helix--status-mode-spans ui) base)))
          (when (terminal-ui-idle-status-details ui)
            (terminal-ui-set-idle-status-details ui details))
          (when (terminal-ui-status ui)
            (setf (terminal-ui-status-details ui) details)
            (terminal-ui--paint-live ui))))
        (xenolith-helix--paint-mode-presentation ui)))
  (values))

;;;; -- Cursor shape --

(defvar *xenolith-original-terminal-ui-fullscreen-paint* nil
  "The fullscreen painter underneath this module's wrapper, or NIL.")

(defvar *xenolith-original-terminal-ui-fullscreen-leave* nil
  "The fullscreen exit underneath this module's wrapper, or NIL.")

(defvar *xenolith-original-terminal-ui-fullscreen-enter* nil
  "The fullscreen entry underneath this module's wrapper, or NIL.")

(defvar *xenolith-helix-cursor-styles* (make-hash-table :test #'eq)
  "The cursor shape sequence last written for each UI.")

(defun xenolith-helix--escape-sequence (body)
  "Return BODY as a terminal control sequence.

Common Lisp reads \\e in a string literal as a plain e, so the escape byte has
to be built; spelled literally, the sequence was written as text into the
composer's cursor cell."
  (concatenate 'string (string #\Escape) body))

(defun xenolith-helix--cursor-sequence (ui)
  "Return the terminal cursor shape sequence for UI's mode."
  (case (xenolith-helix-mode ui)
    (:insert                    (xenolith-helix--escape-sequence "[6 q"))
    ((:normal :select :command) (xenolith-helix--escape-sequence "[2 q"))
    (t                          nil)))

(defun xenolith-helix--cursor-shape-p (ui)
  "Return whether UI's terminal is live enough to take a mode cursor shape."
  (if (typep ui 'fullscreen-terminal-ui)
      (fullscreen-terminal-ui-active-p ui)
      (terminal-ui-started-p ui)))

(defun xenolith-helix--paint-cursor-style (ui)
  "Write UI's mode cursor shape when it differs from the last one.

The composer paints the normal and select cursor as a character cell, so the
block survives terminals that ignore the shape request; the request still
gives insert its bar and normal its block where it is honoured."
  (let ((sequence (and (typep ui 'terminal-ui)
                       (xenolith-helix--cursor-shape-p ui)
                       (xenolith-helix--cursor-sequence ui))))
    (when (and sequence
               (not (equal sequence (gethash ui *xenolith-helix-cursor-styles*))))
      (let ((terminal (terminal-ui-terminal ui)))
        (terminal--write terminal sequence)
        (terminal-flush terminal)
        (setf (gethash ui *xenolith-helix-cursor-styles*) sequence)))))
;;;; -- Mouse reporting --

(defvar *xenolith-helix-mouse-states* (make-hash-table :test #'eq)
  "The mouse reporting state last written for each UI.")

(defun xenolith-helix--mouse-sequence (ui)
  "Return the mouse reporting controls UI's mode wants, or NIL.

Insert and command mode keep the viewport's button reports, so a click still
places the composer caret and the wheel scrolls. Normal and select mode release
the mouse, so a plain drag runs the terminal's own selection and PageUp and
PageDown scroll the transcript. With *XENOLITH-HELIX-MOUSE-SCROLL-P* on, those
modes keep the reports too and selection needs the terminal's Shift bypass."
  (case (xenolith-helix-mode ui)
    ((:insert :command) (mouse-reporting-enable-sequence))
    ((:normal :select)
     (if *xenolith-helix-mouse-scroll-p*
         (mouse-reporting-enable-sequence)
         (mouse-reporting-disable-sequence)))
    (t                  nil)))

(defun xenolith-helix--paint-mouse (ui)
  "Write UI's mouse reporting state when it differs from the last one."
  (let ((sequence (and (typep ui 'terminal-ui)
                       (xenolith-helix--cursor-shape-p ui)
                       (xenolith-helix--mouse-sequence ui))))
    (when (and sequence
               (not (equal sequence (gethash ui *xenolith-helix-mouse-states*))))
      (let ((terminal (terminal-ui-terminal ui)))
        (terminal--write terminal sequence)
        (terminal-flush terminal)
        (setf (gethash ui *xenolith-helix-mouse-states*) sequence)))))

(defun xenolith-helix--paint-mode-presentation (ui)
  "Present UI's mode: its cursor shape and whether it holds the mouse."
  (xenolith-helix--paint-cursor-style ui)
  (xenolith-helix--paint-mouse ui))

(defun xenolith-helix--reset-mouse-reporting (ui)
  "Forget UI's mouse state, since leaving fullscreen ends reporting."
  (remhash ui *xenolith-helix-mouse-states*)
  (values))


(defun xenolith-helix--reset-cursor-style (ui)
  "Restore UI's terminal default cursor shape."
  (when (gethash ui *xenolith-helix-cursor-styles*)
    (let ((terminal (terminal-ui-terminal ui)))
      (terminal--write terminal (xenolith-helix--escape-sequence "[0 q"))
      (terminal-flush terminal)
      (remhash ui *xenolith-helix-cursor-styles*))))

(defun xenolith-helix--fullscreen-paint (ui &rest arguments)
  "Paint UI and keep the terminal presentable for its mode."
  (prog1 (apply *xenolith-original-terminal-ui-fullscreen-paint* ui arguments)
    (handler-case
        (xenolith-helix--paint-mode-presentation ui)
      (serious-condition (condition)
        (setf *xenolith-helix-failure* condition)
        (xenolith-log "helix: mode presentation FAILED ~A" condition)))))

(defun xenolith-helix--fullscreen-enter (ui &rest arguments)
  "Enter fullscreen and forget the mouse state its entry sequence overwrote."
  (prog1 (apply *xenolith-original-terminal-ui-fullscreen-enter* ui arguments)
    (handler-case
        (xenolith-helix--reset-mouse-reporting ui)
      (serious-condition (condition)
        (setf *xenolith-helix-failure* condition)
        (xenolith-log "helix: mouse state FAILED ~A" condition)))))

(defun xenolith-helix--fullscreen-leave (ui &rest arguments)
  "Leave fullscreen and restore the terminal default cursor and mouse state."
  (prog1 (apply *xenolith-original-terminal-ui-fullscreen-leave* ui arguments)
    (handler-case
        (progn
          (xenolith-helix--reset-cursor-style ui)
          (xenolith-helix--reset-mouse-reporting ui))
      (serious-condition (condition)
        (setf *xenolith-helix-failure* condition)
          (xenolith-log "helix: terminal restore FAILED ~A" condition)))))

;;;; -- UI hooks --

(defvar *xenolith-original-application-terminal-ui-create* nil
  "The UI create builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-original-terminal-ui-process-event* nil
  "The UI event builder underneath this module's wrapper, or NIL.")

(defun xenolith-helix--create-ui (&rest arguments)
  "Create the application UI and install Helix modality on it."
  (let ((ui (apply *xenolith-original-application-terminal-ui-create* arguments)))
    (when *xenolith-install-helix-p*
      (handler-case
          (xenolith-helix-install ui)
        (serious-condition (condition)
          (setf *xenolith-helix-failure* condition)
          (xenolith-log "helix: install FAILED ~A" condition))))
    ui))

(defun xenolith-helix--process-event (ui event &key queue-completion-p queue-editing-p)
  "Handle mode-owned events before the tracked completion policy sees them.

Insert mode opens command mode on a colon that starts an empty prompt, so the
command line is one key away from the readline-style prompt. Normal mode owns
the whole keymap, so completion there only repaints and never submits the
draft. Command mode leaves completion to the tracked policy, which opens the
same candidate menu the slash and parenthesized lines show."
  (let ((state (xenolith-helix--state-for-ui ui)))
    (cond
      ((and state
            (eq (xenolith-helix-state-mode state) :insert)
            (equal event '(:insert ":"))
            (zerop (length (line-editor-text
                            (xenolith-helix-state-editor state)))))
       (xenolith-helix--enter-command state)
       (with-terminal-ui-locked (ui)
         (terminal-ui--repaint-live ui))
       (values :changed nil))
      ((and state
            (eq (xenolith-helix-state-mode state) :normal)
            (member event '(:complete :complete-previous) :test #'eq))
       (with-terminal-ui-locked (ui)
         (terminal-ui--repaint-live ui))
       (values :changed nil))
      (t
       (funcall *xenolith-original-terminal-ui-process-event*
                ui event
                :queue-completion-p queue-completion-p
                :queue-editing-p queue-editing-p)))))

;;;; -- Escape routing --

(defvar *xenolith-helix-escape-mode* nil
  "The Helix mode an escape event arrived in, or NIL for another key.")

(defvar *xenolith-original-application-input-controller--process-event* nil
  "The tracked input controller event processor captured before the first wrap.")

(defvar *xenolith-original-application-input-controller--request-active-turn-cancellation*
  nil
  "The tracked turn cancellation request captured before the first wrap.")

(defun xenolith-helix--state-mode-for-editor (editor)
  "Return EDITOR's Helix mode, or NIL when it has no state."
  (let ((state (and editor (xenolith-helix--state-for-editor editor))))
    (and state (xenolith-helix-state-mode state))))

(defun xenolith-helix--escape-passthrough-p (mode)
  "Return true when an escape in MODE belongs to the editor, not the turn.

Insert, select, and command mode leave MODE on escape; only normal mode hands
the escape to the turn, so the first escape never cancels a turn the reader is
still typing into."
  (and mode (not (eq mode :normal))))

(defun xenolith-helix--controller-mode (controller)
  "Return the Helix mode of CONTROLLER's editor, or NIL."
  (let* ((application (ignore-errors
                        (application-input-controller-application controller)))
         (ui        (and application (ignore-errors (application-ui application))))
         (editor    (and ui (ignore-errors (terminal-ui-editor ui)))))
    (xenolith-helix--state-mode-for-editor editor)))

(defun xenolith-helix--escape-process-event (controller event)
  "Record the mode EVENT arrives in, then route it as the tracked handler does."
  (let ((*xenolith-helix-escape-mode*
          (and (eq event :escape)
               (xenolith-helix--controller-mode controller))))
    (funcall *xenolith-original-application-input-controller--process-event*
             controller event)))

(defun xenolith-helix--escape-turn-cancellation (controller &rest arguments)
  "Decline an escape-driven cancellation while the editor is outside normal mode.

The escape then reaches the editor, which leaves insert, select, or command
mode; the next escape, from normal mode, interrupts the turn."
  (if (xenolith-helix--escape-passthrough-p *xenolith-helix-escape-mode*)
      nil
      (apply
       *xenolith-original-application-input-controller--request-active-turn-cancellation*
       controller arguments)))

(defvar *xenolith-helix-input-event-wrapper* nil
  "The input event wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-cancellation-wrapper* nil
  "The turn cancellation wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-ui-create-wrapper* nil
  "The UI create wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-ui-event-wrapper* nil
  "The UI event wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-completions-wrapper* nil
  "The completion wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-status-details-wrapper* nil
  "The status detail wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-prompt-wrapper* nil
  "The prompt renderer wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-paint-wrapper* nil
  "The fullscreen paint wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-leave-wrapper* nil
  "The fullscreen leave wrapper this module installed, for reload detection.")

(defvar *xenolith-helix-enter-wrapper* nil
  "The fullscreen enter wrapper this module installed, for reload detection.")

(defparameter *xenolith-helix-hooks*
  (list (list 'application-input-controller--process-event
              '*xenolith-original-application-input-controller--process-event*
              '*xenolith-helix-input-event-wrapper*
              #'xenolith-helix--escape-process-event)
        (list 'application-input-controller--request-active-turn-cancellation
              '*xenolith-original-application-input-controller--request-active-turn-cancellation*
              '*xenolith-helix-cancellation-wrapper*
              #'xenolith-helix--escape-turn-cancellation)
        (list 'application-terminal-ui-create
              '*xenolith-original-application-terminal-ui-create*
              '*xenolith-helix-ui-create-wrapper*
              #'xenolith-helix--create-ui)
        (list 'terminal-ui-process-event
              '*xenolith-original-terminal-ui-process-event*
              '*xenolith-helix-ui-event-wrapper*
              #'xenolith-helix--process-event)
        (list 'terminal-ui--matching-completions
              '*xenolith-original-terminal-ui--matching-completions*
              '*xenolith-helix-completions-wrapper*
              #'xenolith-helix--matching-completions)
        (list 'application--status-details
              '*xenolith-original-application--status-details*
              '*xenolith-helix-status-details-wrapper*
              #'xenolith-helix--status-details)
        (list 'terminal-ui--render-prompt-content
              '*xenolith-original-terminal-ui--render-prompt-content*
              '*xenolith-helix-prompt-wrapper*
              #'xenolith-helix--render-prompt-content-hook)
        (list 'terminal-ui-fullscreen-paint
              '*xenolith-original-terminal-ui-fullscreen-paint*
              '*xenolith-helix-paint-wrapper*
              #'xenolith-helix--fullscreen-paint)
        (list 'terminal-ui-fullscreen-leave
              '*xenolith-original-terminal-ui-fullscreen-leave*
              '*xenolith-helix-leave-wrapper*
              #'xenolith-helix--fullscreen-leave)
        (list 'terminal-ui-fullscreen-enter
              '*xenolith-original-terminal-ui-fullscreen-enter*
              '*xenolith-helix-enter-wrapper*
              #'xenolith-helix--fullscreen-enter))
  "The tracked definitions this module wraps, as (NAME PREVIOUS-VAR WRAPPER-VAR WRAPPER).")

(defun xenolith-helix--install-hooks ()
  "Install the Helix UI hooks and refresh installed editors."
  (dolist (hook *xenolith-helix-hooks*)
    (destructuring-bind (name previous-var wrapper-var wrapper) hook
      (when (fboundp name)
        (xenolith--chain name previous-var wrapper-var wrapper))))
  (when (boundp '*xenolith-composer-mode-label-function*)
    (setf *xenolith-composer-mode-label-function* nil))
  (maphash (lambda (editor state)
             (declare (ignore editor))
             (xenolith-helix--apply-mode state))
           *xenolith-helix-states*))

(defun xenolith-helix--remove-hooks ()
  "Remove the Helix UI hooks and restore installed editors."
  (dolist (hook *xenolith-helix-hooks*)
    (destructuring-bind (name previous-var wrapper-var wrapper) hook
      (declare (ignore wrapper))
      (xenolith--unchain name previous-var wrapper-var)))
  (when (boundp '*xenolith-composer-mode-label-function*)
    (setf *xenolith-composer-mode-label-function* nil))
  (maphash (lambda (editor state)
             (declare (ignore editor))
             (xenolith-helix--reset-cursor-style
              (xenolith-helix-state-ui state))
             (xenolith-helix--restore-defaults
              (clinedi:line-editor-keymap
               (xenolith-helix-state-editor state))))
           *xenolith-helix-states*))

(if *xenolith-install-helix-p*
    (xenolith-helix--install-hooks)
    (xenolith-helix--remove-hooks))
