(in-package #:autolith)

;;;; Xenolith dev: opt-in context budget for child agents.
;;;;
;;;; A long child run degrades before it ends: attention dilutes as the window
;;;; fills, and the work a saturated run reports is worse than the work a fresh
;;;; one would do. This module delivers one phase instruction at a request-safe
;;;; boundary, so a role that opts in can finish and hand off while it is still
;;;; sharp.
;;;;
;;;; The distribution ships the mechanism only. Which agent runs long, and how
;;;; its workflow reads, is a property of the layer that owns that agent, so a
;;;; policy layer names the roles and may replace the wording. With
;;;; *XENOLITH-CONTEXT-BUDGET-ROLES* empty this module delivers nothing. The
;;;; phase text carries no number: the model is told what to do, not how full
;;;; its window is.

(defparameter *xenolith-context-budget-p*
  (not (equal (uiop:getenv "XENOLITH_CONTEXT_BUDGET") "off"))
  "Whether the site layer may deliver context-budget phases to a child agent.")

(defparameter *xenolith-context-budget-default-phases*
  '(:finish 0.30d0 :finalize 0.40d0 :urgent 0.46d0)
  "Ascending fill ratios of the configured context window, one per phase.")

(defparameter *xenolith-context-budget-default-instructions*
  '(:finish
    "Context budget: finish only. Complete the work already in progress to a coherent state and start nothing new: no new item, no new file, no new delegated agent."
    :finalize
    "Context budget: finalize now. Stop expanding scope. Verify what is already done with the project's own checks, record the result, leave the tree in a consistent state, and finish the turn with a full report."
    :urgent
    "Context budget: exhausted. Make no further changes. Report the state as it stands and name anything unrun, failing, or unfinished.")
  "Workflow-neutral phase text for a policy layer that supplies none of its own.")

(defvar *xenolith-context-budget-roles* '()
  "Child agent definition names this module bounds.

A name is matched against the sentence Autolith writes into a child's goal
context, \"You are child agent <id> of type <name>, depth <n>.\", so it must
equal the agent definition's :name. An empty list disables delivery, and a
policy layer outside this tree owns the list.")

(defvar *xenolith-context-budget-phases* *xenolith-context-budget-default-phases*
  "Ascending fill ratios per phase as a plist.

A value that is not three ascending ratios in (0 0.95) is replaced by the
default and logged once.")

(defvar *xenolith-context-budget-instructions* *xenolith-context-budget-default-instructions*
  "Phase instruction text as a plist, falling back per missing phase.")

(defparameter *xenolith-context-budget-state-limit* 64
  "Conversations retaining a latched phase, newest activity first.")

(defparameter *xenolith-context-budget-ranks*
  '((:none . 0) (:finish . 1) (:finalize . 2) (:urgent . 3))
  "Phase order used to latch the highest phase a conversation reached.")
(defvar *xenolith-context-budget-minimum-window* 1000000
  "The smallest context window this module bounds, in tokens.

The phases matter on a large window, where a long run can fill hundreds of
thousands of tokens before it loses the task. A smaller window reaches its own
compaction limit first, so the module stays silent there.")

(defvar *xenolith-context-budget-states* (make-hash-table :test #'equal)
  "Conversation identifiers mapped to their latched phase, fill and time.")

(defvar *xenolith-context-budget-order* nil
  "Conversation identifiers with latched phases, newest activity first.")

(defvar *xenolith-context-budget-lock* (make-lock "Xenolith context budget")
  "The lock protecting the latch and its eviction order.")

(defvar *xenolith-context-budget-failure* nil
  "The most recent condition or bad threshold set this module degraded on.")

(defvar *xenolith-context-budget-phases-warned-p* nil
  "Whether a bad threshold set has already been logged once.")

(defun xenolith-context-budget--rank (phase)
  "Return PHASE's order in the phase ladder, or 0 for an unknown phase."
  (or (cdr (assoc phase *xenolith-context-budget-ranks*)) 0))

(defun xenolith-context-budget--ratio-p (value)
  "Return true when VALUE is usable as a fill ratio."
  (and (realp value) (< 0 value) (< value 0.95)))

(defun xenolith-context-budget--phases ()
  "Return the phase ratios as a plist, falling back to the defaults."
  (let ((phases *xenolith-context-budget-phases*))
    (if (and (xenolith-context-budget--ratio-p (getf phases :finish))
             (xenolith-context-budget--ratio-p (getf phases :finalize))
             (xenolith-context-budget--ratio-p (getf phases :urgent))
             (< (getf phases :finish) (getf phases :finalize) (getf phases :urgent)))
        phases
        (progn
          (unless *xenolith-context-budget-phases-warned-p*
            (setf *xenolith-context-budget-phases-warned-p* t
                  *xenolith-context-budget-failure* :phases)
            (xenolith-log "Xenolith: context budget phases ~S are not ascending ratios; using defaults."
                          phases))
          *xenolith-context-budget-default-phases*))))

(defun xenolith-context-budget--phase-for (fill phases)
  "Return the phase FILL implies under PHASES, without touching any state."
  (cond ((not (realp fill)) :none)
        ((>= fill (getf phases :urgent)) :urgent)
        ((>= fill (getf phases :finalize)) :finalize)
        ((>= fill (getf phases :finish)) :finish)
        (t :none)))

(defun xenolith-context-budget--advance (identifier fill phases)
  "Latch FILL's phase for IDENTIFIER and return the highest phase it reached.

A conversation with no latched phase records nothing until FILL reaches the
first phase, so the latch does not fill with conversations that never needed it."
  (let ((candidate (xenolith-context-budget--phase-for fill phases)))
    (with-lock-held (*xenolith-context-budget-lock*)
      (let* ((entry   (gethash identifier *xenolith-context-budget-states*))
             (current (and entry (getf entry :phase)))
             (phase   (if (and current
                               (<= (xenolith-context-budget--rank candidate)
                                   (xenolith-context-budget--rank current)))
                          current
                          candidate)))
        (if (and (null entry) (eq phase :none))
            :none
            (progn
              (setf (gethash identifier *xenolith-context-budget-states*)
                    (list :phase phase :fill fill :updated (get-universal-time))
                    *xenolith-context-budget-order*
                    (cons identifier
                          (remove identifier *xenolith-context-budget-order*
                                  :test #'string=)))
              (let ((evicted (nthcdr *xenolith-context-budget-state-limit*
                                     *xenolith-context-budget-order*)))
                (dolist (evicted-identifier evicted)
                  (remhash evicted-identifier *xenolith-context-budget-states*))
                (when evicted
                  (setf *xenolith-context-budget-order*
                        (subseq *xenolith-context-budget-order*
                                0 *xenolith-context-budget-state-limit*))))
              (when (and (not (eq phase current)) (not (eq phase :none)))
                (xenolith-log "Xenolith: context budget ~A at ~,3F for ~A"
                              phase fill identifier))
              phase))))))

(defun xenolith-context-budget--window (configuration)
  "Return CONFIGURATION's context window when it is large enough to bound."
  (let ((window (ignore-errors (config :context-window configuration))))
    (and (integerp window)
         (>= window *xenolith-context-budget-minimum-window*)
         window)))

(defun xenolith-context-budget--fill (configuration conversation)
  "Return (values FILL-RATIO KNOWN-P) for CONVERSATION under CONFIGURATION.

The reported total is the previous request's usage, one request stale and
counting that response's output, so it errs high. The estimate is the idle
meter's fallback, which omits request-local contributions and, without a
registry, tool schemas, so it errs low. Both are used to pick a phase boundary
and neither is ever rendered anywhere."
  (let ((window (xenolith-context-budget--window configuration)))
    (when window
      (let ((reported (ignore-errors
                        (conversation-last-total-tokens conversation))))
        (cond
          ((and (integerp reported) (plusp reported))
           (values (/ (float reported 1d0) window) t))
          (t
           (let ((estimate (ignore-errors
                             (context-estimate-known-tokens configuration conversation))))
             (when (and (integerp estimate) (not (minusp estimate)))
               (values (/ (float estimate 1d0) window) t)))))))))

(defun xenolith-context-budget--role (goal)
  "Return the configured role GOAL names, or NIL."
  (when (stringp goal)
    (find-if (lambda (role)
               (and (stringp role)
                    (plusp (length role))
                    (search (format nil "of type ~A," role) goal :test #'char=)))
             *xenolith-context-budget-roles*)))

(defun xenolith-context-budget--instruction (phase)
  "Return the instruction text for PHASE."
  (or (getf *xenolith-context-budget-instructions* phase)
      (getf *xenolith-context-budget-default-instructions* phase)))

(define-context-contributor xenolith-context-budget (request)
  "Deliver the configured phase to a bounded child role."
  (when (and *xenolith-context-budget-p* *xenolith-context-budget-roles*)
    (handler-case
        (unless (request-context-compaction-p request)
          (let ((role (xenolith-context-budget--role
                       (request-context-goal-context request))))
            (when role
              (let ((conversation (request-context-conversation request)))
                (multiple-value-bind (fill known-p)
                    (xenolith-context-budget--fill
                     (request-context-configuration request) conversation)
                  (when known-p
                    (let ((phase (xenolith-context-budget--advance
                                  (conversation-identifier conversation)
                                  fill
                                  (xenolith-context-budget--phases))))
                      (unless (eq phase :none)
                        (make-context-contribution
                         :identifier  "xenolith-context-budget"
                         :instruction (xenolith-context-budget--instruction phase)
                         :priority    90
                         :class       ':mandatory
                         :lifetime    ':while-relevant)))))))))
      (serious-condition (condition)
        (setf *xenolith-context-budget-failure* condition)
        (xenolith-log "Xenolith: context budget contributor failed: ~A" condition)
        nil))))

(defun xenolith-context-budget-status ()
  "Return (CONVERSATION-ID PHASE FILL) rows, newest activity first."
  (with-lock-held (*xenolith-context-budget-lock*)
    (loop for identifier in *xenolith-context-budget-order*
          for entry = (gethash identifier *xenolith-context-budget-states*)
          when entry
            collect (list identifier (getf entry :phase) (getf entry :fill)))))

(defun xenolith-context-budget-reset ()
  "Forget every latched phase and return NIL."
  (with-lock-held (*xenolith-context-budget-lock*)
    (clrhash *xenolith-context-budget-states*)
    (setf *xenolith-context-budget-order* nil))
  nil)
