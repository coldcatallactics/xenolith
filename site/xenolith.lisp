(in-package #:autolith)

;;;; Xenolith: loader for the distribution's site layer.
;;;;
;;;; The manifest names module files relative to the site root. Each module is
;;;; ordinary Common Lisp in package AUTOLITH; a module that fails is recorded
;;;; and reported instead of aborting startup, so one broken module degrades
;;;; the distribution rather than the session.

(defparameter *xenolith-version* "0.1.0"
  "The Xenolith distribution version.")

(defparameter *xenolith-modules*
  '(("theme/gruvbox" . "modules/theme/gruvbox.lisp")
    ("look/fullscreen" . "modules/look/fullscreen.lisp")
    ("look/composer" . "modules/look/composer.lisp")
    ("look/spacing" . "modules/look/spacing.lisp")
    ("look/surfaces" . "modules/look/surfaces.lisp")
    ("look/band" . "modules/look/band.lisp")
    ("look/legend" . "modules/look/legend.lisp")
    ("look/markdown" . "modules/look/markdown.lisp")
    ("edit/helix" . "modules/edit/helix.lisp")
    ("dev/preview" . "modules/dev/preview.lisp")
    ("dev/context-budget" . "modules/dev/context-budget.lisp"))
  "Ordered (NAME . RELATIVE-PATH) modules, relative to the site root.")

(pushnew :xenolith *features* :test #'eq)

(defvar *xenolith-site-root* nil
  "The site root the current module set was loaded from.")

(defvar *xenolith-module-records* nil
  "Module load records in manifest order for the newest load.")

(defvar *xenolith-install-themes-p* t
  "Whether modules may install a presentation theme into the live image.

Checks and headless tooling bind this to NIL to load the site layer without
changing the session's presentation.")

(defun xenolith-site-root (&optional pathname)
  "Return the site root for PATHNAME, defaulting to the loading init file."
  (let ((pathname (or pathname *user-init-pathname*)))
    (cond
      (pathname
       (uiop:pathname-directory-pathname
        (uiop:ensure-absolute-pathname pathname)))
      ((uiop:getenv "AUTOLITH_SITE_CONFIG_ROOT")
       (uiop:ensure-directory-pathname
        (uiop:getenv "AUTOLITH_SITE_CONFIG_ROOT")))
      (t
       (error "Xenolith cannot determine its site root.")))))

(defun xenolith-log (control &rest arguments)
  "Append one diagnostic line to XENOLITH_LOG when it names a file."
  (let ((path (uiop:getenv "XENOLITH_LOG")))
    (when (and path (plusp (length path)))
      (handler-case
          (with-open-file (stream path
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
            (apply #'format stream control arguments)
            (terpri stream))
          (error () nil)))))

(defun xenolith--seconds-since (start)
  "Return the seconds elapsed since START as a float."
  (/ (float (- (get-internal-real-time) start))
     internal-time-units-per-second))

(defvar *xenolith-chain-bases* (make-hash-table :test #'eq)
  "The definition underneath every Xenolith wrapper, per wrapped function name.")

(defvar *xenolith-chains* (make-hash-table :test #'eq)
  "The active wrappers per wrapped function name, innermost first.

Each entry is (WRAPPER-VAR PREVIOUS-VAR WRAPPER).")

(defun xenolith--rechain (name)
  "Rebuild NAME's wrappers from the definition underneath and install the last."
  (let ((below (first (gethash name *xenolith-chain-bases*))))
    (dolist (entry (gethash name *xenolith-chains*))
      (destructuring-bind (wrapper-var previous-var wrapper) entry
        (setf (symbol-value wrapper-var) wrapper
              (symbol-value previous-var) below
              below wrapper)))
    (when below
      (setf (symbol-function name) below)))
  name)

(defun xenolith--chain (name previous-var wrapper-var wrapper)
  "Install WRAPPER over NAME, remembering the module by WRAPPER-VAR.

The definition underneath every Xenolith wrapper for NAME is captured once, and
wrappers keep the order they were first installed in. Reloading one module
replaces its entry and leaves the other modules' wrappers where they are."
  (unless (gethash name *xenolith-chain-bases*)
    (setf (gethash name *xenolith-chain-bases*)
          (list (and (fboundp name) (symbol-function name)))))
  (let* ((entries (gethash name *xenolith-chains*))
         (entry   (assoc wrapper-var entries)))
    (if entry
        (setf (second entry) previous-var
              (third entry)  wrapper)
        (setf (gethash name *xenolith-chains*)
              (nconc entries (list (list wrapper-var previous-var wrapper))))))
  (xenolith--rechain name)
  name)

(defun xenolith--unchain (name previous-var wrapper-var)
  "Remove this module's wrapper from NAME and restore the definition below it.

A module that wrapped NAME after this one keeps its wrapper installed. When the
last wrapper for NAME goes, the captured definition is restored and forgotten."
  (declare (ignore previous-var))
  (let ((entries (gethash name *xenolith-chains*)))
    (when entries
      (let ((remaining (remove wrapper-var entries :key #'first)))
        (if remaining
            (progn
              (setf (gethash name *xenolith-chains*) remaining)
              (xenolith--rechain name))
            (progn
              (remhash name *xenolith-chains*)
              (let ((base (first (gethash name *xenolith-chain-bases*))))
                (when (and base (fboundp name))
                  (setf (symbol-function name) base))
                (remhash name *xenolith-chain-bases*)))))))
  (setf (symbol-value wrapper-var) nil)
  name)

(defun xenolith--release-chains ()
  "Restore every wrapped function to its captured definition and forget the wrappers.

The loader calls this before a fresh module pass, so wrappers are rebuilt from
the tracked definitions in manifest order instead of from the previous pass."
  (maphash (lambda (name entries)
             (declare (ignore entries))
             (let ((base (first (gethash name *xenolith-chain-bases*))))
               (when (and base (fboundp name))
                 (setf (symbol-function name) base))))
           *xenolith-chains*)
  (clrhash *xenolith-chains*)
  (clrhash *xenolith-chain-bases*)
  (values))

(defun xenolith--env-integer (name default)
  "Return the integer NAME names in the environment, or DEFAULT.

A value that is not an integer keeps DEFAULT, so a typo cannot break the look."
  (let ((value (uiop:getenv name)))
    (or (and value
             (plusp (length value))
             (ignore-errors (parse-integer value :junk-allowed t)))
        default)))

(defun xenolith--fill-after-resets (text fill)
  "Return TEXT with FILL written again after every style reset.

FILL is a style's terminal sequence, so a row keeps its background and
foreground across the resets the renderer emits between spans. A NIL or empty
FILL returns TEXT unchanged."
  (if (or (null fill) (zerop (length fill)) (zerop (length text)))
      text
      (with-output-to-string (out)
        (write-string fill out)
        (loop with reset = *terminal-style-reset*
              with start = 0
              for position = (search reset text :start2 start)
              while position
              for next = (+ position (length reset))
              do (write-string text out :start start :end next)
                 (write-string fill out)
                 (setf start next)
              finally (write-string text out :start start)))))

(defun xenolith-load-module (name pathname)
  "Load module NAME from PATHNAME, returning a record and signaling nothing."
  (let ((start (get-internal-real-time)))
    (handler-case
        (progn
          (load pathname :verbose nil :print nil)
          (list :name name :path pathname :status :loaded
                :seconds (xenolith--seconds-since start)))
      (serious-condition (condition)
        (format *error-output* "~&Xenolith: module ~A failed: ~A~%"
                name condition)
        (list :name name :path pathname :status :failed
              :seconds (xenolith--seconds-since start)
              :condition condition)))))

(defun xenolith-load-modules (&key root)
  "Load every module in *XENOLITH-MODULES* from ROOT in manifest order.

A previous pass's wrappers are released first, so the pass rebuilds them from
the tracked definitions in manifest order instead of stacking onto its own work."
  (let ((root (or root (xenolith-site-root)))
        (records '()))
    (setf *xenolith-site-root* root)
    (xenolith--release-chains)
    (dolist (entry *xenolith-modules*)
      (destructuring-bind (name . relative) entry
        (let ((pathname (merge-pathnames relative root)))
          (push (if (probe-file pathname)
                    (xenolith-load-module name pathname)
                    (list :name name :path pathname
                          :status :missing :seconds 0.0))
                records))))
    (setf *xenolith-module-records* (nreverse records))))

(defun xenolith-report (&optional (stream *standard-output*))
  "Print Xenolith's version, site root, theme, and module records."
  (format stream "~&xenolith ~A~%  site  ~A~%  theme ~S~%"
          *xenolith-version*
          (or *xenolith-site-root* "unloaded")
          (ignore-errors (terminal-theme-name *terminal-theme*)))
  (dolist (record *xenolith-module-records*)
    (format stream "  ~12A ~8A ~6,1F ms~%"
            (getf record :name)
            (string-downcase (symbol-name (getf record :status)))
            (* 1000 (getf record :seconds))))
  (values))
