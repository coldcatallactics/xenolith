(in-package #:autolith)

;;;; Xenolith site initialization.
;;;;
;;;; Autolith loads this file from --site-config-root (or
;;;; AUTOLITH_SITE_CONFIG_ROOT) before trusted directory and user
;;;; initialization, after the tracked image and any selected private image
;;;; commit. Modules add presentation and behavior; they do not modify the
;;;; tracked source tree.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *xenolith-init-root*
    (uiop:pathname-directory-pathname
     (uiop:ensure-absolute-pathname *user-init-pathname*))
    "The site root of the init file being loaded.")
  (load (merge-pathnames "xenolith.lisp" *xenolith-init-root*)
        :verbose nil :print nil))

(eval-when (:load-toplevel :execute)
  (xenolith-load-modules :root *xenolith-init-root*))
