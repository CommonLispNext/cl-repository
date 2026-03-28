(defpackage :cl-repository-client/protected-systems
  (:use :cl)
  (:export #:*protected-system-prefixes*
           #:*protect-loaded-systems*
           #:*loaded-systems-snapshot*
           #:snapshot-loaded-systems
           #:ensure-snapshot
           #:system-protected-p))
(in-package :cl-repository-client/protected-systems)

(defvar *protected-system-prefixes*
  '("swank" "slynk")
  "System name prefixes that should not be updated or reinstalled.
   Covers both the main system and subsystems (e.g. swank/sbcl, slynk/mrepl).
   A prefix protects only when the corresponding package is present in the image.")

(defvar *protect-loaded-systems* t
  "When true, systems captured in the loaded-systems snapshot are protected from updates.")

(defvar *loaded-systems-snapshot* nil
  "Hash-set of system names (strings) captured by SNAPSHOT-LOADED-SYSTEMS.
   NIL means snapshot not yet taken.")

(defun snapshot-loaded-systems ()
  "Capture all currently registered ASDF systems into *LOADED-SYSTEMS-SNAPSHOT*.
   Typically called once on first use of load-system or cmd-update."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (sys (asdf:registered-systems))
      (setf (gethash (string-downcase sys) ht) t))
    (setf *loaded-systems-snapshot* ht)))

(defun ensure-snapshot ()
  "Take snapshot if *PROTECT-LOADED-SYSTEMS* is T and no snapshot exists yet."
  (when (and *protect-loaded-systems* (null *loaded-systems-snapshot*))
    (snapshot-loaded-systems)))

(defun prefix-matches-p (name prefix)
  "Return T when NAME matches PREFIX exactly or as a parent system (prefix/)."
  (or (string= name prefix)
      (and (> (length name) (length prefix))
           (string= name prefix :end1 (length prefix))
           (char= (char name (length prefix)) #\/))))

(defun system-protected-p (system-name)
  "Return T when SYSTEM-NAME should not be updated/reinstalled.
   Protected if:
   1. Name matches a prefix from *PROTECTED-SYSTEM-PREFIXES* AND that prefix's
      package is present in the image, OR
   2. *PROTECT-LOADED-SYSTEMS* is T and name is in the snapshot."
  (let ((name (string-downcase (string system-name))))
    (or
     (some (lambda (prefix)
             (and (prefix-matches-p name prefix)
                  (find-package (string-upcase prefix))))
           *protected-system-prefixes*)
     (and *protect-loaded-systems*
          *loaded-systems-snapshot*
          (gethash name *loaded-systems-snapshot*)))))
