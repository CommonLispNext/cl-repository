(defpackage :cl-repository-client/asdf-integration
  (:use :cl)
  (:import-from :cl-repository-client/installer #:systems-root)
  (:export #:configure-asdf-source-registry
           #:load-system-init-files))
(in-package :cl-repository-client/asdf-integration)

(defun configure-asdf-source-registry ()
  "Prepend the cl-repository systems directory to the ASDF source registry.
   Preserves existing programmatic configuration (e.g. Quicklisp paths)."
  (let ((root (systems-root)))
    (when (probe-file root)
      (let* ((root-str (namestring root))
             (current asdf/source-registry:*source-registry-parameter*)
             (new-config
               (if (and (listp current)
                        (eq (first current) :source-registry))
                   ;; Prepend our tree to existing config entries
                   `(:source-registry
                     (:tree ,root-str)
                     ,@(rest current))
                   ;; No prior programmatic config — inherit defaults
                   `(:source-registry
                     (:tree ,root-str)
                     :inherit-configuration))))
        (asdf:initialize-source-registry new-config)))))

(defun load-system-init-files ()
  "Load cl-repo-init.lisp files from all installed systems for CFFI setup."
  (let ((root (systems-root)))
    (when (probe-file root)
      (dolist (system-dir (uiop:subdirectories root))
        (dolist (version-dir (uiop:subdirectories system-dir))
          (let ((init-file (merge-pathnames "cl-repo-init.lisp" version-dir)))
            (when (probe-file init-file)
              (load init-file))))))))
