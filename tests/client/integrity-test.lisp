(defpackage :cl-repository-client/tests/integrity-test
  (:use :cl :rove)
  (:import-from :cl-repository-client/integrity
                #:record-file-manifest #:verify-installed-system
                #:verification-result-status #:verification-result-modified-files
                #:verification-result-added-files #:verification-result-removed-files))
(in-package :cl-repository-client/tests/integrity-test)

(defun make-temp-dir ()
  "Create a unique temp directory for testing."
  (let ((path (merge-pathnames
               (format nil "cl-repo-test-~a/" (get-universal-time))
               (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "x" path))
    path))

(defun write-test-file (dir name content)
  (let ((path (merge-pathnames name dir)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string content s))
    path))

(defun cleanup-dir (dir)
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

(deftest test-record-and-verify-ok
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (progn
           (write-test-file dir "hello.lisp" "(defun hello () :hello)")
           (write-test-file dir "sub/deep.lisp" "(defun deep () :deep)")
           (let ((entries (record-file-manifest dir)))
             (ok (= 2 (length entries)))
             (ok (assoc "hello.lisp" entries :test #'string=))
             (ok (assoc "sub/deep.lisp" entries :test #'string=)))
           (let ((result (verify-installed-system dir :name "test" :version "1.0")))
             (ok (eq :ok (verification-result-status result)))
             (ok (null (verification-result-modified-files result)))
             (ok (null (verification-result-added-files result)))
             (ok (null (verification-result-removed-files result)))))
      (cleanup-dir dir))))

(deftest test-verify-modified-file
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (progn
           (write-test-file dir "code.lisp" "(original)")
           (record-file-manifest dir)
           ;; Modify file
           (write-test-file dir "code.lisp" "(modified)")
           (let ((result (verify-installed-system dir :name "test" :version "1.0")))
             (ok (eq :modified (verification-result-status result)))
             (ok (equal '("code.lisp") (verification-result-modified-files result)))
             (ok (null (verification-result-added-files result)))
             (ok (null (verification-result-removed-files result)))))
      (cleanup-dir dir))))

(deftest test-verify-added-file
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (progn
           (write-test-file dir "original.lisp" "(original)")
           (record-file-manifest dir)
           ;; Add new file
           (write-test-file dir "extra.lisp" "(extra)")
           (let ((result (verify-installed-system dir :name "test" :version "1.0")))
             (ok (eq :modified (verification-result-status result)))
             (ok (null (verification-result-modified-files result)))
             (ok (equal '("extra.lisp") (verification-result-added-files result)))
             (ok (null (verification-result-removed-files result)))))
      (cleanup-dir dir))))

(deftest test-verify-removed-file
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (progn
           (write-test-file dir "keep.lisp" "(keep)")
           (write-test-file dir "gone.lisp" "(gone)")
           (record-file-manifest dir)
           ;; Remove file
           (delete-file (merge-pathnames "gone.lisp" dir))
           (let ((result (verify-installed-system dir :name "test" :version "1.0")))
             (ok (eq :modified (verification-result-status result)))
             (ok (null (verification-result-modified-files result)))
             (ok (null (verification-result-added-files result)))
             (ok (equal '("gone.lisp") (verification-result-removed-files result)))))
      (cleanup-dir dir))))

(deftest test-verify-no-manifest
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (progn
           (write-test-file dir "file.lisp" "(test)")
           (let ((result (verify-installed-system dir :name "test" :version "1.0")))
             (ok (eq :no-manifest (verification-result-status result)))))
      (cleanup-dir dir))))

(deftest test-manifest-excludes-itself
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (progn
           (write-test-file dir "code.lisp" "(code)")
           (let ((entries (record-file-manifest dir)))
             (ok (= 1 (length entries)))
             (ok (null (assoc ".cl-repo-manifest.sexp" entries :test #'string=)))))
      (cleanup-dir dir))))
