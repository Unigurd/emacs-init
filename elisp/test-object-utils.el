;; -*- lexical-binding: t -*-

(require 'object-utils)

(defclass gurd-test-req-param (gurd-req-param)
  ((required :initarg :required)
   (with-default :initarg :with-default :initform 1)
   (non-param)))

(ert-deftest gurd-req-param ()
  (should-error (gurd-test-req-param) :type 'unbound-slot)
  ;; This test will throw an unbound-slot error if it fails
  (should (gurd-test-req-param :required 'foo)))
