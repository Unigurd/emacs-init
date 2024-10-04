;; -*- lexical-binding: t -*-


(require 'gurd-utils)

(defmacro gurd-maybe-error (expression)
  `(condition-case err
       (list 'ok ,expression)
     (error (list 'fail error))))

(defmacro gurd-signal-equal (&rest rest)
  `(equal* ,@(mapcar (lambda (expr) `(gurd-maybe-error ,expr))
                     rest)))

(defmacro gurd-signal-equalish (&rest rest)
  )

(ert-deftest gurd-signal-equalish ()
  (should (gurd-signal-equal (foo) (bar)))
  (should (gurd-signal-equal (foo) (bar) (baz)))
  (should (gurd-signal-equal (+ 1 2) (+ 2 1)))
  (should (not (gurd-signal-equal 1 2)))
  (should (not (gurd-signal-equal 'void-function (foo))))
  (should (not (gurd-signal-equal (/ 1 0) (foo))))
  (should (gurd-signal-equal)))

(provide 'test-utils)
