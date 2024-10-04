;; -*- lexical-binding: t -*-

(require 'gurd-utils)

(ert-deftest equal* ()
  (should (equal*))
  (should (equal* 1))
  (should (equal* 1 1))
  (should (equal* '(1 2) '(1 2) '(1 2)))
  (should (not (equal* '(1 2) '(1 2) '(1 3)))))

(ert-deftest flatmap ()
  (should (equal (list 1 1 2 2 3 3)
                 (flatmap '(lambda (x) (list x x)) '(1 2 3)))))


(ert-deftest gurd-factorial ()
  (should (= 1 (gurd-factorial 1) (gurd-factorial 0)))
  (should (= (gurd-factorial 33) 8683317618811886495518194401280000000)))

(provide 'test-gurd-utils)
