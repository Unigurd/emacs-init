;;; -*- lexical-binding: t -*-

(require 'gurd-macros)
(require 'ert)

(ert-deftest unless-recursive ()
  (cl-labels ((aux (n)
                (should (< n 1))
                (unless-buffer-recursive
                  (aux (1+ n)))))
    (aux 0)))

(ert-deftest unless-buffer-recursive ()
  (let ((buf (generate-new-buffer "test")))
    (cl-labels ((aux (n)
                  (should (< n 3))
                  (unless-buffer-recursive
                    (with-current-buffer buf
                      (aux (1+ n))))))
      (aux 0))))

(provide 'test-gurd-macros)

