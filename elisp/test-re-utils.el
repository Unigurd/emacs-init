;; -*- lexical-binding: t -*-

(require 'test-utils)
(require 're-utils)
(require 'propel)


(ert-deftest gurd-re-string-matches ()
  (should (equal '("a" "b" "c" "d")
                 (iter->list (iter-take-while (lambda (str) (not (string= "" str)))
                                              (gurd-re-string-matches ".?" 0 nil "abcd")))))
  (should (equal '("a" "b" "c" "d" "" "")
                 (iter->list (iter-take 6 (gurd-re-string-matches ".?" 0 nil "abcd"))))))

(ert-deftest re-buffer-match ()
  (should (equal "me random"
                 (with-temp-buffer
                   (insert "some random text")
                   (re-match-string (re-buffer-match :buffer (current-buffer)
                                                     :match-data (progn
                                                                   (goto-char (point-min))
                                                                   (re-search-forward "me .*om")
                                                                   (match-data)))
                                    0))))
  '(should-error (equal "me random"
                        (re-match-string (with-temp-buffer
                                           (insert "some random text")
                                           (re-buffer-match :buffer (current-buffer)
                                                            :match-data (progn
                                                                          (goto-char (point-min))
                                                                          (re-search-forward "me .*om")
                                                                          (match-data))))
                                         0))))

(cl-defmacro with-temp-string-buffer (string &body body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,string)
     ,@body))

(defun re-search-1-test-helper (regex string start end)
  (let ((string-result
         (condition-case err
             (list 'ok (when-let ((match (re-search-1 regex
                                                      :string string
                                                      :start start
                                                      :end end)))
                         (re-match-string match 0)))
           (error (list 'fail err))))
        (buffer-result
         (condition-case err
             (list 'ok (with-temp-string-buffer string
                         (when-let ((match (re-search-1 regex
                                                        :buffer (current-buffer)
                                                        :start start
                                                        :end end)))
                           (re-match-string match 0))))
           (error (list 'fail err)))))
    (if (equal string-result buffer-result)
        string-result
      (list string-result buffer-result))))

(ert-deftest re-search-1 ()
  ;; Test varous indexing cases
  (should (equal '(ok nil) (re-search-1-test-helper "" "a" 2 3)))
  (should (equal '(ok "") (re-search-1-test-helper "a?" "a" 0 0)))
  (should (equal '(ok "a") (re-search-1-test-helper "a?" "a" 0 1)))
  (should (equal '(ok "") (re-search-1-test-helper "a?" "a" 1 1)))
  (should (equal '(ok nil) (re-search-1-test-helper "a" "a" 1 1)))
  (should (equal '(ok "aa") (re-search-1-test-helper "a*" "aaaaaaa" 0 2)))
  (should (equal '(ok "aa") (re-search-1-test-helper "aa" "aaaaaaa" 0 2)))
  (should (equal '(ok "a") (re-search-1-test-helper "a?" "a" nil nil)))
  (should (equal '(ok nil) (re-search-1-test-helper "a" "bbbbbba" 7 8)))

  ;; Test searching in a non-current buffer
  (should (with-temp-string-buffer "a"
            (let ((search-buffer (current-buffer)))
              (with-temp-string-buffer "b"
                (re-search-1 "a" :buffer search-buffer)))))

  ;; Test various cases
  (forall ((regex-char propel-ascii-char)
           (string (propel-string propel-ascii-char))
           (start propel-natural)
           (end propel-natural))
    (equal 'ok (car (re-search-1-test-helper (string regex-char) string start end))))

  ;; Test START < 0 is like if START = nil
  (forall ((regex-char propel-ascii-char)
           (string (propel-string propel-ascii-char))
           (n propel-natural))
    (let ((regex (string regex-char)))
      (equal (re-search-1-test-helper regex string (- n) nil)
             (re-search-1-test-helper regex string nil nil))))

  ;; Test END > (length string) is like if END = nil
  (forall ((regex-char propel-ascii-char)
           (string (propel-string propel-ascii-char))
           (n propel-natural))
    (let ((regex (string regex-char)))
      (equal (re-search-1-test-helper regex string nil (+ (length string) n))
             (re-search-1-test-helper regex string nil nil))))

  ;; TODO: test with buffer narrowing
  )


(counter-test ((regex-char propel-ascii-char)
               (string (propel-string propel-ascii-char))
               (start propel-natural)
               (end propel-natural))
              (equal 'ok (car (re-search-1-test-helper (string regex-char) string start end))))


(provide 'test-re-utils)
