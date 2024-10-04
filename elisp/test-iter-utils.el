; -*- lexical-binding: t -*-

(require 'iter-utils)


(ert-deftest iter-yield-from2 ()
  (should (let ((iter (funcall (iter-lambda ()
                                 (let ((iter (iter-yield-prev)))
                                   (iter-next iter)
                                   (iter-yield-from2 iter t))))))
            (list (iter->list (iter-take 2 iter))
                  '(t nil))))
  (should (equal (iter->list
                  (iter-supply
                   (iter-take 10 (funcall (iter-lambda2 (x) (y)
                                            (iter-yield-from2 (iter-yield-prev x) y))
                                          'ta))
                   (iter-count 0 20)))
                 '(ta 0 1 2 3 4 5 6 7 8))))


(ert-deftest iter-prefix ()
  (should (equal (iter->list (iter-prefix '+ 0 (iter 0 1 2 3 4 5 6 7)))
                 '(0 1 3 6 10 15 21 28))))

(ert-deftest iter ()
  (should (let ((args '(1 2 3 4)))
            (equal args (iter->list (apply 'iter args)))))
  (should (equal '(1) (iter->list (iter 1))))
  (should (not (iter->list (iter)))))

(ert-deftest iter-merge ()
  (should (equal (iter->list (iter-merge nil (iter 1 2 3)
                                         (iter 0 4 5 5)
                                         (iter)
                                         (iter -3 3 4)))
                 '(-3 0 1 2 3 3 4 4 5 5)))
  (should (not (iter->list (iter-merge nil (iter))))))

(ert-deftest iter-unique ()
  (should (equal (iter->list (iter-unique (iter 1 2 3 4 4 5 6 6 6 6 7 8 8 9 9)))
                 '(1 2 3 4 5 6 7 8 9))))

(ert-deftest iter-filter ()
  (should (equal (iter->list (iter-filter (partial #'< 3)
                                          (list->iter '(0 1 2 3 4 5))))
                 '(4 5))))

(ert-deftest iter-list ()
  (should (let ((l (iter-lambda (a)
                     (while (>= a 0)
                       (iter-yield a)
                       (cl-decf a)))))
            (iter->list (funcall l 10))))
  (should (equal '(1 2 3 4) (iter->list (list->iter '(1 2 3 4)))))
  (should (let ((l (iter-lambda (a)
                     (while (>= a 0)
                       (iter-yield a)
                       (cl-decf a)))))
            (equal '(10 9 8 7 6 5 4 3 2 1 0)
                   (iter->list (list->iter (iter->list (funcall l 10))))))))


(iter-defun iter-test-nats ()
  (iter-yield 1)
  (iter-yield-from (iter-map '1+ (iter-test-nats))))

(ert-deftest iter-map ()
  (should (equal (iter->list (iter-map #'- (iter 0 1 2 3 4)))
                 '(0 -1 -2 -3 -4)))

  (should (equal '(1 2 3 4 5 6 7 8 9 10)
                 (iter->list (iter-take 10 (iter-test-nats))))))

(ert-deftest iter-append ()
  (should (equal (iter->list (iter-append (list->iter '(1 2 3 4))
                                          (list->iter '())
                                          (list->iter '(5 6 7 8))
                                          (list->iter '(9 10 11 12))))
                 '(1 2 3 4 5 6 7 8 9 10 11 12))))

(ert-deftest iter-take ()
  (should (equal (iter->list (iter-take 3 (list->iter '(1 2 3 4 5 6 7 8 9 0))))
                 '(1 2 3)))
  ;; iter-take passes arguments to iter-next to inner iter
  (should (let ((xs '(1 2 3 4 5 6))
                (iter (iter-take 10 (iter-yield-prev))))
            (equal (cons nil (cdr xs))
                   (cl-loop for x in xs
                            collect (iter-next iter x)))))
  (should (equal (iter->list (iter-supply (iter-take 5 (iter-yield-prev -1)) (iter-count 0 10)))
                 '(-1 0 1 2 3))))

;; LOOG AT THIS
(ert-deftest iter-drop ())
(iter->list (iter-supply (iter-drop 10 (iter-yield-prev))))




(ert-deftest iter-loop ()
  ;; Handle finite non-empty iterators
  (should (equal '(1 1 1 1 1) (iter->list (iter-take 5 (iter-loop (iter 1))))))
  ;; Handle empty iterators without infinite loop
  (should (not (iter->list (iter-loop (empty-iter)))))
  ;; Handle infinite iterators without infinite loop
  (should (equal '(1 1 1 1 1) (iter->list (iter-take 5 (iter-loop (iter-loop (iter 1))))))))

(ert-deftest iter-retval ()
  (= -1 (iter-retval (funcall (iter-lambda ()
                                (iter-yield-from (iter-take 100 (iter-count)))
                                -1)))))

(ert-deftest empty-iter ()
  (not ()))

(ert-deftest iter-cons ()
  (should (equal '(1 2 3)
                 (iter->list (iter-cons 1 (iter-cons 2 (iter-cons 3 (empty-iter))))))))

(ert-deftest iter-snoc ()
  (should (equal '(1 2 3)
                 (iter->list (iter-snoc (iter-snoc (iter-snoc (empty-iter) 1) 2) 3)))))

(ert-deftest iter-rest ()
  (should (equal '(2 3 4) (iter->list (iter-rest (list->iter '(1 2 3 4))))))
  (should (not (iter->list (iter-rest (empty-iter))))))

(ert-deftest iter-zip ()
  (should (equal (iter->list (iter-zip (list->iter '(1 2 3 4 5 6))
                                       (iter-loop (iter 0))
                                       (iter-cons 'a (iter-snoc (iter-cons 'b (empty-iter)) 'c))))
                 '((1 0 a) (2 0 b) (3 0 c)))))

(ert-deftest iter-zip-seq ()
  (should (equal (iter->list (iter-zip-seq (vector (iter-count 0 4)
                                                   (iter-count 0 9)
                                                   (iter-count 0 5))))
                 (list (vector 0 0 0)
                       (vector 1 1 1)
                       (vector 2 2 2)
                       (vector 3 3 3)))))

(ert-deftest iter-next-default ()
  (should (= 9 (iter-next-default (empty-iter)  9)))
  (should (= 8 (iter-next-default (iter 8) 9))))

(ert-deftest iter-supply ()
  (should (equal (iter->list (iter-take 10 (iter-supply (iter-yield-prev) (list->iter '(1 2 3 4)))))
                 '(nil 1 2 3 4 nil nil nil nil nil)))

  (should (equal (iter->list (iter-supply (iter-take 3 (iter-yield-prev)) (list->iter '(1 2 3 4 5))))
                 '(nil 1 2))))

(should (equal (iter->list (iter-take 4 (iter-supply (iter-yield-prev) (list->iter '(1 2 3 4)))))
               '(nil 1 2 3 4 nil nil nil nil nil)))

(ert-deftest iter-count ()
  (should (equal (iter->list (iter-count -1 5))
                 '(-1 0 1 2 3 4)))
  (should (equal (iter->list (iter-count nil 5))
                 '(0 1 2 3 4)))
  (should (= 100 (length (iter->list (iter-take 100 (iter-count 9)))))))

(ert-deftest iter-yield-prev ()
  (should (equal (iter->list (iter-take 10 (iter-supply (iter-yield-prev) (iter-count 0 100))))
                 '(nil 0 1 2 3 4 5 6 7 8)))
  (should (equal (iter->list (iter-take 10 (iter-supply (iter-yield-prev -1) (iter-count 0 100))))
                 '(-1 0 1 2 3 4 5 6 7 8))))

(ert-deftest iter-propagate ()
  (should (equal (let ((iter (iter-yield-prev)))
                   (iter-next iter 0)
                   (iter->list (iter-take 9 (iter-propagate iter 1 '+))))
                 '(1 2 4 8 16 32 64 128 256))))

(ert-deftest iter-lambda2 ()
  (should (equal (let ((iter (funcall (iter-lambda2 (a b) (c)
                                        (iter-yield c)
                                        (iter-yield a)
                                        (iter-yield b))
                                      1 2)))
                   (cons (iter-next iter 3)
                         (iter->list iter)))
                 '(3 1 2))))

(ert-deftest iter-defun2 ()
  (should (progn (iter-defun2 test-of-iter-defun2 (a b) (c)
                   (iter-yield c)
                   (iter-yield a)
                   (iter-yield b))

                 (equal (let ((iter (test-of-iter-defun2 1 2)))
                          (cons (iter-next iter 3)
                                (iter->list iter)))
                        '(3 1 2)))))

(provide 'test-iter-utils)
