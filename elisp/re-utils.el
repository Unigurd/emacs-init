;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'iter-utils)
(require 'object-utils)

(cl-defmacro with-match-data (match-data &body body)
  "Sets and restore MATCH-DATA for the duration of BODY.
Probably not correct etiquette. I suspect the normal thing is just
to set MATCH-DATA and let other people worry about their MATCH-DATA too."
  (declare (indent 1))
  `(save-match-data
     (set-match-data ,match-data)
     ,@body))

(defun re-call (fun match-data &rest args)
  "Call FUN on ARGS with MATCH-DATA set as global match-data.
For the built-in regular expression functions that expect global match-data."
  (with-match-data match-data
    (apply fun args)))

(iter-defun gurd-re-matches (regex &optional buffer-or-name string)
  "Generator for finding all matches of a regex in a buffer or string.
Moves point in BUFFER-OR-NAME. Modifies MATCH-DATA.
Will loop infinitely if the empty string is matched.
If the regex might match an empty string you can fix it by calling
(iter-take-while (lambda (string) (not (string= \"\" string))) (gurd-re-matches ...))"
  (cl-assert (not (and buffer-or-name string)) ;; TODO: meaningful error message
             )
  (if string
      (let ((pos 0))
        (while (string-match regex string pos)
          (setf pos (match-end 0))
          (iter-yield (match-data))))
    (let* ((buffer (or buffer-or-name (current-buffer)))
           (pos (with-current-buffer buffer (point-min))))
      (while (with-current-buffer buffer
               (goto-char pos)
               (prog1 (re-search-forward regex nil t)
                 (setf pos (point))))
        (iter-yield (match-data))))))

(iter-defun gurd-re-string-matches (regex match-numbers &optional buffer-or-name string)
  (iter-yield-from
   (iter-map (lambda (_) ; match-data is already set from gurd-re-matches
               (with-current-buffer (or buffer-or-name (current-buffer))
                 (if (listp match-numbers)
                     (mapcar (lambda (n) (match-string n string))
                             match-numbers)
                   (match-string-no-properties match-numbers string))))
             (gurd-re-matches regex buffer-or-name string))))


(cl-defmacro regex-do (regex &body body)
  (let ((match-data-var (gensym "match-data")))
    `(iter-do (,match-data-var (gurd-re-matches ,regex))
       (set-match-data ,match-data-var)
       ,@body)))

(cl-defgeneric re-match-string (match n))

(defclass re-buffer-match (gurd-req-param)
  ((buffer :initarg :buffer :type buffer)
   (match-data :initarg :match-data :type list)))

(cl-defmethod re-match-string ((match re-buffer-match) n)
  ;; TODO: decide on a more controlled error to throw
  ;; if the buffer has been deleted
  (with-current-buffer (slot-value match 'buffer)
    (with-match-data (slot-value match 'match-data)
      (match-string-no-properties n))))

(defclass re-string-match (gurd-req-param)
  ((string :initarg :string :type string)
   (match-data :initarg :match-data :type list)))

(cl-defmethod re-match-string ((match re-string-match) n)
  (with-match-data (slot-value match 'match-data)
    (match-string-no-properties n (slot-value match 'string))))

;; (re-match-string (re-string-match :string "abekat" :match-data (progn (string-match "beka" "abekat")
;;                                                                       (match-data)))
;;                  0)


;; 0 1 2 3 4 5 6
;; |a|b|e|k|a|t|

;; "" 0 0
;; "a" 0 1

(defun re-search-buffer-1 (regex buffer &optional start end)
  ;; We don't care about setting start to (point-min) rather
  ;; than 0, cause (goto-char 0) will just go to the actual
  ;; (point-min) regardless
  (setf start (if start (max (1+ start) 1) 1))
  (when (or (not end) (<= start end))
    (save-excursion
      (with-current-buffer buffer
        (let ((end (if end (1+ end) (point-max))))
          (when (<= (point-min) start end (point-max))
            (goto-char start)
            (save-match-data
              (when (re-search-forward regex end t)
                (re-buffer-match :buffer buffer :match-data (match-data))))))))))

(cl-defun re-search-1 (regex &key buffer string start end)
  "Think of the positions START and END as in-between characters, not
  at the characters. Like this:
  0 1 2 3 4 5 6
  |a|b|e|k|a|t|
  The bars indicate the in-betweens and the numbers above them show
  what START and END would be at each number. An implication is that if
  START = END the only valid match is the empty string.

START <= actual start of match <= actual end of match <= END.
"
  ;; when no match, should it return a match data structure that returns
  ;; nil for all re-match-string calls?
  (cl-assert (xor buffer string))
  (setf start (if start (max start 0) 0))
  ;; No need to search if end < start
  (when (or (not end) (<= start end))
    (if string
        (save-match-data
          (let ((end (if end
                         (min end (length string))
                       (length string))))
            (when (<= start end)
              ;; We create a substring to make sure we don't match after
              ;; END. Just checking that the end of a match is not after
              ;; END isn't enough as even if it is there might be other
              ;; matches that end before that we haven't found.
              (let ((substring (substring string start (or end (length string)))))
                ;; No need to start searching after the end of the string
                (when (string-match regex substring)
                  (re-string-match :string substring :match-data (match-data)))))))
      (save-match-data
        (save-excursion
          ;; Just switch to buffer instead of using with-
          (with-current-buffer buffer
            (let ((start start ;; (1+ start)
                         )
                  (end (if end (+ end 1) (point-max))))
              (when (and (<= start (point-max))
                         (<= (point-min) end))
                (goto-char start)
                (when (re-search-forward regex end t)
                  (re-buffer-match :buffer buffer :match-data (match-data)))))))))))

;; (with-temp-buffer
;;  (insert "abekat")
;;  (re-search-1 "kat" :buffer (current-buffer) :end 7))

;; (re-search-1 "kat" :string "abekat" :end 6)

;; (cl-defgeneric re-search (regob start end))


(provide 're-utils)

