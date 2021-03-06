;; Word counter

;; Load a file as a string - thanks to https://lisp-lang.org/learn/io
(defun load-in-file (filename)
  (uiop:read-file-string filename))


;; Trying to build my own string splitter
(defun split-by-space (string)
  (do-split string))

(defun do-split (string &optional (remainder nil))
  (let ((pos (position #\Space string :from-end t)))
    (if pos
	(do-split (subseq string 0 pos) (cons (subseq string (+ 1 pos)) remainder))
	(cons string remainder))))



;; reduce though words list and count occurrence of input word
;; (reduce #'function-to-apply sequence :initial-value value)
(defun count-occurrences (str word)
  (reduce #'(lambda (acc w)
	      (if (equal w word)
		  (+ acc 1)
		  acc))
	  (split-by-space str) :initial-value 0))


;; Putting it all together
(defun word-occurrence-counter (filename word)
  (progn
    (let ((str (load-in-file filename)))
      (count-occurrences str word))))


;; TODO: Then try using streams in case the file size is huge
