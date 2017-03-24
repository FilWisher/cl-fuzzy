;; definition from https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp
(defun levenshtein-distance (str1 str2)
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int)."
  (let ((n (length str1))
	(m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
	  ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
	  (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
	(setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
	(setf (svref col 0) (1+ i))
	(dotimes (j m)
	  (setf (svref col (1+ j))
		(min (1+ (svref col j))
		     (1+ (svref prev-col (1+ j)))
		     (+ (svref prev-col j)
			(if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
	(rotatef col prev-col))
      (svref prev-col m))))

(defun get-distance (str1 str2)
    (levenshtein-distance str1 str2))

(defun get-distance-pairs (words s) 
    "Returns list of pairs of words and their distance from s"
    (map 'list 
	 #'(lambda (s*) 
	    (list s* (get-distance s s*)))
	 words))

(defun sort-cadr(pairs)
    "Sorts a list of pairs by their cdr"
    (sort (copy-list pairs) #'< :key #'cadr))

(defun sort-words-closest (words s)
    "Sort a list of words by their levenshtein distance from s"
    (let ((pairs (get-distance-pairs words s)))
	(map 'list #'car
	     (sort-cadr pairs))))

(defun top-n-closest(n words s)
    "returns the top n closest words (by levenshtein distance) to s"
    (subseq (sort-words-closest words s) 0 n))

(defun list-from-file (filename)
    "load lines from a file into a list"
    (with-open-file (stream filename)
	(loop for line = (read-line stream nil)
	     while line
	     collect line)))

(defparameter *full-dict* (list-from-file "./wordlist"))

(defun run-finder ()
    (format t "word: ")
    (let ((word (read)))
	(print (top-n-closest 5 *full-dict* word)))
    (run-finder))

;; (run-finder)
