# cl-fuzzy
fuzzy finder in common lisp

wordlist originally published [here](http://www-personal.umich.edu/~jlawler/wordlist)

```cl
(load "fuzzy.lisp")

(top-n-closest 10 *full-dict* "hello")
;; returns the top 10 closest words to "hello" in *full-dict*
```