(defmodule clj-comp
  (doc "Clojure-inspired function composition.")
  (export all))

(defun compose
  "Function composition.


#### Usage

```commonlisp
> (include-file \"clj/include/compose.lfe\")
compose
> (funcall (compose #'math:sin/1 #'math:asin/1)
           0.5)
0.49999999999999994
> (funcall (compose `(,#'math:sin/1
                      ,#'math:asin/1
                      ,(lambda (x) (+ x 1))))
           0.5)
1.5
```

Or used in another function call:

```commonlisp
> (lists:filter (compose #'not/1 #'zero?/1)
                '(0 1 0 2 0 3 0 4))
(1 2 3 4)
```

The usage above is best when [[compose/1]] will be called from in
functions like `(lists:foldl ...)` or `(lists:filter ...)`, etc.
However, one may also call [[compose/1]] in the following manner, best
suited for direct usage:

```commonlisp
> (compose #'math:sin/1 #'math:asin/1 0.5)
0.49999999999999994
> (compose `(,#'math:sin/1
             ,#'math:asin/1
             ,(lambda (x) (+ x 1))) 0.5)
1.5
```"
  ((func-1 func-2) (when (is_function func-2))
    (lambda (x)
      (funcall func-1
        (funcall func-2 x))))
  ((funcs x)
    (funcall (compose funcs) x)))

(defun compose (f g x)
  "Equivalent to `(funcall (compose f g) x)`."
  (funcall (compose f g) x))

(defun compose (funcs)
  "[[compose/1]] every function in a list, `funcs`."
  (lists:foldl #'compose/2 (lambda (x) x) funcs))

(defun partial
  "The [[partial/2]] function is arity 2 where the first parameter must be a
function and the second parameter may either be a single item or a list of
items.

When `funcall` is called against the result of the [[partial/2]] call, a second
parameter is applied to the partial function. This parameter too may be
either a single item or a list of items.


#### Usage

```commonlisp
> (set f (partial #'+/2 1))
#Fun<lfe_eval.28.86468545>
> (funcall f 2)
3
> (set f (partial #'+/3 1))
#Fun<lfe_eval.28.86468545>
> (funcall f '(2 3))
6
> (set f (partial #'+/3 '(2 3)))
#Fun<lfe_eval.28.86468545>
> (funcall f 4)
9
> (set f (partial #'+/4 '(2 3)))
#Fun<lfe_eval.28.86468545>
> (funcall f '(4 5))
14
```"
  ((func args-1) (when (is_list args-1))
   (match-lambda
     ((args-2) (when (is_list args-2))
      (apply func (++ args-1 args-2)))
     ((arg-2)
      (apply func (++ args-1 `(,arg-2))))))
  ((func arg-1)
   (match-lambda
     ((args-2) (when (is_list args-2))
      (apply func (++ `(,arg-1) args-2)))
     ((arg-2)
      (funcall func arg-1 arg-2)))))
