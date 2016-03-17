(defmodule clj-p
  (doc "Clojure-inspired predicates.")
  (export all))

(defun string? (data)
  "Return `true` iff `data` is a flat list of printable characters."
  (io_lib:printable_list data))

(defun unicode? (data)
  "Return `true` iff `data` is a flat list of printable Unicode characters."
  (io_lib:printable_unicode_list data))

(defun list? (data)
  "Return `true` iff `data` is a list."
  (andalso (is_list data) (not (string? data))))

(defun tuple? (data)
  "Return `true` iff `data` is a tuple."
  (is_tuple data))

(defun atom? (data)
  "Return `true` iff `data` is an atom."
  (is_atom data))

(defun binary? (data)
  "Return `true` iff `data` is a binary."
  (is_binary data))

(defun bitstring? (data)
  "Return `true` iff `data` is a bitstring."
  (is_bitstring data))

(defun bool? (data)
  "Return `true` iff `data` is a boolean."
  (is_boolean data))

(defun float? (data)
  "Return `true` iff `data` is a float."
  (is_float data))

(defun function? (data)
  "Return `true` iff `data` is a function."
  (is_function data))

(defun function? (data arity)
  "Return `true` iff `data` is a function with arity `arity`."
  (is_function data arity))

(defun func? (data)
  "Equivalent to [[function?/1]]."
  (is_function data))

(defun func? (data arity)
  "Equivalent to [[function?/2]]."
  (is_function data arity))

(defun integer? (data)
  "Return `true` iff `data` is an integer."
  (is_integer data))

(defun int? (data)
  "Equivalent to [[integer?/1]]."
  (is_integer data))

(defun number? (data)
  "Return `true` iff `data` is a number."
  (is_number data))

(defun record? (data record-tag)
  "Return `true` iff `data` is a tuple and its first element is `record-tag`."
  (is_record data record-tag))

(defun record? (data record-tag size)
  "Return `true` iff [[record?/2]] would hold and `data`'s size is `size`."
  (is_record data record-tag size))

(defun reference? (data)
  "Return `true` iff `data` is a reference."
  (is_reference data))

(defun map? (data)
  "Return `true` iff `is_map` is an Erlang BIF and `data` is a map."
  (if (erl_internal:bif 'is_map 1)
    (call 'erlang 'is_map data)
    'false))

(defun set? (x)
  "Return `true` iff `x` is a set or ordset."
  (or (sets:is_set x)
      (ordsets:is_set x)))

(defun dict?
  "Return `true` iff `data` is a dict."
  ((data) (when (=:= 'dict (element 1 data)))
   'true)
  ((_)
   'false))

(defun proplist?
  "Return `true` iff `data` is a proplist, i.e. [[proplist-kv?/1]] holds for
all elements in `data`.."
  ((data) (when (is_list data))
   (if (lists:all #'proplist-kv?/1 data)
     'true
     'false))
  ((_)
   'false))

(defun proplist-kv?
  "Return `true` if given a 2-tuple whose first element (key) is an atom
or a single atom."
  ((`#(,key ,_)) (when (is_atom key))
   'true)
  ((bool-key) (when (is_atom bool-key))
   'true)
  ((_)
   'false))

(defun undefined? (x)
  "Return `true` iff `x` is `undefined`."
  (=:= x 'undefined))

(defun undef? (x)
  "Equivalent to [[undef?/1]]."
  (=:= x 'undefined))

(defun nil?
  "Return `true` iff given `nil` or the empty list."
  (['nil] 'true)
  ([()]   'true)
  ([_]    'false))

(defun true? (x)
  "Return `true` iff `x` is `true`."
  (=:= x 'true))

(defun false? (x)
  "Return `true` iff `x` is `false`."
  (=:= x 'false))

(defun some?
  "Return `true` iff a given argument is not `false` or `undefined`."
  (['false]     'false)
  (['undefined] 'false)
  ([_]          'true))

(defun odd? (x)
  "Return `true` iff `x` is odd."
  (=:= 1 (rem x 2)))

(defun even? (x)
  "Return `true` iff `x` is even."
  (=:= 0 (rem x 2)))

(defun zero? (x)
  "Return `true` iff `x` is zero."
  (=:= 0 x))

(defun pos? (x)
  "Return `true` iff `x` is greater than zero."
  (> x 0))

(defun neg? (x)
  "Return `true` iff `x` is less than zero."
  (< x 0))

(defun identical? (x y)
  "Test if two arguments are the exactly equal."
  (=:= x y))

(defun empty? (x)
  "Return `true` iff `x` is the empty list."
  (=:= x '()))

(defun every? (pred x)
  "Equivalent to [[all?/2]]."
  (lists:all pred x))

(defun all? (pred x)
  "Equivalent to `lists:all/2`."
  (lists:all pred x))

(defun any? (pred x)
  "Equivalent to `lists:any/2`."
  (lists:any pred x))

(defun not-any? (pred x)
  "Return `false` iff `(pred x)` is `true` for any element in `x`."
  (not (lists:any pred x)))

(defun some
  "Return the first value of `(pred x)` for any `x` in a given list
where [[some?/1]] holds, else `undefined`."
  ([pred  ()] 'undefined)
  ([pred `(,h . ,t)]
   (let ((x (funcall pred h)))
     (if (some? x) x (some pred t)))))

(defun element?
  "Return `true` iff `element` is an element of `x`,
where `x` is a list, set or ordset."
  ((element x) (when (is_list x))
   (any? (lambda (y) (identical? element y)) x))
  ((element x)
   (cond
    ((sets:is_set x)
     (sets:is_element element x))
    ((ordsets:is_set x)
     (ordsets:is_element element x))
    ('true 'false))))
