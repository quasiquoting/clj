(defmacro in? (item collection)
  "Return `true` iff `item` is in `collection`."
  `(orelse ,@(lists:map
               (lambda (x)
                 `(=:= (quote ,x) ,item))
               `(,@(cadr collection)))))

(defmacro not-in? (item collection)
  "Return `true` iff `item` is *not* in `collection`."
  `(not (in? ,item ,collection)))

(defmacro generate-predicate-wrappers ()
  "This macro allows developers to use `(include-lib ...)` on this file and
pull in the functions from the passed module, making them available to
call as if they were part of the language."
  `(progn ,@(kla:wrap-mod-funcs 'clj-p)))

(generate-predicate-wrappers)

(defun loaded-predicates ()
  "This is just a dummy function for display purposes when including from the
REPL (the last function loaded has its name printed in stdout).

This function needs to be the last one in this include."
  'ok)
