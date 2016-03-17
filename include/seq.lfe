(defmacro get-in args
  "Return the value in a nested associative structure `data` (the last element
of `args`), where `keys` (all but the last element of `args`) is a sequence of
keys. Return `'undefined` if the key is not present."
  (let* ((data (lists:last args))
         (keys (lists:droplast args)))
    `(apply #'clj-seq:get-in/2 (list ,data (list ,@keys)))))

(defmacro generate-sequence-wrappers ()
  "This macro allows developers to use `(include-lib ...)` on this file and
pull in the functions from the passed module, making them available to
call as if they were part of the language."
  `(progn ,@(kla:wrap-mod-funcs 'clj-seq)))

(generate-sequence-wrappers)

(defun loaded-seq ()
  "This is just a dummy function for display purposes when including from the
REPL (the last function loaded has its name printed in stdout).

This function needs to be the last one in this include."
  'ok)
