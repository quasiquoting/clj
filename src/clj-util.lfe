(defmodule clj-util
  (doc "Utility functions for the `clj` library.")
  (export all))

(defun get-version ()
  "Return the version of `clj`."
  (lutil:get-app-version 'clj))

(defun get-versions ()
  "Return `#(clj (`[[get-version/0]]`)` appended to
the result of `lutil:get-versions/0`."
  (++ (lutil:get-versions)
      `(#(clj ,(get-version)))))
