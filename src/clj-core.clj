;;;; Various macros and functions inspired by clojure.core
;;;;
(defmodule clj-core
  (export (replace 2)))

(include-lib "clj/include/compose.lfe")

;; Similar to #'clojure.core/replace
;;
;; Usage:
;; > (-> (map "Erlang"    "LFE"
;;            "difficult" "great")
;;       (replace (string:tokens "Erlang syntax is difficult." " ."))
;;       (string:join " ")
;;       (++ "!"))
;; "LFE syntax is great!"
;;
(defun replace (smap lst)
  "Given a map of replacement pairs and a list/form, return a list/form with
  any elements = a key in `SMAP' replaced with the corresponding val in `SMAP'."
  (lists:map
    (lambda (x)
      (maps:get x smap x))
    lst))
