(ns is-it-a-letter)

(defn letter? [s]
  (not= (re-matches #"[a-zA-Z]{1}" s) nil)
)