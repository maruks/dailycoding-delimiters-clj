(ns delimiters.core)

(defn delimiters [is-delimeter? delims words temp-acc was-delimiter? [x & xs]]
  (if x
    (cond
      (= (is-delimeter? x) was-delimiter?) (recur is-delimeter? delims words (cons x temp-acc) (is-delimeter? x) xs)
      :else (if (is-delimeter? x)
              (recur is-delimeter? delims (cons (apply str (reverse temp-acc)) words) (list x) (is-delimeter? x) xs)
              (recur is-delimeter? (cons (apply str (reverse temp-acc)) delims) words (list x) (is-delimeter? x) xs)))
    (if (is-delimeter? (first temp-acc))
      [(reverse (cons (apply str (reverse temp-acc)) delims)) words]
      [(reverse delims) (cons (apply str (reverse temp-acc)) words)])))

(defn interleave-all [[x & xr :as xs] [y & yr :as ys] acc first?]
  (cond
    (and (nil? x) (nil? y)) (reverse acc)
    (and x y) (if first?
                (recur xr ys (cons x acc) (not first?))
                (recur xs yr (cons y acc) (not first?)))
    (some? x) (recur xr ys (cons x acc) first?)
    (some? y) (recur xs yr (cons y acc) first?)))

(defn reverse-words [delims xs]
  (let [is-delim?    (partial contains? (set delims))
        first-delim? (is-delim? (first xs))
        [ds ws]      (delimiters is-delim? '() '() '() first-delim? xs)]
    (apply str (interleave-all ds ws '() first-delim?))))
