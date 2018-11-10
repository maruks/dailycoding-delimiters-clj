(ns delimiters.core-test
  (:require [clojure.test :refer :all]
            [delimiters.core :refer :all]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :as prop]))

(deftest reverse-words-test
  (testing "some examples"
    (is (= "123//---abc///d-" (reverse-words "--" "abc///d---123//-")))
    (let [ds "/:"]
      (is (= "here/world:hello" (reverse-words ds "hello/world:here")))
      (is (= "here/world:hello/" (reverse-words ds "hello/world:here/")))
      (is (= "here//world:hello" (reverse-words ds "hello//world:here" ))))
    (let [ds "/:-"]
      (is (= ":-:123/fgh///e/-/d-/-abc" (reverse-words ds ":-:abc/d///e/-/fgh-/-123")))
      (is (= "123///d---abc//-"  (reverse-words ds "abc///d---123//-"))))))

(def string-and-elems (g/bind (g/not-empty g/string-alphanumeric)
                                #(g/tuple (g/return %) (g/vector (g/elements %) 1 (inc (quot (count %) 5))))))

(defn words [s ds]
  (filter (comp pos? count) (str/split s (re-pattern (format "(%s)+" (apply str (interpose "|" (map #(format "\\Q%s\\E" %) ds))))))))

(defspec result-has-all-words-in-reversed-order
  1000
  (prop/for-all [[string seperators] string-and-elems]
                (let [r (reverse-words (apply str seperators) string)]
                  (= (words r seperators) (reverse (words string seperators))))))
