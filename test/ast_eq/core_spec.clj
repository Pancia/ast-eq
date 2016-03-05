(ns ast-eq.core-spec
  (:require [untangled-spec.core :refer [specification behavior assertions]]
            [ast-eq.core :refer [ast=]]))

(defn *contains? [exp]
  (fn [act] (clojure.set/subset? (set exp) (set act))))

(defn check-env [state & [exp-env rejected-msg]]
  (*contains? (merge {:state state}
                     (cond-> {}
                       exp-env (merge {:value {:env exp-env}})
                       rejected-msg (merge {:rejected/msg rejected-msg})))))

(specification "main features"
  (assertions "checks equality for literals"
    (ast= '(5 2)
          '(5 2))
    =fn=> (check-env :accepted {}))
  (assertions "meta-vars capture the value of its pair in the env"
    (ast= '(?x 5)
          '(2  5))
    =fn=> (check-env :accepted {'?x 2})
    "and will eval to that value"
    (ast= '(?x 3 ?x)
          '(2  3 2))
    =fn=> (check-env :accepted {'?x 2})
    (ast= '(?x 3 ?x)
          '(2  3 2))
    =fn=> (check-env :accepted {'?x 2})
    "meta-vars will throw an error if the paired value is incorrect"
    (ast= '(?x ?x)
          '(2  3))
    =fn=> (check-env :rejected {'?x 2} [['?x 2] 3]))
  (assertions "_'s will pass equality checks, ie: acts as a positional ignore"
    (ast= '(2 _)
          '(2 \&))
    =fn=> (check-env :accepted {}))
  (assertions "only checks as long as there is input in the exp tape"
    (ast= '(0 1 2)
          '(0 1 2 3))
    =fn=> (check-env :accepted)
    "to verify that there is nothing more, use $"
    (ast= '(0 1 2 $)
          '(0 1 2))
    =fn=> (check-env :accepted)
    (ast= '(0 1 2 $)
          '(0 1 2 3))
    =fn=> (check-env :rejected {} [:end-of-input 3]))
  (assertions "ast= works with nested seqs"
    (ast= '(some-> ?a (+ ?a) (/ ?a))
          '(some-> 2  (+ 2)  (/ 2)))
    =fn=> (check-env :accepted {'?a 2})
    (ast= '(some-> ?b (+ ?b) (/ ?b))
          '(some-> 2  (+ 3)  (/ 2)))
    =fn=> (check-env :rejected {'?b 2})
    (ast= '(some-> ?c (+ ?c) (/ ?c))
          '(some-> 2  (+ 2)  (/ 3)))
    =fn=> (check-env :rejected {'?c 2}))
  (behavior "checks coll types match"
    (assertions
      (ast= '(0 (1) [2])
            '(0 (1) [2]))
      =fn=> (check-env :accepted)
      (ast= '(0 [1])
            '(0 (1)))
      =fn=> (check-env :rejected {} :coll/mismatch)
      "but ignores the outer type"
      (ast= '(0 1 2 3)
            [0 1 2 3])
      =fn=> (check-env :accepted)
      "but will keep trying in ellipses state"
      (ast= '(0 ... [2])
            '(0 (1) [2]))
      =fn=> (check-env :accepted)
      (ast= '(0 ... [3])
            '(0 1 2 (3)))
      =fn=> (check-env :rejected)
      "empty lists are equal (dont break recursive descent)"
      (ast= '(0 ())
            '(0 ()))
      =fn=> (check-env :accepted)
      (ast= '(0 ())
            '(0 []))
      =fn=> (check-env :rejected {} ['() []])))
  (behavior "ellipses will skip input"
    (assertions
      "until a match is found"
      (ast= '(0   ...   5)
            '(0 1 2 3 4 5))
      =fn=> (check-env :accepted)
      (ast= '(0 ... 5)
            '(0     5))
      =fn=> (check-env :accepted)
      "or we reach the end"
      (ast= '(0 ... 5)
            '(0     1))
      =fn=> (check-env :rejected)
      "unterminated ellipsis are invalid"
      (ast= '(0 ...)
            '(0 1 2))
      =fn=> (check-env :rejected {} :ellipsis/unterminated)
      "ellipses at the beginning are ok"
      (ast= '(... 0)
            '(0 1 2))
      =fn=> (check-env :accepted)
      "work with collections as expected"
      (ast= '(0 ... (3))
            '(0  1  (2 3)))
      =fn=> (check-env :rejected)
      "work with meta-vars"
      (ast= '(?l ... ?l)
            '(:l :m  :l))
      =fn=> (check-env :accepted {'?l :l})
      "even both at the same time!"
      (ast= '(?x ... (?x))
            '(3  1   (3)))
      =fn=> (check-env :accepted {'?x 3})
      (ast= '(?a ... (?a))
            '(:a :b  (:c)))
      =fn=> (check-env :rejected {'?a :a})
      (ast= '(?d ... (?d 2) 7)
            '(:d  :e (:d 3) 6))
      =fn=> (check-env :rejected {'?d :d} [2 3])
      (ast= '(?f ... (?f 2) 7)
            '(:f  :g (:f 2) 6))
      =fn=> (check-env :rejected {'?f :f} [7 6])
      "empty lists are equal dont break recursive descent"
      (ast= '(?h ... () 6)
            '(:h  :i () 6))
      =fn=> (check-env :accepted)
      (ast= '(?h ... (  ) 7)
            '(:h  :i (:h) 6))
      =fn=> (check-env :rejected {'?h :h} :expected/more)
      (ast= '(?j ... (:any) 7)
            '(:j  :k (    ) 6))
      =fn=> (check-env :rejected {'?j :j} :expected/more))))
