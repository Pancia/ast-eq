(ns ast-eq.core
  (:require [machine.core :refer [build run]]))

(defn clean [result]
  (-> result
      (update :steps (comp set keys))
      (dissoc :on-empty)))

(defn ast=
  ([exp]
   (fn [act]
     (let [result (ast= exp act)]
       (or (-> result
               :state #{:normal})
           (clean result)))))
  ([exp act]
   (letfn [(meta-var? [x]
             (re-find #"^\?.+$" (str x)))
           (ignore-var? [x]
             (re-find #"^_" (str x)))
           (ellipsis? [x]
             (re-find #"^\.{3}$" (str x)))
           (end-of-input? [x]
             (= '$ x))
           (update-env [auto exp act]
             (.value auto #(update % :env assoc exp act)))
           (type= [x y] (= (type x) (type y)))
           (*= [x y] (and (= x y) (type= x y)))
           (normal-step [{:keys [value tapes]
                          :as auto}]
             (let [[exp act] (mapv first tapes)
                   {:keys [env]} value]
               (cond
                 (every? (every-pred coll? seq) [exp act])
                 (if (type= exp act)
                   (-> auto (run exp act)
                       (assoc :tapes tapes)
                       (.advance :all)
                       (.transition #(or (#{:rejected} %) :normal)))
                   (.reject auto :coll/mismatch))

                 (meta-var? exp)
                 (if-let [true-exp (get env exp)]
                   (if (= true-exp act)
                     (.advance auto :all)
                     (.reject auto [[exp true-exp] act]))
                   (-> auto (update-env exp act)
                       (.advance :all)))

                 (end-of-input? exp)
                 (if (not act)
                   auto
                   (.reject auto [:end-of-input act]))

                 (ignore-var? exp)
                 (.advance auto :all)

                 (ellipsis? exp)
                 (-> auto (.advance 0)
                     (.transition :ellipsis))

                 :else (if (*= act exp)
                         (.advance auto :all)
                         (.reject auto [exp act])))))
           (ellipsis-step [{:keys [value tapes]
                            :as auto}]
             (let [[exp act] (mapv first tapes)
                   {:keys [env]} value]
               (cond
                 (ellipsis? exp)   (.reject auto :ellipsis/not-allowed)
                 (ignore-var? exp) (.reject auto :ignore/not-allowed)

                 (and (meta-var? exp) (not (get env exp)))
                 (.reject auto :meta-var/unbound)

                 (every? (every-pred coll? seq) [act exp])
                 (cond
                   (*= (first act) (get env (first exp) (first exp)))
                   (.transition auto :normal)

                   :else
                   (.advance auto 1))

                 :else
                 (if (*= act (get env exp exp))
                   (-> auto (.advance :all)
                       (.transition :normal))
                   (.advance auto 1)))))]
     (-> {:env {}}
       (build {:normal   normal-step
               :ellipsis ellipsis-step
               :accept-states #{:normal}
               :on-empty (fn [auto exp act]
                           (if exp
                             (and (not= (first exp) '$)
                                  (.reject auto :expected/more))
                             (and (#{:ellipsis} (:state auto))
                                  (.reject auto :ellipsis/unterminated))))})
       (run exp act)
       (clean)))))
