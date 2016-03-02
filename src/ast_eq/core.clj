(ns ast-eq.core
  (:require [machine.core :refer [build run]]))

(defn ast=
  "WIP"
  ([exp]
   (fn [act]
     (-> (ast= exp act)
         :state #{:accepted})))
  ([exp act]
   (letfn [(meta-var? [x]
             (re-find #"^\?.+$" (str x)))
           (ignore-var? [x]
             (re-find #"^_" (str x)))
           (ellipsis? [x]
             (re-find #"^\.{3}$" (str x)))
           (update-env [auto exp act]
             (.value auto #(update % :env assoc exp act)))
           (normal-step [{:keys [value tapes]
                          :as auto}]
             (let [[exp act] (mapv first tapes)
                   {:keys [env]} value]
               (cond
                 (and (coll? act) (coll? exp))
                 (if (->> [act exp]
                          (map type)
                          (apply =))
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

                 (ignore-var? exp)
                 (.advance auto :all)

                 (ellipsis? exp)
                 (-> auto (.advance 0)
                     (.transition :ellipsis))

                 :else (if (= act exp)
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

                 (and (coll? act) (coll? exp))
                 (cond
                   (= (first act)
                      (or (get env (first exp))
                          (first exp)))
                   (.transition auto :normal)

                   :else
                   (.reject auto :ellipsis/rejected))

                 :else
                 (if (= act (get env exp exp))
                   (-> auto (.advance :all)
                       (.transition :normal))
                   (.advance auto 1)))))]
     (-> {:env {}}
       (build {:normal   normal-step
               :ellipsis ellipsis-step
               :accept-states #{:normal}
               :on-empty (fn [auto exp act]
                           (when (not act) (.reject auto :expected/more)))})
       (run exp act)))))
