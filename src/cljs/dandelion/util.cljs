(ns dandelion.util
  (:require
   [clojure.string :as cstr]))

(defn tablize [m]
  (let [heads (map key m)
        tails (map val m)]
    [:table
     [:thead
      [:tr
       (map (fn [h] [:th h]) heads)]]]
    [:tbody
     [:tr
      (map (fn [t] [:td t]) tails)]]))

(defn class [& ks]
  (let [without-nils (filter #(not (nil? %)) ks)]
    {:class (cstr/join " " (map name without-nils))}))
