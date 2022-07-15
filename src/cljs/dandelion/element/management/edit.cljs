(ns freesia.element.management.edit
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [clojure.spec.alpha :as s]
   [reagent.core :as r]
   [freesia.shape :as h]
   [freesia.util  :as u]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]))

(def entity (atom nil))

#_(defn fetch []
  (go (let [response (<! (http/get "http://localhost:3548/api/management"))
            managements (-> response :body)]
        (prn "%%%%" managements)
        (reset! mgmt-list managements))))

(defn existing [management]
  [:table
   [:thead
    [:tr
     [:th "entity-id"]
     [:th "user-hash"]
     [:th "name"]
     [:th "num-of-users"]]]
   [:tbody
    [:tr
     [:td (-> management :doc :entity-id)]
     [:td (-> management :doc :user-hash)]
     [:td (-> management :doc :name)]
     [:td (-> management :doc :num-persons)]]]])

(defn form []
  (let [_name (r/atom nil)
        _num-persons (r/atom nil)]
    (fn []
      [:div
       [:label {:for "name"} "name"]
       [:input {:type :text
                :on-change #(reset! _name (-> % .-target .-value))}]
       [:label {:for "name"} "num-persons"]
       [:input {:type "text"
                :on-change #(reset! _num-persons (-> % .-target .-value ))}]
       [:a {:type :text
            :on-click
            #(println (str @_name ";;;;;" @_num-persons))}
        "submit"]])))

(defn unit []
  (if @entity
    [:<>
     [:p "current state:"]
     (existing @entity)
     [form]]
    [:p "No management is selected for edit."]))

