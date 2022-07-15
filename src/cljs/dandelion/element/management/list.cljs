(ns freesia.element.management.list
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   #_[clojure.spec.alpha :as s]
   #_[bitem.pias.common.shape :as h]
   [cljs-http.client :as http]
   [freesia.state :as state]
   [cljs.core.async :refer [<!]]
   [freesia.element.management.edit :as e]
   ))

(defonce data (atom []))

(defn fetch []
  (let [url  "http://localhost:3548/api/management"]
    (go (let [response (<! (http/get url))
              managements (-> response :body)]
          (reset! data managements)))))

(defn item [m]
  [:tr
   [:td {:class "text-center"} (-> m :doc :user-hash)]
   [:td {:class "text-center"} (-> m :doc :name)]
   [:td {:class "text-center"} (-> m :doc :num-persons)]
   [:td {:class "text-center"}
    [:button {:class "btn"
              :on-click
              (fn []
                (swap! state/control assoc :management (-> m :doc :name)))} "Load"]]
   [:td {:class "text-center"}
    [:button {:class "btn"
              :on-click
              (fn []
                (reset! e/entity m)
                (swap! state/control assoc :element [:management :edit]))} "Edit"]]])

(defn table []
  (fetch)
  [:div {:class "card"}
   [:h1 {:class "card-title text-center"}
    "List of Managements Subscribed"]
   [:table {:class "table"}
    [:thead
     [:tr
      [:th {:class "text-center"} "Hash"]
      [:th {:class "text-center"} "Name"]
      [:th {:class "text-center"} "Users"]
      [:th {:class "text-center"} "Mode"]
      [:th {:class "text-center"} "Action"]]]
    [:tbody
     (map item @data)]]])
