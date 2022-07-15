(ns freesia.element.sign.up
  (:require
   [freesia.state :as state]))

(defn card []
  [:div {:class "w-400 mw-full"}
   [:div {:class "card p-1"}
    [:div {:class "px-card py-30"}
     [:h3 {:class "text-center"} "Bitem"]
     [:h5 {:class "text-center"}
      "Investment Accounting Suite"]]
    [:div {:class "content"}
     [:form
      [:div {:class "form-group input-group"}
       [:div {:class "input-group-prepend"}
        [:span {:class "input-group-text"}
         [:i {:class "fa fa-envelope-o",
              :aria-hidden "true"}]]]
       [:input {:type "email"
                :class "form-control"
                :placeholder "email"
                :required "required"}]]
      [:div {:class "form-group input-group"}
       [:div {:class "input-group-prepend"}
        [:span {:class "input-group-text"}
         [:i {:class "fa fa-envelope-o"
              :aria-hidden "true"}]]]
       [:input {:type "password"
                :class "form-control"
                :placeholder "password"
                :required "required"}]]
      [:input {:class "btn btn-primary btn-block"
               :value "Sign In"
               :on-click #(swap! state/control assoc :signed true)}]]]
    [:div {:class "px-card py-10 bg-light-lm bg-very-dark-dm rounded-bottom"}
     [:p {:class "font-size-12 m-0"}
      "We will notify you whenever we make a new post. No spam, no marketing, we promise."]]]])
