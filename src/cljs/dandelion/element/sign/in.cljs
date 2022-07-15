(ns freesia.element.sign.in
  (:require
   [freesia.state :as state]))

(def mock-person
  {:name "Olav Sugaredy"
   :email "olavsuraredy@frugaly.com"
   :abilities [:read :write :edit :delete]
   :entity-id "df449b09-177c-447d-ae59-1983c7a9019f"
   :management nil
   :managements ["a987c332-8f67-480d-a021-54a0768453ff"]})

(defn page []
  [:div {:class "page-wrapper with-navbar"}
   [:div {:class "content-wrapper"}
    [:div {:class "d-flex justify-content-center"}
     [:div {:class "card p-1 w-400"}
      [:div {:class "px-card py-30"}
       [:h3 {:class "text-center"} "Bitem / PIAS"]
       [:h5 {:class "text-center"}
        "Professional Investment Accounting Suite"]]
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
                 :read-only true
                 :on-click (fn []
                             (swap! state/control assoc :person mock-person)
                             (swap! state/control assoc :signed true)
                             (swap! state/control assoc :element [:management :list])
                             (print @state/control))}]]]
      [:div {:class "px-card py-10 bg-light-lm bg-very-dark-dm rounded-bottom"}
       [:p {:class "font-size-12 m-0"}
        "We will notify you whenever we make a new post. No spam, no marketing, we promise."]]]]]])
