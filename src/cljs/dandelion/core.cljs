(ns dandelion.core
  (:require
   [reagent.core :as rc]
   [reagent.dom :as rd]
   [reitit.frontend :as rf]
   [reitit.frontend.easy :as rfe]
   [reitit.coercion.spec :as rcs]
   #_[malli.core :as mc]
   [malli.generator :as mg]
   #_[spec-tools.data-spec :as ds]
   #_ [fipp.edn :as fedn]))

;; (def log (.-log js/console))

(def input-mode
  (rc/atom :field))

(def user-schema (rc/atom {}))

(def malli-schema (rc/atom nil))

(def output-size (rc/atom 1))

(def keyword->predicate
  {:string 'string?
   :int 'int?
   :float 'float?
   :any 'any?
   :number 'number?
   :positive-number 'pos?
   :negative-number 'neg?
   :positive-integer 'pos-int?
   :negative-integer 'neg-int?
   :non-negative-integer 'nat-int?
   :double 'double?
   :boolean 'boolean?
   :uuid 'uuid?
   :uri 'uri?
   :datetime 'inst?
   :any-array 'seqable?
   :any-object 'map?
   :char 'char?
   :set 'set?
   :null 'nil?
   :false 'false?
   :true 'true?
   :zero 'zero?})

(defn extract-fields [entry]
  [(keyword (:name entry))
   (keyword->predicate (:type entry))])
#_
(= [:age 'int?]
   (extract-fields {:name "age" :type :int}))

(defn schema-user->malli [entries]
  (apply conj [:map] (map extract-fields (vals entries))))

#_
(= [:map [:nickname 'string?] [:age 'int?]]
   (schema-user->malli {1 {:name "nickname" :type :string}
                        2 {:name "age" :type :int}}))

(defn output-size-input []
  [:div
   [:label "output sample size: "]
   [:input {:class "input is-rounded"
            :type "text"
            :placeholder @output-size
            :on-change #(let [v (-> % .-target .-value)]
                          (reset! output-size v))}]])

(def generation-ready?
  (rc/atom false))

(defn check-schema-ready []
  (let [check-item (fn [item]
                     (and (-> item val :name nil? not)
                          (-> item val :type nil? not)))
        p (every? true? (map check-item @user-schema))]
    (reset! generation-ready? p)))

(defn type-selection [id]
  (let [dropped? (rc/atom false)
        showing (rc/atom "select")
        selected (rc/atom nil)
        action (fn [t]
                 (fn []
                   (check-schema-ready)
                   (reset! selected t)
                   (swap! dropped? not)
                   (reset! showing (name t))
                   (swap! user-schema (fn [us] (assoc-in us [id :type] t)))))]
    (fn []
      [:div {:class (if @dropped? "dropdown is-active" nil)}
       [:div {:class "dropdown-trigger"}
        [:button {:class "button"
                  :on-click #(swap! dropped? not)
                  :aria-controls "dropdown-menu"}
         [:span @showing]]]
       [:div {:class "dropdown-menu"
              :id "dropdown-menu"
              :role "menu"}
        (conj [:div {:class "dropdown-content"}]
              (for [k (keys keyword->predicate)]
                [:a {:class "dropdown-item"
                     :key k
                     :on-click (action k)} (name k)]))]])))

(defn field-name-input [id]
  [:input {:class "input"
           :type "text"
           :placeholder "Input"
           :key id
           :on-change #(let [v (-> % .-target .-value)]
                         (swap! user-schema (fn [us] (assoc-in us [id :name] v)))
                         (check-schema-ready))}])

(defn schema-table []
  [:table {:class "table"}
   [:thead
    [:tr
     [:th "Action"]
     [:th "Name"]
     [:th "Type"]]]
   [:tbody
    (for [item @user-schema]
      [:tr
       [:td {:key (str "action-" item)} (key item)]
       [:td {:key (str "name-" item)} [field-name-input (key item)]]
       [:td {:key (str "type-" item)} [type-selection (key item)]]])]])

(defn add-field-button []
  [:button
   {:class "button"
    :on-click #(swap! user-schema assoc (inc (count @user-schema)) {})}
   "Add field!"])

(defn generate-button []
  [:button
   {:class (if generation-ready?
             "button is-primary"
             "button is-static")
    :on-click #(do
                 (reset! malli-schema nil)
                 (reset! malli-schema
                         (mg/sample (schema-user->malli @user-schema)
                                    {:seed 42
                                     :size (js/parseInt @output-size)})))}
   "Generate!"])

(defn field-input []
  [:div
   [:div {:class "field"}
    [:div {:class "control"}]
    [schema-table]
    [add-field-button]
    [output-size-input]
    [generate-button]]])

(defn json-input []
  [:textarea {:class "textarea"
              :placeholder "e.g. Hello world"}])

(defn result-window []
  [:div {:class "column"}
   [:h1 "Result"]
   [:textarea {:class "textarea"
               :rows 25
               :read-only true
               :value (str               
                       (-> (into [] @malli-schema)
                           (clj->js)
                           (js/JSON.stringify nil 4)))}]])

(defn app-page []
  [:div {:class "section"}
   [:div {:class "container"}
    [:div {:class "columns"}
     [:div {:class "column"}
      (case @input-mode
        :field [field-input]
        :json [json-input])]
     [result-window]]]])

(defn about-page []
  [:div
   [:p "Object generator."]])

(defonce match (rc/atom nil))

(defn navbar []
  [:nav.navbar
   {:aria-label "main navigation", :role "navigation"}
   [:div#navbarBasicExample.navbar-menu
    [:div.navbar-start
     [:a.navbar-item {:href (rfe/href ::app)} "App"]
     [:a.navbar-item {:href (rfe/href ::about)} "About"]]]])

(defn current-page []
  [:div
   [navbar]
   (when @match
     (let [view (:view (:data @match))]
       [view @match]))])

(def routes
  [["/" {:name ::app
         :view app-page}]
   ["/about" {:name ::about
              :view about-page}]])

(defn run []
  (rfe/start!
   (rf/router routes
              {:data {:coercion rcs/coercion}})
   (fn [m] (reset! match m))
   {:use-fragment true})
  (rd/render [current-page]
             (js/document.getElementById "root")))

(defn ^:export init []
  (run)
  (print "initialized and loaded"))

(defn ^:export refresh []
  (run)
  (print "hot-reloaded"))
