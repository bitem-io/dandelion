(ns dandelion.core
  (:require
   [clojure.string :as str]
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

(def first-names
  ["Liam" "Noah" "Oliver" "Elijah" "James" "William"
   "Benjamin" "Lucas" "Henry" "Theodore" "Jack" "Levi"
   "Alexander" "Jackson" "Mateo" "Daniel" "Michael" "Mason"
   "Sebastian" "Ethan" "Logan" "Owen" "Samuel" "Jacob"
   "Asher" "Aiden" "John" "Joseph" "Wyatt" "David"])

(def last-names
  ["Smith" "Johnson" "Williams" "Brown" "Jones" "Garcia"
   "Miller" "Davis" "Rodriguez" "Martinez" "Hernandez"
   "Lopez" "Gonzales"])

(def full-names
  (for [first-name first-names
        last-name last-names]
    (str first-name " " last-name)))

(def emails
  (for [first-name first-names
        last-name last-names]
    (str (str/lower-case first-name) "."
         (str/lower-case last-name) "@jsongenerator.com")))

(def relationships
  [:spouse :husband :wife :grandfather :grandmother
   :cousin :neice :aunt :uncle :son :daughter :relative
   :boss :employee :employer :colleague])

(def us-states
  [{:full "Alabama" :standard "Ala." :code "AL"}
   {:full "Alaska" :standard "Alaska" :code "AK"}
   {:full "Arizona" :standard "Ariz." :code "AZ"}
   {:full "Arkansas" :standard "Ark." :code "AR"}
   {:full "California" :standard "Calif." :code "CA"}
   {:full "Canal Zone" :standard "C.Z." :code "CZ"}
   {:full "Colorado" :standard "Colo." :code "CO"}
   {:full "Connecticut" :standard "Conn." :code "CT"}
   {:full "Delaware" :standard "Del." :code "DE"}
   {:full "District of Columbia" :standard "D.C." :code "DC"}
   {:full "Florida" :standard "Fla." :code "FL"}
   {:full "Georgia" :standard "Ga." :code "GA"}
   {:full "Guam" :standard "Guam" :code "GU"}
   {:full "Hawaii" :standard "Hawaii" :code "HI"}
   {:full "Idaho" :standard "Idaho" :code "ID"}
   {:full "Illinois" :standard "Ill." :code "IL"}
   {:full "Indiana" :standard "Ind." :code "IN"}
   {:full "Iowa" :standard "Iowa" :code "IA"}
   {:full "Kansas" :standard "Kan." :code "KS"}
   {:full "Kentucky" :standard "Ky." :code "KY"}
   {:full "Louisiana" :standard "La." :code "LA"}
   {:full "Maine" :standard "Maine" :code "ME"}
   {:full "Maryland" :standard "Md." :code "MD"}
   {:full "Massachusetts" :standard "Mass." :code "MA"}
   {:full "Michigan" :standard "Mich." :code "MI"}
   {:full "Minnesota" :standard "Minn." :code "MN"}
   {:full "Mississippi" :standard "Miss." :code "MS"}
   {:full "Missouri" :standard "Mo." :code "MO"}
   {:full "Montana" :standard "Mont." :code "MT"}
   {:full "Nebraska" :standard "Neb." :code "NE"}
   {:full "Nevada" :standard "Nev." :code "NV"}
   {:full "New Hampshire" :standard "N.H." :code "NH"}
   {:full "New Jersey" :standard "N.J." :code "NJ"}
   {:full "New Mexico" :standard "N.M." :code "NM"}
   {:full "New York" :standard "N.Y." :code "NY"}
   {:full "North Carolina" :standard "N.C." :code "NC"}
   {:full "North Dakota" :standard "N.D." :code "ND"}
   {:full "Ohio" :standard "Ohio" :code "OH"}
   {:full "Oklahoma" :standard "Okla." :code "OK"}
   {:full "Oregon" :standard "Ore." :code "OR"}
   {:full "Pennsylvania" :standard "Pa." :code "PA"}
   {:full "Puerto Rico" :standard "P.R." :code "PR"}
   {:full "Rhode Island" :standard "R.I." :code "RI"}
   {:full "South Carolina" :standard "S.C." :code "SC"}
   {:full "South Dakota" :standard "S.D." :code "SD"}
   {:full "Tennessee" :standard "Tenn." :code "TN"}
   {:full "Texas" :standard "Texas" :code "TX"}
   {:full "Utah" :standard "Utah" :code "UT"}
   {:full "Vermont" :standard "Vt." :code "VT"}
   {:full "Virgin Islands" :standard "V.I." :code "VI"}
   {:full "Virginia" :standard "Va." :code "VA"}
   {:full "Washington" :standard "Wash." :code "WA"}
   {:full "West Virginia" :standard "W.Va." :code "WV"}
   {:full "Wisconsin" :standard "Wis." :code "WI"}
   {:full "Wyoming" :standard "Wyo." :code "WY"}])

(defn gen-person-name [kind]
  (into [] (concat [:enum]
                   (case kind
                     :first first-names
                     :last last-names
                     :full full-names))))

(defn gen-email []
  (into [] (concat [:enum] emails)))

(defn gen-relationship []
  (into [] (concat [:enum] relationships)))

(defn gen-state [kind]
  (into [] (concat [:enum]
                   (map kind us-states))))

(def zip-codes
  (let [create (fn []
                 (->> (for [_ (range 5)]
                        (rand-int 10))
                      (into [])
                      (map #(-> % str))
                      (str/join)))]
    (into [] (for [_ (range 50)]
               (create)))))

(defn gen-zip-code []
  (into [] (concat [:enum] zip-codes)))

(def input-mode
  (rc/atom :field))

(def user-schema (rc/atom {}))

(def malli-schema (rc/atom nil))

(def output-size (rc/atom 1))

(def agenda
  {:person {:first-name (gen-person-name :first)
            :last-name (gen-person-name :last)
            :full-name (gen-person-name :full)
            :email (gen-email)
            :relaltionship (gen-relationship)
            :age [:and int? [:> 1] [:< 110]]}
   :address {:state-full (gen-state :full)
             :state-standard (gen-state :standard)
             :state-code (gen-state :code)
             :zip-code (gen-zip-code)}
   :math {:number 'number?
          :positive-integer [:and pos-int? [:> 1] [:< 10000]]
          :negative-integer [:and neg-int? [:< -1] [:> -10000]]
          :double 'double?
          :boolean 'boolean?}})

#_
(def keyword->predicate
  {:string 'string?
   :int 'int?
   :float 'float?
   :any 'any?

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

(defn ->s [& ks]
  (let [names (map name ks)]
    (str/join " " names)))

(defn extract-fields [entry]
  (let [{:keys [name category type]} entry
        kname (keyword name)
        spec (-> agenda category type)]
    (js/alert spec)
    [kname spec]))

(defn user-schema->malli [entries]
  (apply conj [:map]
         (map extract-fields
              (vals entries))))

(def generation-ready?
  (rc/atom false))

(defn check-schema-ready []
  (let [check-item (fn [item]
                     (and (-> item val :name nil? not)
                          (-> item val :type nil? not)))
        p (every? true? (map check-item @user-schema))]
    (reset! generation-ready? p)))

(defn category-selection [id]
  (let [dropped? (rc/atom false)
        showing (rc/atom "select")
        selected (rc/atom nil)
        act (fn [t]
                 (fn []
                   (check-schema-ready)
                   (reset! selected t)
                   (swap! dropped? not)
                   (reset! showing (name t))
                   (swap! user-schema
                          (fn [us] (assoc-in
                                    us
                                    [id :category]
                                    t)))))]
    (fn []
      [:div {:class (when @dropped?
                      (->s :dropdown :is-active))}
       [:div {:class "dropdown-trigger"}
        [:button {:class "button"
                  :on-click #(swap! dropped? not)
                  :aria-controls "dropdown-menu"}
         [:span @showing]]]
       [:div {:class "dropdown-menu"
              :id "dropdown-menu"
              :role "menu"}
        (conj [:div {:class "dropdown-content"}]
              (for [k (keys agenda)]
                [:a {:class "dropdown-item"
                     :key k
                     :on-click (act k)} (name k)]))]])))

(defn type-selection [id]
  (let [dropped? (rc/atom false)
        showing (rc/atom "select")
        selected (rc/atom nil)
        act (fn [t]
              (fn []
                (check-schema-ready)
                (reset! selected t)
                (swap! dropped? not)
                (reset! showing (name t))
                (swap! user-schema
                       (fn [us] (assoc-in
                                 us
                                 [id :type]
                                 t)))))]
    (fn []

      [:div {:class (when @dropped?
                      (->s :dropdown :is-active))}
       [:div {:class "dropdown-trigger"}
        [:button {:class "button"
                  :on-click #(swap! dropped? not)
                  :aria-controls "dropdown-menu"}
         [:span @showing]]]
       [:div {:class "dropdown-menu"
              :id "dropdown-menu"
              :role "menu"}
        (conj [:div {:class "dropdown-content"}]
              (for [k (->> id
                           (get @user-schema)
                           :category
                           agenda
                           keys)]
                [:a {:class "dropdown-item"
                     :key k
                     :on-click (act k)} (name k)]))]])))

(defn field-name-input [id]
  [:input {:class "input"
           :type "text"
           :placeholder "Input"
           :key id
           :on-change #(let [v (-> % .-target .-value)]
                         (swap! user-schema (fn [us] (assoc-in us [id :name] v)))
                         (check-schema-ready))}])

(defn add-field-button []
  [:button
   {:class (->s :button :is-link :is-light)
    :on-click #(swap! user-schema assoc (inc (count @user-schema)) {})}
   "Add field!"])

(defn schema-table []
  [:div {:class :box}
   [:table {:class (->s :table
                        :is-fullwidth
                        :is-bordered
                        :has-text-centered)}
    [:thead
     [:tr
      [:th "Level"]
      [:th "Name"]
      [:th "Category"]
      [:th "Type"]]]
    [:tbody
     (for [item @user-schema]
       [:tr
        [:td {:key (str "action-" item)} (key item)]
        [:td {:key (str "name-" item)} [field-name-input (key item)]]
        [:td {:key (str "category-" item)} [category-selection (key item)]]
        [:td {:key (str "type-" item)} [type-selection (key item)]]])]]
   [add-field-button]])

(defn generate-button []
  [:button
   {:class (->s :button :is-warning :is-light
                (if generation-ready?
                  :is-primary
                  :is-static))
    :on-click #(do
                 (reset! malli-schema nil)
                 (reset! malli-schema
                         (mg/sample (user-schema->malli @user-schema)
                                    {:seed 42
                                     :size (js/parseInt @output-size)})))}
   "Generate!"])

(defn output-size-input []
  [:div {:class :box}
   [:div {:class :field}
    [:label {:class :label}
     "Output size: "]
    [:input {:class :input
             :type :text
             :placeholder @output-size
             :on-change #(let [v (-> % .-target .-value)]
                           (reset! output-size v))}]]
   [generate-button]])

(defn field-input-content []
  [:div
   [:div {:class "container"}
    [:h2 {:class (-> :is-size-4)}
     "INPUT"]
    [schema-table]
    [output-size-input]]])

(defn json-input-content []
  [:textarea {:class "textarea"
              :placeholder "e.g. Hello world"}])

(defn output-column []
  [:div {:class (->s :column :is-half)}
   [:h2 {:class (-> :is-size-4)}
    "OUTPUT"]
   [:div {:class :box}
    [:textarea {:class "textarea"
                :rows 25
                :read-only true
                :value (str
                        (-> (into [] @malli-schema)
                            (clj->js)
                            (js/JSON.stringify nil 4)))}]]])

(defn input-column []
  [:div {:class (->s :column :is-half)}
   (case @input-mode
     :field [field-input-content]
     :json [json-input-content])])

(defn app-page []
  [:div {:class "section"}
   [:div {:class "container"}
    [:div {:class "columns"}
     [input-column]
     [output-column]]]])

(defn about-page []
  [:div
   [:p "Object generator."]])

(defonce match
  (rc/atom nil))

(defn navbar []
  [:nav {:class "navbar is-info"
         :role "navigation"
         :aria-label "main navigation"}
   [:div {:id "navbarBasicExample", :class "navbar-menu"}
    [:div {:class "navbar-start"}
     [:a {:class "navbar-item"}
      "Easy Object Generator"]
     [:a {:class "navbar-item"
          :href (rfe/href ::app)}
      "App"]
     [:a {:class "navbar-item"
          :href (rfe/href ::about)}
      "About"]]]])

(defn current-page []
  [:div
   [navbar]
   #_@user-schema ;; temp
   #_@malli-schema ;; temp
   (when @match
     (let [view (:view (:data @match))]
       (print @match)
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
