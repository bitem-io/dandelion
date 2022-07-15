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
   :pos-int 'pos-int?
   :neg-int 'neg-int?
   :nat-int 'nat-int?
   :pos 'pos?
   :neg 'neg?
   :double 'double?
   :boolean 'boolean?
   :ident 'ident?
   :simple-ident 'simple-ident?
   :qualified-ident 'qualified-ident?
   :keyword 'keyword?
   :simple-keyword 'simple-keyword?
   :qualified-keyword 'qualified-keyword?
   :symbol 'symbol?
   :simple-symbol 'simple-symbol?
   :qualified-symbol 'qualified-symbol?
   :uuid 'uuid?
   :uri 'uri?
   :decimal 'decimal?
   :inst 'inst?
   :seqable 'seqable?
   :indexed 'indexed?
   :map 'map?
   :vector 'vector?
   :list 'list?
   :seq 'seq?
   :char 'char?
   :set 'set?
   :nil 'nil?
   :false 'false?
   :true 'true?
   :zero 'zero?
   :rational 'rational?
   :coll 'coll?
   :empty 'empty?
   :associative 'associative?
   :sequential 'sequential?
   :ratio 'ratio?
   :bytes 'bytes?
   :ifn 'ifn?
   :fn 'fn?})

(defn extract-fields [entry]
  [(keyword (:name entry))
   (keyword->predicate (:type entry))])

(= [:age 'int?]
   (extract-fields {:name "age" :type :int}))

(defn schema-user->malli [entries]
  (apply conj [:map] (map extract-fields (vals entries))))

(= [:map [:nickname 'string?] [:age 'int?]]
   (schema-user->malli {1 {:name "nickname" :type :string}
                        2 {:name "age" :type :int}}))

(defn output-size-input []
  [:div
   [:label "output sample size: "]
   [:input {:class "input"
           :type "text"
           :placeholder @output-size
           :on-change #(let [v (-> % .-target .-value)]
                         (reset! output-size v))}]])

(defn type-selection [id]
  (let [dropped? (rc/atom false)
        showing (rc/atom "select")
        selected (rc/atom nil)
        action (fn [t]
                 (fn []
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
                [:a {:class "dropdown-item" :on-click (action k)} (name k)]))]])))

(defn field-name-input [id]
  [:input {:class "input"
           :type "text"
           :placeholder "Input"
           :on-change #(let [v (-> % .-target .-value)]
                         (swap! user-schema (fn [us] (assoc-in us [id :name] v))))}])

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
       [:th (key item)]
       [:td [field-name-input (key item)]]
       [:td [type-selection (key item)]]])]])

(defn add-field-button []
  [:button
   {:on-click #(swap! user-schema assoc (inc (count @user-schema)) {})}
   "Add field!"])

(defn generate-button []
  [:button
   {:on-click #(do
                 (reset! malli-schema nil)
                 (reset! malli-schema
                         (mg/sample (schema-user->malli @user-schema)
                                    {:seed 129, :size (js/parseInt @output-size)})))}
   "Generate!"])

(defn field-input []
  [:div
   [:div {:class "field"}
    [:div {:class "control"}
     [:input {:class "input"
              :type "text"
              :placeholder "Text input"}]]
    [schema-table]
    [add-field-button]
    [output-size-input]
    [generate-button]]])

(defn json-input []
  [:textarea {:class "textarea"
              :placeholder "e.g. Hello world"}])

(defn app-page []
  [:div {:class "section"}
   [:div {:class "container"}
    [:div {:class "columns"}
     [:div {:class "column"}
      (case @input-mode
        :field [field-input]
        :json [json-input])]
     [:div {:class "column"}
      [:textarea {:class "textarea"
                  :placeholder "e.g. Hello world"
                  :value (str
                          "\n"
                          @user-schema
                          "\n"
                          (schema-user->malli @user-schema)
                          "\n"
                         
                          (with-out-str
                            (cljs.pprint/pprint
                             (.stringify js/JSON (clj->js @malli-schema))
                                         ))

                          )}]]]]])





(-> {:a 1}
    (clj->js)
    key
    (js/JSON.stringify)
    #_(clojure.string/replace  "{" "")
    #_(clojure.string/replace  "}" "")
    #_(clojure.string/replace  "" "")
    )

(defn about-page []
  [:div
   [:h2 "About frontend"]
   [:ul
    [:li [:a {:href "http://google.com"} "external link"]]
    [:li [:a {:href (rfe/href ::foobar)} "Missing route"]]]
   [:div
    {:content-editable true
     :suppressContentEditableWarning true}
    [:p "Link inside contentEditable element is ignored."]
    [:a {:href (rfe/href ::app)} "Link"]]])

(defonce match (rc/atom nil))

(defn current-page []
  [:div
   [:ul
    [:li [:a {:href (rfe/href ::app)} "App"]]
    [:li [:a {:href (rfe/href ::about)} "About"]]]
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
