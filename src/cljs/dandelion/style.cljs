(ns dandelion.style
  (:require
   [garden.core :as g]))

(defn font []
  (str "@import "
       "url('"
       "https://fonts.googleapis.com/css2?family=Source+Code+Pro&display=swap"
       "');"))

(defn body []
  (g/css [:body {:background-color :black
                 :color :white
	         :font-family "'Source Code Pro'"
                 :font-size "13px"
                      }]))

(defn combined []
  [:style
   (str (font)
        (body))])
