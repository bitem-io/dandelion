(ns dandelion.sign
  (:require
   [clojure.spec.alpha :as s]
   [buddy.sign.jwt :as bjwt]
   [dandelion.shape :as h])
  (:import
   (java.time Instant)))

(def secret-temp
  "bitem-success")

(defn now []
  (Instant/now))

(defn after-seconds [sec]
  (-> (Instant/now)
      (.plusSeconds sec)))

(s/fdef jwt
  :args (s/cat :person ::h/person
               :duration (s/and int? pos?)))
(defn jwt
  ([person]
   (jwt person 3600))
  ([person duration]
   (let [claims (assoc (dissoc (:doc person) :password)
                       :exp (after-seconds duration))]
     (bjwt/sign claims secret-temp))))

(defn verify [token]
  (bjwt/unsign token secret-temp))
