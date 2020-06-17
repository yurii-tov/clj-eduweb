(ns clj-eduweb.lesson
  (:require [clj-eduweb.core :refer :all]
            [clj-eduweb.qti :as qti]))


(defn move-forward [frame]
  (try (qti/with-qti-frame frame
         (with-driver-config {:implicit-wait 0}
           (qti/move-forward)))
       (wait-for (condition (stale? frame)))
       (catch Exception e)))



(defn store-answer []
  "Store answer for current qti question, jump to the next one"
  (let [frame (qti/find-qti-frame)]
    (qti/with-qti-frame frame
      (qti/store-answer))
    (move-forward frame)))


(defn fill-answer []
  "Plug stored answer into current question, jump to the next one"
  (let [frame (qti/find-qti-frame)]
    (qti/with-qti-frame frame
      (qti/fill-answer))
    (move-forward frame)))
