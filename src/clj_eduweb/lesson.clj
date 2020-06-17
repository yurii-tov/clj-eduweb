(ns clj-eduweb.lesson
  (:require [clj-eduweb.core :refer :all]
            [clj-eduweb.qti :as qti]))


(defn move-forward [frame]
  (try (qti/with-qti-frame frame
         (with-driver-config {:implicit-wait 0}
           (qti/move-forward)))
       (wait-for (condition (stale? frame)))
       (catch Exception e)))


(defn complete []
  (click (find-element (css "#e4-library-LessonPanel-closeButton")))
  (get-text (find-element (css ".lesson-result-score-value"))))


(defn start []
  (click (find-element (css "#btnStartLesson"))))


(defn start-again []
  "Start from results page"
  (click (find-element (css "#e4-library-LessonStatsPanel-StartAgainButton")))
  (start))


(defn complete-and-restart []
  (complete)
  (start-again))


;; Content testing


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
      (qti/fill-answer)
      (qti/submit))
    (move-forward frame)))


(defn store-answers []
  (while (store-answer)))


(defn fill-answers []
  (while (fill-answer))
  (complete))
