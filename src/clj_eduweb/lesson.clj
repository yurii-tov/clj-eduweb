(ns clj-eduweb.lesson
  (:require [clj-eduweb.core :refer :all]
            [clj-eduweb.qti :as qti]))


;; Lesson runtime controls


(defn move-forward [frame]
  (try (qti/with-qti-frame-explicit
         frame
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


(def answers (atom {}))


(defn store-answer []
  "Store answer for current qti question, jump to the next one"
  (let [frame (qti/find-qti-frame)]
    (qti/with-qti-frame-explicit
      frame
      (swap! answers qti/store-answer))
    (move-forward frame)))


(defn fill-answer []
  "Plug stored answer into current question, jump to the next one"
  (let [frame (qti/find-qti-frame)]
    (qti/with-qti-frame-explicit frame
      (qti/fill-answer @answers)
      (Thread/sleep 200)
      (qti/submit))
    (Thread/sleep 1000)
    (move-forward frame)))


(defn store-answers []
  (reset! answers {})
  (while (store-answer))
  (complete-and-restart)
  @answers)


(defn fill-answers []
  (while (fill-answer))
  (complete))
