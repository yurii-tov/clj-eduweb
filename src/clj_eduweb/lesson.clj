(ns clj-eduweb.lesson
  (:require [clj-eduweb.core :refer :all]
            [clj-eduweb.qti :as qti]))


;; Lesson runtime controls


(defn move-forward [frame]
  (try (qti/with-qti-frame-explicit
         frame
         (with-driver-config {:implicit-wait 0}
           (qti/move-forward)))
       (wait-for (condition (element-stale? frame)))
       (catch Exception e)))


(defn skip []
  (qti/with-qti-frame
    (qti/move-forward)))


(defn complete []
  (element-click (find-element (css "#e4-library-LessonPanel-closeButton")))
  (element-text (find-element (css ".lesson-result-score-value"))))


(defn start []
  "Start from lesson main page"
  (element-click (find-element (css "#btnStartLesson"))))


(defn start-again []
  "Start from results page"
  (element-click (find-element (css "#e4-library-LessonStatsPanel-StartAgainButton")))
  (start))


(defn complete-and-restart []
  (complete)
  (start-again))


;; Questions controls


(defn solve-question []
  "Plug answer into current question, jump to the next one"
  (let [frame (qti/find-qti-frame)]
    (qti/with-qti-frame-explicit frame
      (qti/solve))
    (Thread/sleep 500)
    (move-forward frame)))


(defn solve []
  "Solve all questions in lesson, return final score"
  (while (solve-question))
  (complete))
