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
  "Start from lesson main page"
  (click (find-element (css "#btnStartLesson"))))


(defn start-again []
  "Start from results page"
  (click (find-element (css "#e4-library-LessonStatsPanel-StartAgainButton")))
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
    (Thread/sleep 1000)
    (move-forward frame)))


(defn solve []
  "Solve all questions in lesson, return final score"
  (while (solve-question))
  (complete))
