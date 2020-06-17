(ns clj-eduweb.qti
  (:require [clj-eduweb.core :refer :all])
  (:import org.openqa.selenium.support.ui.Select
           org.openqa.selenium.WebElement))


;; Qti base elements


(def ^{:dynamic true} *qti-frame*)


(defn find-qti-frame []
  (find-element (css "[name=qti-player-frame]")))


(defmacro with-qti-frame [frame & body]
  `(let [frame# ~frame]
     (binding [*qti-frame* {:element frame#
                            :src (get-attribute frame# "src")
                            :path (re-find #"(?<=\?path=)[^&]+"
                                           (get-attribute frame# "src"))}]
       (with-frame frame#
         ~@body))))


(defn find-qti-main-panel
  []
  (find-element (css "#main-panel")))


;; Qti contents (interactions)


(defn find-qti-interactions []
  (let [p (find-qti-main-panel)]
    (with-driver-config {:implicit-wait 0}
      (doall (for [[i-type selector] {:choice ".choice-interaction"
                                      :text-input "input[type=text]"
                                      :link ".link-interaction"
                                      :select "select.inline-choice"}
                   element (find-elements p (css selector))]
               {:itype i-type :element element})))))


(defmulti interaction-value :itype)


(defmethod interaction-value :select [i]
  (get-attribute (:element i) "value"))


(defmethod interaction-value :text-input [i]
  (get-attribute (:element i) "value"))


(defmulti interaction-fill (fn [i _] (:itype i)))


(defmethod interaction-fill :select [i answer]
  (.selectByVisibleText (new Select (:element i)) answer))


;; Qti control elements


(defn qti-show-answer []
  (click (find-element (css "#show-solution-button"))))


(defn qti-submit []
  (click (find-element (css "#commit-button"))))


(defn qti-next []
  (click (find-element (css "#next-button"))))


;; Content testing


(def qti-answers (atom {}))


(defn qti-store-answer []
  (qti-show-answer)
  (Thread/sleep 1)
  (swap! qti-answers
         assoc
         (:path *qti-frame*)
         (mapv interaction-value
               (find-qti-interactions))))


(defn qti-fill []
  (when-let [answer (@qti-answers (:path *qti-frame*))]
    (dorun (map interaction-fill
                (find-qti-interactions)
                answer))))
