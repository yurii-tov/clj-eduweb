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


(defn find-interactions []
  (let [p (find-qti-main-panel)]
    (with-driver-config {:implicit-wait 0}
      (doall (for [[i-type selector] {:choice ".choice-interaction"
                                      :text-input "input[type=text]"
                                      :link ".link-interaction"
                                      :select "select.inline-choice"}
                   element (find-elements p (css selector))]
               {:itype i-type :element element})))))


(defmulti interaction-value :itype)


(defmethod interaction-value :default [i])


(defmethod interaction-value :select [i]
  (get-attribute (:element i) "value"))


(defmethod interaction-value :text-input [i]
  (get-attribute (:element i) "value"))


(defmethod interaction-value :choice [i]
  (set (map (fn [x] (get-attribute x "value"))
            (find-elements (:element i)
                           (css "input[checked]")))))


(defmulti interaction-fill (fn [i _] (:itype i)))


(defmethod interaction-fill :default [i _])


(defmethod interaction-fill :select [i answer]
  (.selectByVisibleText (new Select (:element i)) answer))


(defmethod interaction-fill :text-input [i answer]
  (send-keys (:element i) answer))


(defmethod interaction-fill :choice [i answer]
  (->> (find-elements (:element i) (css "input"))
       (filter (comp answer (fn [x] (get-attribute x "value"))))
       (map click)
       dorun))


;; Qti control elements


(defn show-answer []
  (click (find-element (css "#show-solution-button"))))


(defn submit []
  (click (find-element (css "#commit-button"))))


(defn move-forward []
  (click (find-element (css "#next-button"))))


;; Fetch question data


(defn fetch-data []
  (execute-javascript
   (format "return (await fetch('%s/%s').then(r => r.text()))"
           (get-base-url)
           (:path *qti-frame*))))


;; Content testing


(defn store-answer [storage]
  (show-answer)
  (Thread/sleep 1000)
  (assoc storage
         (:path *qti-frame*)
         (mapv interaction-value
               (find-interactions))))


(defn fill-answer [storage]
  (when-let [answer (-> *qti-frame* :path storage)]
    (dorun (map interaction-fill
                (find-interactions)
                answer))))
