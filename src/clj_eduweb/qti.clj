(ns clj-eduweb.qti
  (:require [clj-eduweb.core :refer :all]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io])
  (:import org.openqa.selenium.support.ui.Select
           org.openqa.selenium.WebElement))


;; Context handling


(def ^{:dynamic true} *qti-frame*)


(defn find-qti-frame []
  (find-element (css "[name=qti-player-frame]")))


(defmacro with-qti-frame-explicit
  [frame & body]
  `(let [frame# ~frame]
     (binding [*qti-frame* {:element frame#
                            :src (get-attribute frame# "src")
                            :path (re-find #"(?<=\?path=)[^&]+"
                                           (get-attribute frame# "src"))}]
       (with-frame frame#
         ~@body))))


(defmacro with-qti-frame [& body]
  `(with-qti-frame-explicit
     (find-qti-frame)
     ~@body))


(defn find-qti-main-panel
  []
  (find-element (css "#main-panel")))


;; Peek question data


(declare fetch-data
         parse-data
         extract-responses)


(defn peek-answer []
  "High-level api for obtaining answer for given question"
  (-> (fetch-data)
      (parse-data)
      (extract-responses)))


(defn fetch-data []
  "Obtain question raw xml"
  (execute-javascript
   (format "return (await fetch('%s/%s').then(r => r.text()))"
           (get-base-url)
           (:path *qti-frame*))))


(defn parse-data [raw-xml]
  "Parse question xml into clojure data"
  (with-open [is (io/input-stream
                  (.getBytes raw-xml "utf-8"))]
    (xml/parse is)))


(defn extract-responses [xml-doc]
  "Get responses data from parsed xml document
   Return vector of responses
   (One response for interaction)"
  (let [interactions (->> xml-doc
                          zip/xml-zip
                          (iterate zip/next)
                          (take-while (complement zip/end?))
                          (keep (comp :responseIdentifier :attrs zip/node)))
        responses (->> xml-doc
                       :content
                       (filter (comp #{:responseDeclaration} :tag))
                       (map (juxt (comp :identifier :attrs)
                                  (fn [r] (->> (:content r)
                                               (filter (comp #{:correctResponse} :tag))
                                               first
                                               :content
                                               (map (comp first :content))))))
                       (map (fn [[identifier correct-responses]]
                              (vector identifier
                                      (if (> (count correct-responses) 1)
                                        (vec correct-responses)
                                        (first correct-responses)))))
                       (into {}))]
    (mapv responses interactions)))


;; Interaction-level API


(defn find-interactions []
  (let [p (find-qti-main-panel)]
    (with-driver-config {:implicit-wait 0}
      (doall (for [[i-type selector] {:choice ".choice-interaction"
                                      :text-input "input[type=text]"
                                      :link ".link-interaction"
                                      :select "select.inline-choice"}
                   element (find-elements p (css selector))]
               {:itype i-type :element element})))))


(defmulti interaction-value
  "Extract values from interaction element"
  :itype)


(defmethod interaction-value :default [_])


(defmulti interaction-fill
  "Input provided answer to interaction"
  (fn [i _] (:itype i)))


(defmethod interaction-fill :default [_])


(defmulti interaction-parse-answer
  "Convert raw text answer to more convinient form"
  (fn [i _] (:itype i)))


(defmethod interaction-parse-answer :default
  [_ answer] answer)


;;;; Text input


(defmethod interaction-value :text-input [i]
  (get-attribute (:element i) "value"))


(defmethod interaction-fill :text-input [i answer]
  (send-keys (:element i) answer))


;;;; Choice


(defmethod interaction-value :choice [i]
  (set (map (fn [x] (get-attribute x "value"))
            (find-elements (:element i)
                           (css "input[checked]")))))


(defmethod interaction-fill :choice [i answer]
  (->> (find-elements (:element i) (css "input"))
       (filter (comp answer (fn [x] (get-attribute x "value"))))
       (map click)
       dorun))


;;;; Select


(defmethod interaction-value :select [i]
  (get-attribute (:element i) "value"))


(defmethod interaction-fill :select [i answer]
  (.selectByVisibleText (new Select (:element i)) answer))


;; Question-level API


(defn show-answer []
  (click (find-element (css "#show-solution-button"))))


(defn submit []
  (click (find-element (css "#commit-button"))))


(defn store-answer [storage]
  (show-answer)
  (Thread/sleep 1000)
  (assoc storage
         (:path *qti-frame*)
         (mapv interaction-value
               (find-interactions))))


(defn fill-answer
  ([storage]
   (when-let [answer (-> *qti-frame* :path storage)]
     (dorun (map interaction-fill
                 (find-interactions)
                 answer))))
  ([]
   (dorun (map (fn [i a] (interaction-fill
                          i
                          (interaction-parse-answer i a)))
               (find-interactions)
               (peek-answer)))))


(defn solve []
  (fill-answer)
  (Thread/sleep 500)
  (submit))


(defn move-forward []
  (click (find-element (css "#next-button"))))
