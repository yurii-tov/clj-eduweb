(ns clj-eduweb.qti
  (:require [clj-eduweb.core :refer :all]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.string :as cstr]
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
                                               (mapv (comp first :content))))))
                       (into {}))]
    (mapv responses interactions)))



;; Interaction-level API


(defn find-interactions []
  (let [p (find-qti-main-panel)]
    (with-driver-config {:implicit-wait 0}
      (doall (for [[i-type selector] {:choice (css ".choice-interaction")
                                      :text-input (css "input.text-entry")
                                      :link (css ".link-interaction")
                                      :select (css "select.inline-choice")
                                      :order (css ".order-interaction")
                                      :container (xpath ".//table[./tbody/tr/td/div/div[contains(@class, 'match-interaction-container')]]")}
                   element (find-elements p selector)]
               {:itype i-type :element element})))))


(defmulti interaction-parse-answer
  "Convert raw text answer to more convinient form"
  (fn [i _] (:itype i)))


(defmethod interaction-parse-answer :default
  [_ answer] answer)


(defmulti interaction-fill
  "Input provided answer to interaction"
  (fn [i _] (:itype i)))


(defmethod interaction-fill :default [_])


;;;; Text input


(defmethod interaction-fill :text-input [i answer]
  (send-keys (:element i) answer))


(defmethod interaction-parse-answer :text-input
  [_ answer] (first answer))


;;;; Choice


(defmethod interaction-fill :choice [i answer]
  (->> answer
       (map (fn [a] (find-element
                     (:element i)
                     (css (format "input[value=%s]" a)))))
       (map click)
       dorun))


;;;; Link


(defmethod interaction-parse-answer :link [i answer]
  (->> answer
       (map (fn [x] (cstr/split x #" ")))
       (into {})))


(defmethod interaction-fill :link [i answer]
  (doseq [xs answer
          x xs]
    (click (find-element (css (format ".match-interaction-item-%s" x))))))


;;;; Select


(defmethod interaction-parse-answer :select
  [_ answer] (first answer))


(defmethod interaction-fill :select [i answer]
  (.selectByVisibleText (new Select (:element i)) answer))


;;;; Order


(defmethod interaction-fill :order [i answer]
  (dorun (map (fn [i x]
                (let [items (find-elements (css ".order-interaction-item"))
                      element (find-element (css (format ".order-interaction-item-%s" x)))]
                  (click element)
                  (click (items i))))
              (range)
              answer)))


;;;; Container


(defmethod interaction-parse-answer :container
  [_ answer] (map (fn [x] (cstr/split x #" "))
                  answer))


(defmethod interaction-fill :container [i answer]
  (doseq [[x c] answer
          :let [container (find-element (css (format ".match-interaction-container-%s .panel-heading" c)))
                item (find-element (css (format ".match-interaction-item-%s" x)))]]
    (click item)
    (click container)))


;; Question-level API


(defn show-answer []
  (click (find-element (css "#show-solution-button"))))


(defn submit []
  (click (find-element (css "#commit-button"))))


(defn fill-answer []
  (dorun (map (fn [i a] (interaction-fill
                         i
                         (interaction-parse-answer i a)))
              (find-interactions)
              (peek-answer))))


(defn solve []
  (fill-answer)
  (Thread/sleep 500)
  (submit))


(defn move-forward []
  (click (find-element (css "#next-button"))))
