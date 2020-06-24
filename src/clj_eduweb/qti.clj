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


(defn extract-qti-path [url]
  (re-find #"(?<=\?path=)[^&]+" url))


(defmacro with-qti-frame-explicit
  [frame & body]
  `(let [frame# ~frame]
     (binding [*qti-frame*
               {:src (get-attribute frame# "src")}]
       (with-frame frame#
         ~@body))))


(defmacro with-qti-frame [& body]
  `(with-qti-frame-explicit
     (find-qti-frame)
     ~@body))


(defn find-qti-main-panel
  []
  (find-element (css "#main-panel")))


;; Obtain qti source


(defn fetch-source []
  "Obtain question raw xml"
  (execute-javascript
   (format "return (await fetch('%s/%s').then(r => r.text()))"
           (get-base-url)
           (extract-qti-path
            (or (:src *qti-frame*)
                (.getCurrentUrl *driver*))))))


(defn parse-source [raw-xml]
  "Read qti xml into clojure data"
  (with-open [is (io/input-stream
                  (.getBytes raw-xml "utf-8"))]
    (xml/parse is)))


(defn extract-interactions
  ([xml-doc]
   "Extract meaningful parts from given qti data:
   - Right answers
   - Interactions source
   Return vector of
   {:answer [...]
    :source {:tag ...
             :content ...
             :attrs ...}}"
   (let [interactions (->> xml-doc
                           zip/xml-zip
                           (iterate zip/next)
                           (take-while (complement zip/end?))
                           (map zip/node)
                           (filter (comp :responseIdentifier :attrs)))
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
     (mapv (fn [i] {:answer (responses (-> i :attrs :responseIdentifier))
                    :source i})
           interactions)))
  ([] (-> (fetch-source)
          parse-source
          extract-interactions)))


;; Interaction-level API


(defn find-interactions [interactions-tags]
  "Find interactions WebElements in the DOM.
   Order found interactions by provided list of tag names,
   e.g. [:inlineChoiceInteraction
         :inlineChoiceInteraction
         :simpleChoice].
  Return vector of hashmaps like (for example)
  {:element ...
   :type :choice}"
  (let [selectors {:choice (css ".choice-interaction")
                   :text-input (css "input.text-entry")
                   :link (css ".link-interaction")
                   :select (css "select.inline-choice")
                   :order (css ".order-interaction")
                   :container (xpath ".//table[./tbody/tr/td/div/div[contains(@class, 'match-interaction-container')]]")}
        tag-translation {:choiceInteraction :choice
                         :textEntryInteraction :text-input
                         :inlineChoiceInteraction :select
                         :orderInteraction :order
                         :matchInteraction [:container :link]}
        main-panel (find-qti-main-panel)
        determine-interaction (fn [[interaction-type & variants]]
                                (when-not interaction-type
                                  (throw (new RuntimeException "Unable to determine interaction type")))
                                (let [found (with-driver-config {:implicit-wait 0}
                                              (find-elements
                                               main-panel
                                               (selectors interaction-type)))]
                                  (if (seq found)
                                    [found interaction-type]
                                    (recur variants))))]
    ((reduce (fn [result interaction-type]
               (let [[elements interaction-type]
                     (if (coll? interaction-type)
                       (determine-interaction interaction-type)
                       [(find-elements main-panel (selectors interaction-type))
                        interaction-type])
                     element-index (result interaction-type 0)
                     element (elements element-index)
                     interactions (result :interactions)]
                 (assoc result
                        interaction-type (inc element-index)
                        :interactions (conj interactions
                                            {:type interaction-type
                                             :element element}))))
             {:interactions []}
             (map tag-translation
                  interactions-tags))
     :interactions)))


(defmulti interaction-derive-answer
  "Make interaction-specific answer data structure"
  (fn [i _] (:type i)))


(defmethod interaction-derive-answer :default
  [_ {:keys [answer]}] answer)


(defmulti interaction-fill
  "Input provided answer to interaction"
  (fn [i _] (:type i)))


(defmethod interaction-fill :default [_])


;;;; Text input


(defmethod interaction-fill :text-input [i answer]
  (send-keys (:element i) answer))


(defmethod interaction-derive-answer :text-input
  [_ {:keys [answer]}] (first answer))


;;;; Choice


(defmethod interaction-fill :choice [i answer]
  (->> answer
       (map (fn [a] (find-element
                     (:element i)
                     (css (format "input[value=%s]" a)))))
       (map click)
       dorun))


;;;; Link


(defmethod interaction-derive-answer :link [_ {:keys [answer]}]
  (map (fn [x] (cstr/split x #" "))
       answer))


(defmethod interaction-fill :link [i answer]
  (doseq [xs answer
          x xs]
    (click (find-element (css (format ".match-interaction-item-%s" x))))))


;;;; Select


(defmethod interaction-derive-answer :select
  [_ {[a] :answer
      {content :content} :source}]
  (->> content
       (filter (comp #{a} :identifier :attrs))
       ((comp first :content first))))


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


(defmethod interaction-derive-answer :container [_ {:keys [answer]}]
  (map (fn [x] (cstr/split x #" "))
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
  (let [interactions-data (extract-interactions)
        interactions-tags (map (comp :tag :source)
                               interactions-data)]
    (dorun (map (fn [interaction data]
                  (interaction-fill
                   interaction
                   (interaction-derive-answer interaction data)))
                (find-interactions interactions-tags)
                interactions-data))))


(defn solve []
  (fill-answer)
  (Thread/sleep 500)
  (submit))


(defn move-forward []
  (click (find-element (css "#next-button"))))
