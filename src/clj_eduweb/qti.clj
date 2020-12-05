(ns clj-eduweb.qti
  (:require [clj-eduweb.core :refer :all]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.string :as cstr]
            [clojure.java.io :as io])
  (:import org.openqa.selenium.support.ui.Select
           org.openqa.selenium.interactions.Actions
           org.openqa.selenium.WebElement))


;; Context handling


(def ^{:dynamic true} *qti-frame*)


(defn find-qti-frame []
  (find-element (css "[name=qti-player-frame], [name=sco-frame]")))


(defn extract-qti-path [url]
  (re-find #"(?<=\?path=)[^&]+" url))


(defmacro with-qti-frame-explicit
  [frame & body]
  `(let [frame# ~frame]
     (binding [*qti-frame*
               {:src (element-attribute frame# "src")}]
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


(defn peek-source
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
          peek-source)))


;; Interaction-level API


(defmulti interaction-derive-answer
  "Transform raw answer into interaction-specific form"
  :type)


(defmethod interaction-derive-answer :default
  [i] (i :answer))


(defmulti interaction-fill
  "Input provided answer to interaction"
  (fn [i _] (:type i)))


(defmethod interaction-fill :default [_])


(defn find-interactions []
  "Get all interactions data (including source, WebElement, right answer)
   Inspecting DOM as well as qti xml data from server.
   Return vector of hashmaps like (for example)
   {:element ...
    :type ...
    :source ...
    :answer ...}"
  (let [selectors {:choice (css ".choice-interaction")
                   :text-input (css "input.text-entry")
                   :link (css ".link-interaction")
                   :hottext (css ".hottext-interaction")
                   :select (css "select.inline-choice")
                   :order (css ".order-interaction")
                   :container (xpath ".//table[descendant::*[contains(@class, 'match-interaction')]]|.//div[descendant::*[contains(@class, 'match-interaction')]]")
                   :graphic-gap (xpath ".//table[descendant::*[contains(@class, 'graphic-gap-match')]]")
                   :hotspot (css ".hotspot")}
        guess-interaction
        (fn [tag] (or ({:choiceInteraction :choice
                        :textEntryInteraction :text-input
                        :inlineChoiceInteraction :select
                        :hottextInteraction :hottext
                        :orderInteraction :order
                        :matchInteraction [:link :container]
                        :hotspotInteraction :hotspot
                        :graphicGapMatchInteraction :graphic-gap} tag)
                      (throw (new RuntimeException (format "Unknown interaction tag: %s" tag)))))
        main-panel (find-qti-main-panel)
        determine-interaction (fn [[itype & variants]]
                                (when-not itype
                                  (throw (new RuntimeException "Unable to determine interaction type")))
                                (let [found (with-driver-config {:implicit-wait 0}
                                              (find-elements
                                               main-panel
                                               (selectors itype)))]
                                  (if (seq found)
                                    [found itype]
                                    (recur variants))))]
    ((reduce (fn [result {{:keys [tag answer]} :source
                          :as interaction-source}]
               (let [itype-guess (guess-interaction tag)
                     [elements itype]
                     (if (coll? itype-guess)
                       (determine-interaction itype-guess)
                       [(find-elements main-panel (selectors itype-guess))
                        itype-guess])
                     element-index (result itype 0)
                     element (elements element-index)
                     interactions (result :interactions)
                     interaction (into interaction-source
                                       {:type itype
                                        :element element})
                     answer (interaction-derive-answer
                             interaction)]
                 (assoc result
                        itype (inc element-index)
                        :interactions
                        (conj interactions
                              (assoc interaction :answer answer)))))
             {:interactions []}
             (peek-source))
     :interactions)))


;;;; Text input


(defmethod interaction-derive-answer :text-input [{:keys [answer]}]
  (or (first answer) ""))


(defmethod interaction-fill :text-input [i answer]
  (element-send-keys (:element i) answer))


;;;; Choice


(defmethod interaction-fill :choice [i answer]
  (->> answer
       (filter (complement nil?))
       (map (fn [a] (find-element
                     (:element i)
                     (css (format "input[value='%s']" a)))))
       (map element-click)
       dorun))


;;;; Link


(defmethod interaction-derive-answer :link [{:keys [answer]}]
  (map (fn [x] (cstr/split x #" "))
       answer))


(defmethod interaction-fill :link [i answer]
  (doseq [xs answer
          x xs]
    (element-click (find-element (css (format ".match-interaction-item-%s" x))))))


;;;; Select


(defmethod interaction-derive-answer :select
  [{[a] :answer
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
                  (element-click element)
                  (element-click (items i))))
              (range)
              answer)))


;;;; Container


(defmethod interaction-derive-answer :container [{:keys [answer]}]
  (map (fn [x] (cstr/split x #" "))
       answer))


(defmethod interaction-fill :container [i answer]
  (doseq [[x c] answer
          :let [container (find-element (:element i)
                                        (css (format ".match-interaction-container-%s" c)))
                item (find-element (:element i)
                                   (css (format ".match-interaction-items .match-interaction-item-%s" x)))]]
    (element-click item)
    (if-let [heading (first (with-driver-config {:implicit-wait 0}
                              (find-elements container
                                             (css ".panel-heading"))))]
      (element-click heading)
      (element-click container))))


;;;; Hottext


(defmethod interaction-derive-answer :hottext [{:keys [answer source]}]
  (letfn [(xml-element-text [e]
            (if (:content e)
              (xml-element-text (first (:content e)))
              e))]
    (->> source
         zip/xml-zip
         (iterate zip/next)
         (take-while (complement zip/end?))
         (map zip/node)
         (filter (comp #{:hottext} :tag))
         (filter (comp (set answer) :identifier :attrs))
         (map (comp cstr/trim xml-element-text))
         (into #{}))))


(defmethod interaction-fill :hottext [i answer]
  (doseq [x (find-elements (:element i) (css ".hottext"))
          :when (answer (element-text x))]
    (element-click x)))


;;;; Graphic gap match interaction


(defmethod interaction-derive-answer :graphic-gap [{:keys [answer source]}]
  (let [nodes (->> source
                   zip/xml-zip
                   (iterate zip/next)
                   (take-while (complement zip/end?))
                   (map zip/node))
        ids (map (fn [x] (cstr/split x #"\s"))
                 answer)
        parse-coords (fn [x] (partition 2 (map read-string (cstr/split x #","))))
        midpoint (fn [[p1 p2]]
                   (mapv (fn [a b] (quot (+ a b) 2)) p1 p2))]
    (map (fn [[gap-id target-id]]
           (vector ((comp :data :attrs first :content)
                    (first (filter (comp #{gap-id} :identifier :attrs) nodes)))
                   ((comp midpoint parse-coords :coords :attrs)
                    (first (filter (comp #{target-id} :identifier :attrs) nodes)))))
         ids)))


(defmethod interaction-fill :graphic-gap [{:keys [element]} answer]
  (let [midpoint (fn [p1 p2] (mapv (fn [a b] (quot (+ a b) 2)) p1 p2))
        container (find-element element (css ".graphic-gap-match"))
        container-rect (element-rect container)
        container-zero ((juxt :x :y) container-rect)
        container-mid (midpoint container-zero
                                (map + container-zero
                                     ((juxt :width :height) container-rect)))
        [move-offset-x
         move-offset-y] (map - container-zero container-mid)
        single-spot? (= 1 (count (set (map second answer))))]
    (doseq [[data [target-x target-y]] answer]
      (element-click (first (reverse (find-elements element (css (format "[src*='%s']" data))))))
      (when-not single-spot?
        (.. (new Actions *driver*)
            ;; Move mouse to upper-left corner of a container
            (moveToElement container move-offset-x move-offset-y)
            ;; Move mouse to target coordinates
            (moveByOffset target-x target-y)
            element-click
            build
            perform)))))


;;;; Hotspot interaction


(defmethod interaction-derive-answer :hotspot [{:keys [answer source]}]
  (let [nodes (->> source
                   zip/xml-zip
                   (iterate zip/next)
                   (take-while (complement zip/end?))
                   (map zip/node))
        parse-coords (fn [x] (partition 2 (map read-string (cstr/split x #","))))
        midpoint (fn [[p1 p2]]
                   (if p2 (mapv (fn [a b] (quot (+ a b) 2)) p1 p2)
                       p1))]
    (map (fn [spot-id]
           ((comp midpoint parse-coords :coords :attrs)
            (first (filter (comp #{spot-id} :identifier :attrs) nodes))))
         answer)))


(defmethod interaction-fill :hotspot [{:keys [element]} answer]
  (let [midpoint (fn [p1 p2] (mapv (fn [a b] (quot (+ a b) 2)) p1 p2))
        container element
        container-rect (element-rect container)
        container-zero ((juxt :x :y) container-rect)
        container-mid (midpoint container-zero
                                (map + container-zero
                                     ((juxt :width :height) container-rect)))
        [move-offset-x
         move-offset-y] (map - container-zero container-mid)]
    (doseq [[target-x target-y] answer]
      (.. (new Actions *driver*)
          ;; Move mouse to upper-left corner of a container
          (moveToElement container move-offset-x move-offset-y)
          ;; Move mouse to target coordinates
          (moveByOffset target-x target-y)
          element-click
          build
          perform))))


;; Question-level API


(defn show-answer []
  (element-click (find-element (css "#show-solution-button"))))


(defn submit []
  (let [feedback-panel (find-element (css "#feedback-panel"))]
    (element-click (find-element (css "#commit-button")))
    (wait-for (condition (element-displayed? feedback-panel)))
    (let [feedback-class (element-attribute feedback-panel "class")
          feedback-text (element-text feedback-panel)
          feedback-type (or (some (fn [[k v]]
                                    (when (cstr/includes? feedback-class v) k))
                                  {:success "alert-success"
                                   :danger "alert-danger"})
                            :unknown)]
      {:text feedback-text :type feedback-type})))


(defn fill-answer []
  (dorun (map (fn [{:keys [answer] :as interaction}]
                (interaction-fill interaction
                                  answer))
              (find-interactions))))


(defn solve []
  (fill-answer)
  (Thread/sleep 500)
  (submit))


(defn move-forward []
  (element-click (find-element (css "#next-button"))))
