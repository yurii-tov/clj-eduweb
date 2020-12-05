(ns clj-eduweb.elements
  "API to handle various web elements"
  (:require [clj-eduweb.core :refer :all]
            [clojure.string :as cstr]))


;; text inputs


(defn find-inputs
  ([context]
   (find-elements context
                  (css "input[type=text], input[type=password]")))
  ([] (find-inputs *driver*)))


;; links


(defn find-links
  ([context] (find-elements context (css "a")))
  ([] (find-links *driver*)))


(defn links-iterator [links]
  "Find direct resources links, return function for quick navigation"
  (let [urls (atom (mapv (fn [x] (element-attribute x "href"))
                         links))]
    (fn [] (when-let [url (first @urls)]
             (open-url url)
             (swap! urls rest)
             url))))


;; tables


(defn find-tables
  ([] (find-tables *driver*))
  ([context] (find-elements context (css "table"))))


(defn table-rows [table] (find-elements table (css "tr")))


(defn table-column
  "table => either <table> element or list of <tr>
  i      => index of column"
  [table i]
  (let [rows (if (coll? table)
               table
               (table-rows table))]
    (->> rows
         (map (fn [r] (nth (find-elements r (css "td")) i)))
         (map (fn [t] (let [text (element-text t)]
                        (when-not (empty? text) text)))))))


;; radio buttons


(defn find-radio-buttons
  ([context] (find-elements context (css "input[type=radio]")))
  ([] (find-radio-buttons *driver*)))


;; checkboxes


(defn find-checkboxes
  ([context] (find-elements context (css "input[type=checkbox]")))
  ([] (find-checkboxes *driver*)))


(defn checkbox-set
  "set checkbox to on or off.
  if :random keyword provided, set random value"
  [checkbox option]
  (if (= option :random)
    (checkbox-set checkbox (zero? (rand-int 2)))
    (let [checked? (element-attribute checkbox "checked")]
      (if option
        (or checked? (element-click checkbox))
        (and checked? (element-click checkbox))))))


;; buttons


(defn find-buttons
  ([context] (find-elements context (css "button")))
  ([] (find-buttons *driver*)))


(defn button-pressed? [button]
  (= "true" (element-attribute button "aria-pressed")))


;; dialog windows


(defn find-windows [] (find-elements (css ".x-window")))


(def find-window
  "Shorthand for (first (find-windows)).
  Useful if there is only one window"
  (comp first find-windows))


(defn window-close [window]
  (element-click (find-element window (css ".x-tool-close")))
  (wait-for-stale window))


;; context menu


(defn find-context-menus []
  (find-elements (css ".x-menu")))


(defn context-menu-options
  ([context]
   (map (fn [x] (find-element x (css "span")))
        (remove (fn [x] (cstr/includes? (element-attribute x "class") "x-menu-sep-li"))
                (find-elements context (css ".x-menu-list-item")))))
  ([] (context-menu-options *driver*)))


;; comboboxes


(defn find-combo-lists
  ([context] (find-elements context (css ".x-combo-list-inner")))
  ([] (find-combo-lists *driver*)))


(defn find-combo-listitems
  "Get list items (as web elements) from given context"
  ([context] (find-elements context (css "[role=listitem]")))
  ([] (find-combo-listitems *driver*)))


(defn find-comboboxes
  ([] (find-comboboxes *driver*))
  ([context] (find-elements context (css "[role=combobox]"))))


(defn combobox-expand
  "Expand given combobox, ensure there is only one combo-list appeared.
   Returns combo-list"
  [combobox]
  (with-retry
    (element-click (find-element combobox (css "img.x-form-trigger-arrow")))
    (wait-for (condition (let [combo-lists (find-combo-lists)]
                           (and (= 1 (count combo-lists))
                                (first combo-lists)))))))


(defn combobox-collapse
  [combobox]
  (let [[combo-list] (find-combo-lists)]
    (when combo-list
      (element-click (find-element combobox (css "img.x-form-trigger-arrow")))
      (wait-for-stale combo-list))))


(defn combobox-select
  "Select an option in given combobox.
  If keyword :random provided, select random option
  Arguments:
    - combobox (web element)
    - option-spec (value to search element in list of options)
    Option-spec may be:
      String
      java.util.regex.Pattern"
  [combobox option-spec]
  (let [options-list (find-combo-listitems (combobox-expand combobox))
        predicate (cond (string? option-spec)
                        (fn [o] (= (element-text o) option-spec))
                        (= java.util.regex.Pattern
                           (type option-spec))
                        (fn [o] (re-matches option-spec
                                            (element-text o)))
                        :else (throw (new IllegalArgumentException
                                          (format "Wrong option spec: %s. It should be a string, or a pattern"
                                                  option-spec))))]
    (element-click (if (= option-spec :random)
                     (rand-nth options-list)
                     (or (first (filter predicate options-list))
                         (throw (new IllegalStateException
                                     (str "option not found: " option-spec
                                          "; avalilable options is: "
                                          (mapv element-text options-list)))))))))


(defn combobox-options
  "Get list of given combobox options (as list of strings)"
  [combobox]
  (combobox-expand combobox)
  (let [options (mapv element-text (find-combo-listitems))]
    (combobox-collapse combobox)
    options))


(defn combobox-value
  "Get current selected value of a combobox"
  [combo]
  (element-attribute (first (find-inputs combo)) "value"))
