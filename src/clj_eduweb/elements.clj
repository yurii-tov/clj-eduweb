(ns clj-eduweb.elements
  "API to handle various page elements"
  (:require [clj-eduweb.core :refer :all]
            [clojure.string :as cstr]))

;; text inputs

(defn get-inputs
  ([context] 
   (find-elements context 
     (css "input[type=text], input[type=password]")))
  ([] (get-inputs *driver*)))

;; tables

(defn get-tables
  ([] (get-tables *driver*))
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
      (map (fn [t] (let [text (get-text t)] 
             (when-not (empty? text) text)))))))

;; radio buttons

(defn get-radio-buttons 
  ([context] (find-elements context (css "input[type=radio]")))
  ([] (get-radio-buttons *driver*)))

;; checkboxes

(defn get-checkboxes
  ([context] (find-elements context (css "input[type=checkbox]")))
  ([] (get-checkboxes *driver*)))

(defn set-checkbox
  "set checkbox to on or off.
  if :random keyword provided, set random value"
  [checkbox option]
  (if (= option :random)
    (set-checkbox checkbox (zero? (rand-int 2)))
    (let [checked? (get-attribute checkbox "checked")]
      (if option
        (or checked? (click checkbox))
        (and checked? (click checkbox))))))

;; buttons

(defn get-buttons 
  ([context] (find-elements context (css "button")))
  ([] (get-buttons *driver*)))

(defn pressed? [button]
  (= "true" (get-attribute button "aria-pressed")))

;; dialog windows

(defn get-windows [] (find-elements (css ".x-window")))

(defn close-window [window]
  (click (find-element window (css ".x-tool-close")))
  (wait-for-stale window))

;; context menu

(defn get-context-menus []
  (find-elements (css ".x-menu")))

(defn get-context-menu-options 
  ([context]
    (map (fn [x] (find-element x (css "span")))
      (remove (fn [x] (cstr/includes? (get-attribute x "class") "x-menu-sep-li"))
        (find-elements context (css ".x-menu-list-item")))))
  ([] (get-context-menu-options *driver*)))

;; combo list menu

(defn get-comboboxes
  ([] (get-comboboxes *driver*))
  ([context] (find-elements context (css "[role=combobox]"))))

(defn expand-combobox [combobox]
  (click (find-element combobox (css "img.x-form-trigger-arrow"))))

(def collapse-combobox 
  "just an alias to expand-combobox, for clarity"
  expand-combobox)

(defn select-combobox 
  "select an option in given combobox.
  if provide keyword :random provided, option selected in random order"
  [combobox option]
  (expand-combobox combobox)
  (let [options-list (get-combo-list-options)]
    (click (if (= option :random)
             (rand-nth options-list)
             (or (first (filter 
                          (fn [o] (= (get-text o) option)) 
                          options-list))
                 (throw (new IllegalStateException
                          (str "option not found: " option 
                            "; avalilable options is: "
                            (mapv get-text options-list)))))))))

(defn get-combo-lists
  ([context] (find-elements context (css ".x-combo-list-inner")))
  ([] (get-combo-lists *driver*)))

(defn get-combo-list-options
  ([context] (find-elements context (css "[role=listitem]")))
  ([] (get-combo-list-options *driver*)))
