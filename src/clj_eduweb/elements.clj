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

;; comboboxes

(defn get-combo-lists
  ([context] (find-elements context (css ".x-combo-list-inner")))
  ([] (get-combo-lists *driver*)))

(defn get-combo-listitems
  "Get list items (as web elements) from given context"
  ([context] (find-elements context (css "[role=listitem]")))
  ([] (get-combo-listitems *driver*)))

(defn get-comboboxes
  ([] (get-comboboxes *driver*))
  ([context] (find-elements context (css "[role=combobox]"))))

(defn expand-combobox
  "Expand given combobox, ensure there is only one combolist appeared.
   Returns combolist"
  [combobox]
  (with-retry
    (click (find-element combobox (css "img.x-form-trigger-arrow")))
    (wait-for (condition (let [combolists (get-combo-lists)]
                           (and (= 1 (count combolists))
                                (first combolists)))))))

(defn collapse-combobox
  [combobox]
  (let [[combolist] (get-combo-lists)]
    (when combolist
      (click (find-element combobox (css "img.x-form-trigger-arrow")))
      (wait-for-stale combolist))))

(defn select-combobox 
  "Select an option in given combobox.
  Assume there is only one opened combo list at a time.
  if keyword :random provided, option selected in random order"
  [combobox option]
  (let [options-list (get-combo-listitems (expand-combobox combobox))]
    (click (if (= option :random)
             (rand-nth options-list)
             (or (first (filter 
                          (fn [o] (= (get-text o) option)) 
                          options-list))
                 (throw (new IllegalStateException
                          (str "option not found: " option 
                            "; avalilable options is: "
                            (mapv get-text options-list)))))))))

(defn get-combobox-options
  "Get list of given combobox options (as list of strings)"
  [combobox]
  (expand-combobox combobox)
  (let [options (mapv get-text (get-combo-listitems))]
    (collapse-combobox combobox)
    options))
