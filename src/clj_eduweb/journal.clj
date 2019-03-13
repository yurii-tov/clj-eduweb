(ns clj-eduweb.journal
  (:require [clj-eduweb.core :refer :all]
            [clj-eduweb.elements :refer :all]
            [clojure.string :as cstr]))

; tasks

(defn get-tasks [] 
  (find-elements (css ".x-grid3-col-theme")))

(defn gen-taskname []
  (apply str (take 8 (uuid))))

(defn add-task
  ([] (add-task (gen-taskname)))
  ([title]
    (click (find-element (css "#e4-journal-TaskList-addTaskButton button")))
    (let [tasks-count (count (get-tasks))
          [window] (get-windows)
          [button] (get-buttons window)
          [_ title-input] (get-inputs window)]
      (send-keys title-input title)
      (click button)
      (wait-for-stale window)
      (wait-for (condition (> (count (get-tasks)) 
                              tasks-count))))))

; total marks

(defn add-total-mark
  "Parameters:
  mark  =>  :period :year :exam :total"
  [mark]
  (let [valid-options '(:period :year :exam :total)]
    (assert (some #{mark} valid-options)
      (str "valid options are: " valid-options))
    (click (find-element (css "#e4-journal-TaskList-addTaskButton .x-btn-mr")))
    (click (first (get-context-menu-options (first (get-context-menus)))))
    (let [[window] (get-windows)
          options (get-radio-buttons window)
          option (get (into {} (map vector valid-options options)) mark)]
      (click option)
      (click (first (get-buttons window)))
      (wait-for-stale window))))

; cells

(defn get-cells [] (find-elements *driver* (css "td.mark-cell")))

(defn get-columns []
  (->> (get-cells)
    (group-by 
      (fn [col] (re-find #"markColumn\d+" 
                  (get-attribute col "class"))))
    vals
    vec))

(defn get-column [taskname]
  ((get-columns) 
   (. (mapv get-text (get-tasks)) indexOf taskname)))

(defmacro foreach-cell
  "apply cell-fn to each cell found by find-cells-expression.
  find-cells-expression evaluates for each step in order to avoid staleness"
  ([find-cells-expression
    cell-fn]
   `(dorun
      (map (fn [~'i] (~cell-fn ((vec ~find-cells-expression) ~'i)))
        (range 0 (count ~find-cells-expression)))))
  ([cell-fn] `(foreach-cell (get-cells) ~cell-fn)))

; marks

(defn edit-mark 
  "Apply changes to given cell
  edit => function of dialog window"
  [cell edit]
  (double-click cell)
  (let [[w] (get-windows)]
    (edit w)
    (close-window w)
    (wait-for-stale cell)))

(defn set-mark [cell mark]
  (edit-mark cell 
    (fn [w] (->> w get-buttons (filter (fn [b] (= mark (get-text b)))) first click))))

(defn clear-mark [cell]
  (when (or (seq (get-text cell)) 
            (cstr/includes? (get-attribute cell "class") 
                            "to-survey"))
    (edit-mark cell
      (fn [w] (click (first (filter pressed? (get-buttons w))))))))

(defn set-random-mark [cell & marks-to-exclude]
  (edit-mark cell
    (fn [w] 
      (let [not-excluded? (comp (complement (set marks-to-exclude)) get-text)
            buttons (filter (every-pred not-excluded? displayed?) (get-buttons w))]
        (click (rand-nth buttons))))))

(defn set-random-marks [& marks-to-exclude]
  (foreach-cell
    (fn [cell] 
     (and (> (rand-int 10) 0)
      (apply set-random-mark cell marks-to-exclude)))))

(defn fill-column [column marks]
  (dorun
    (map (fn [cell mark] 
           (when mark (set-mark cell mark)))
      column 
      marks)))
