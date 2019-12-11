(ns clj-eduweb.journal
  (:import java.time.LocalDate
           java.time.format.DateTimeFormatter)
  (:require [clj-eduweb.core :refer :all]
            [clj-eduweb.elements :refer :all]
            [clojure.string :as cstr]))

(def ^:dynamic *date-format* "dd.MM.yy")

; tasks

(defn get-tasks [] 
  (find-elements (css ".x-grid3-col-theme")))

(defn gen-taskname []
  (apply str (take 8 (uuid))))

(defn add-task
  "Add new task.
  Arguments: hashmap of options
  Reference:
   ; -------------
   ; basic options
   ; -------------
  {:title \"task title\" ; :random by default
   :control-work? false  ; check \"control work\" checkbox?
   :date \"15.07.19\"    ; date of task: may be string or LocalDate object
   ; ----------------
   ; advanced options
   ; ----------------
   :lesson-type \"name of lesson type\"
   :activity \"name of activity\"
   :scale \"scale name\" ; explicitly set scale for this task
   }"
  ([] (add-task {}))
  ([{:keys [title
            control-work?
            date
            lesson-type
            activity
            scale]
     :or {title :random}}]
   (click (find-element (css "#e4-journal-TaskList-addTaskButton button")))
   (let [tasks-count (count (get-tasks))
         [window] (get-windows)
         [button :as buttons] (get-buttons window)
         [_ title-input] (get-inputs window)]
     (send-keys title-input 
                (if (= title :random)
                  (gen-taskname)
                  title))
     (when control-work?
       (set-checkbox 
        (first (get-checkboxes window)) 
        control-work?))
     (when date
       (let [date (cond (instance? LocalDate date)
                        (.format date (DateTimeFormatter/ofPattern *date-format*))
                        :else date)]
         (send-keys (first (get-inputs window)) date)))
     (when (or lesson-type activity scale)
       (click (last buttons))
       (let [[lesson-type-combo
              activity-combo
              scale-combo] (get-comboboxes window)]
         (dorun (map (fn [combo value] 
                       (when value (select-combobox combo value))) 
                     [lesson-type-combo activity-combo]
                     [lesson-type activity]))
         (when scale
           (set-checkbox (last (get-checkboxes window)) true)
           (select-combobox scale-combo scale))))
     (click button)
     (wait-for-stale window)
     (wait-for (condition (> (count (get-tasks)) 
                             tasks-count))))))

(defn add-tasks
  "Create a bunch of tasks.
  Parameters:
  Hashmap, same as for add-task, but with some additions.
  :count - number of tasks to add
  :date  - string, LocalDate or generator function. See mk-date-gen in clj-eduweb.core namespace
  :step  - interval between dates. ignored if :date is specified"
  [{:keys [date count step]
    :as args
    :or {step 1 date (LocalDate/now)}}]
  (let [parse-date (fn [s] (LocalDate/parse s (DateTimeFormatter/ofPattern *date-format*)))
        gen-date (if (fn? date)
                   date
                   (mk-date-gen
                    {:step step
                     :start (if (string? date)
                              (parse-date date)
                              date)}))
        gen-task-data (fn [] (assoc args :date (gen-date)))]
    (->> (repeatedly gen-task-data)
         (take count)
         (map add-task)
         dorun)))
  
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
  (let [cells (get-cells)
        get-col-num (fn [c] 
                      (second (re-find #"x-grid3-td-markColumn(\d+)" 
                                       (get-attribute c "class"))))
        col-count (count (set (map get-col-num cells)))]
    (apply map vector (partition col-count cells))))

(defn get-column [taskname]
  ((get-columns) 
   (. (mapv get-text (get-tasks)) indexOf taskname)))

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

(defn set-mark 
  "Set specific mark to given cell. 
  For randomness use set-random-mark or set-random-marks functions"
  [cell mark]
  (edit-mark
   cell 
   (fn [w]
     (->> w
          get-buttons
          (filter (fn [b] (= mark (get-text b))))
          first click))))

(defn clear-mark [cell]
  (when (or (seq (get-text cell)) 
            (cstr/includes? (get-attribute cell "class") 
                            "to-survey"))
    (edit-mark cell
               (fn [w] (click (first (filter pressed? (get-buttons w))))))))

(defn set-random-mark
  "Fill given cell with random mark"
  [cell & marks-to-exclude]
  (edit-mark cell
             (fn [w] 
               (let [not-excluded? (comp (complement (set marks-to-exclude)) get-text)
                     buttons (filter (every-pred not-excluded? displayed?) (get-buttons w))]
                 (click (rand-nth buttons))))))

(defn set-random-marks
  "Fill all existing cells on page with random marks"
  [& marks-to-exclude]
  (foreach-element [cell (get-cells)]
                   (and (> (rand-int 10) 0)
                        (apply set-random-mark cell marks-to-exclude))))
