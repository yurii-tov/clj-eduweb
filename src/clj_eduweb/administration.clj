(ns clj-eduweb.administration
  (:require [clj-eduweb.core :refer :all]
            [clj-eduweb.elements :refer :all]))


;; journals


(defn add-journal
  "Add new journal page.
  Args: journal-spec (hashmap)
  Recognized options:
  :period
  :subject
  :group
  :teacher"
  [{:keys [period subject group teacher]}]
  (click (find-element (css "#e4-administration-menutree-journallist-addItem button")))
  (let [[window] (find-windows)]
    (dorun (map (fn [combo value]
                  (when value
                    (combobox-select combo value)))
                (find-comboboxes window)
                [period subject group teacher]))
    (click (first (find-buttons window)))
    (wait-for-stale window)))
