(ns clj-eduweb.date
  (:import java.time.LocalDate
           java.time.format.DateTimeFormatter))


(def ^:dynamic *date-format* "dd.MM.yy")


(defn parse-date [s]
  (LocalDate/parse s (DateTimeFormatter/ofPattern *date-format*)))


(defn format-date [date]
  (.format date (DateTimeFormatter/ofPattern *date-format*)))


(defn mk-date-gen
  "Create generator function which generate LocalDate objects
  Parameters:
  step - interval between dates (in days)
  start - Starting element of a sequence, LocalDate object. By default, current day"
  ([{:keys [step start]
     :or {step 1
          start (LocalDate/now)}}]
   (let [s (atom (.minusDays start step))
         inc-days (fn [d] (.plusDays d step))]
     (fn [] (swap! s inc-days))))
  ([] (mk-date-gen {})))
