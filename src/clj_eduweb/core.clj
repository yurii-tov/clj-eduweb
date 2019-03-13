(ns clj-eduweb.core
  "Wrapper over Selenium API and other general-purpose stuff"
  (:import
    (org.openqa.selenium.chrome 
      ChromeDriver 
      ChromeOptions) 
    (org.openqa.selenium WebElement By) 
    org.openqa.selenium.interactions.Actions 
    (org.openqa.selenium.support.ui 
      FluentWait
      ExpectedCondition
      ExpectedConditions)
    java.time.Duration
    java.util.concurrent.TimeUnit
    java.util.UUID)
  (:require [clojure.string :as cstr]))

;; driver management

(defn set-driver 
  "assign *driver* dynamic variable"
  [driver]
  (def ^:dynamic *driver* driver))

(defn start-chromedriver 
  "Start new Chrome instance.
  Optional arguments:
  :headless       turn on headless mode 
  :implicit-wait  implicit wait (in seconds)"
  [& {:keys [headless implicit-wait]}]
  (let [chrome-options (new ChromeOptions)]
    (when headless (. chrome-options setHeadless true))
    (let [chromedriver (new ChromeDriver chrome-options)]
      (when implicit-wait 
        (.. chromedriver 
          manage 
          timeouts 
          (implicitlyWait implicit-wait TimeUnit/SECONDS)))
      (set-driver chromedriver))))

;; find elements

(defn css [selector]
  (By/cssSelector selector))

(defn find-elements
  ([context selector]
    (vec (. context findElements selector)))
  ([selector] (find-elements *driver* selector)))

(defn find-element
  ([context selector]
    (. context findElement selector))
  ([selector] (find-element *driver* selector)))

;; perform actions on elements

(defn click [el] (. el click))

(defn send-keys [input text]
  (. input clear)
  (. input sendKeys (into-array [text])))

(defn double-click [el] (.. (new Actions *driver*) (doubleClick el) perform))

(defn get-text [el] (cstr/trim (. el getText)))

(defn displayed? [el]
  (. el isDisplayed))

(defn get-attribute [el attr]
  (. el getAttribute attr))

(defn inner-html [element] 
  (. *driver* executeScript "return arguments[0].innerHTML" (into-array [element])))

(defn clean-html [element] 
  (cstr/replace (inner-html element) #"(<\w+)(\s[^>]+)(>)" "$1$3"))

;; Wait for conditions

(defn wait-for
  "Wait for specific condition.
  Args:
  condition  =>  ExpectedCondition instance
  :timeout   =>  (optional) timeout in seconds (default is 3)" 
  [condition & {:keys [timeout]
                :or {timeout 3}}]
  (.. (new FluentWait *driver*)
    (withTimeout (Duration/ofSeconds timeout)) 
    (until condition)))

(defmacro condition [body]
  `(proxy [ExpectedCondition] [] 
     (~'apply [driver#] ~body) 
     (~'toString [] (str "condition: " '~body))))

(defn wait-for-stale [el]
  (wait-for (ExpectedConditions/stalenessOf el)))

;; utils

(defn uuid []
  (str (UUID/randomUUID)))
