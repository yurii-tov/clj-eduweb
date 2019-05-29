(ns clj-eduweb.core
  "Wrapper over Selenium API and other general-purpose stuff"
  (:import
    (org.openqa.selenium.chrome 
      ChromeDriver 
      ChromeOptions) 
    (org.openqa.selenium 
     WebElement 
     By 
     OutputType 
     StaleElementReferenceException) 
    org.openqa.selenium.interactions.Actions 
    (org.openqa.selenium.support.ui 
      FluentWait
      ExpectedCondition
      ExpectedConditions)
    java.time.Duration
    java.util.concurrent.TimeUnit
    java.util.UUID)
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]))

;; driver management

(defn init-driver 
  "Initialize *driver* dynamic variable, if not yet initialized.
  Return its argument (driver)"
  [driver]
  (defonce ^:dynamic *driver* driver)
  driver)

(defmacro with-driver 
  "Perform body with *driver* variable bound to driver"
  [driver & body]
  `(binding [*driver* ~driver]
     ~@body))

(defn config-webdriver 
  "Set WebDriver-specific options.
   Recognized options:
   :implicit-wait  (implicit wait in seconds)"
  [driver {:keys [implicit-wait]
  	       :or {implicit-wait 3}}]
  (when implicit-wait 
    (.. driver
      manage
      timeouts
      (implicitlyWait implicit-wait TimeUnit/SECONDS))))

(defmulti start-driver 
  "Configure and start new webdriver instance, set *driver* dynamic var
   Args:
   options
   Recognized keys: 
   :browser   ; [keyword]           browser type
   :args      ; [vector of strings] driver cli arguments
   :prefs     ; [map]               'preferences' experimental option
   :headless? ; [boolean]           on/off headless mode"
  :browser)

(defmethod start-driver :chrome
  [{:keys [args prefs headless?]
    :as options}]
  (let [chrome-options (new ChromeOptions)]
    (when headless? (. chrome-options setHeadless headless?))
    (when (seq args)
      (. chrome-options addArguments args))
    (when prefs
      (. chrome-options setExperimentalOption "prefs" prefs))
    (let [chromedriver (new ChromeDriver chrome-options)]
      (config-webdriver chromedriver options)
      (init-driver chromedriver))))

(defn start-chromedriver 
  "Start chromedriver with predefined list of options;
  Also support keyword arguments for custom options
  (see (doc start-driver) for available keys)"
  [& {:keys [args prefs] :as options}]
  (let [predefined-args  ["disable-infobars"]
        predefined-prefs {}]
    (start-driver 
      (merge options
        {:browser :chrome
  	     :args    (into predefined-args args)
  	     :prefs   (merge predefined-prefs prefs)}))))

(defn quit-driver []
  (.quit *driver*))

;; navigation

(defn get-url [url]
  (.get *driver* url))

;; take screenshots

(defn take-screenshot
  "Take screenshot of the page.
  Argument is either string (path to file) or keyword denotes output type
  (available is :base64 and :bytes)"
  [output]
  (cond (string? output)
        (let [screenshot (io/file output)]
          (io/copy (.getScreenshotAs *driver* OutputType/FILE)
                   screenshot)
          screenshot)
        (= output :base64)
        (.getScreenshotAs *driver* OutputType/BASE64)
        (= output :bytes)
        (.getScreenshotAs *driver* OutputType/BYTES)
        :else
        (throw (new IllegalStateException (str "Wrong argument: " output)))))

;; find elements

(defn css [selector]
  (By/cssSelector selector))

(defn xpath [selector]
  (By/xpath selector))

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

(defn stale?
	"Test if given element does not attached to DOM"
  [element]
  (try (not (.getLocation element))
       (catch StaleElementReferenceException e true)))

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

;; control structures

(defmacro with-retry
  "Perform an action, and if any exception occurs, retry"
  [& body]
  `(try ~@body (catch Throwable ~'t ~@body)))

(defmacro foreach-element
  "Find n elements by find-expression.
  Then, n times evaluates find-expression and perform body with nth element binded.
  This function is useful when perform destructive actions over collection of elements. For example, actions with nth element invalidate n+1th element"
  [[element find-expression]
   & body]
  `(dorun
     (map (fn [~'i]
            (let [~element ((vec ~find-expression) ~'i)] 
              (do ~@body)))
       (range 0 (count ~find-expression)))))

;; utils

(defn uuid []
  (str (UUID/randomUUID)))
