(ns clj-eduweb.core
  "Wrapper over Selenium API and other general-purpose stuff"
  (:import
    ; selenium
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

    ; java.time
    (java.time LocalDate
               Duration)
    (java.time.format DateTimeFormatter)
    
    ; java.util
    java.util.concurrent.TimeUnit
    java.util.UUID)
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [clojure.set :as cset]))

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
  "Configure webdriver instance.
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
   :browser      ; [keyword]           browser type
   :args         ; [vector of strings] cli switches
   :prefs        ; [map]               'preferences' experimental option
   :capabilities ; [map]               common webdriver settings
   :headless?    ; [boolean]           on/off headless mode"
  :browser)

(defmethod start-driver :chrome
  [{:keys [args prefs capabilities headless?]
    :as options}]
  (let [chrome-options (new ChromeOptions)]
    (when headless? (. chrome-options setHeadless headless?))
    (when (seq args)
      (. chrome-options addArguments args))
    (when prefs
      (. chrome-options setExperimentalOption "prefs" prefs))
    (doseq [[k v] capabilities]
      (. chrome-options setCapability k v))
    (let [chromedriver (new ChromeDriver chrome-options)]
      (config-webdriver chromedriver options)
      (init-driver chromedriver))))

(defn start-chromedriver 
  "Start chromedriver with predefined list of options;
  Also support keyword arguments for custom options
  (see (doc start-driver) for available keys)"
  [& {:keys [args] :as options}]
  (let [predefined-args ["disable-infobars"]]
    (start-driver 
      (merge options
        {:browser :chrome
  	     :args    (into predefined-args args)}))))

(defn quit-driver []
  (.quit *driver*))

;; navigation

(defn get-url [url]
  (.get *driver* url))

(defmacro open-popup
  "Save current set of opened windows, then perform body (assuming new opened windows), then return set of newly opened windows"
  [& body]
  `(let [popups# (set (.. *driver* ~'getWindowHandles))]
     (do ~@body)
     (cset/difference (set (.. *driver* ~'getWindowHandles))
                      popups#)))

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

;; get information about elements

(defn get-text [el] (cstr/trim (. el getText)))

(defn displayed? [el]
  (. el isDisplayed))

(defn stale?
	"Test if given element is not available in a DOM
	(i.e. request to this element will cause StaleElementReferenceException)"
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

(defn date-strings
   "Generate sequence of date strings starting from given date (now by default),
   with provided step interval, and formatting by given pattern (dd.MM.yy by default)"
   [step & {:keys [date
                   format-pattern]
            :or {date (LocalDate/now)
                 format-pattern "dd.MM.yy"}}]
   (map (fn [d] (.format d (DateTimeFormatter/ofPattern format-pattern)))
        (iterate (fn [d] (.plusDays d step)) date)))

(defn set-css-property
  "Set given css property of element.
  Example:
  (set-css-property (find-element (css \"button\"))
                    \"background-color\"
                    \"green\")"
  [el prop value]
  (.executeScript *driver*
                  (format "arguments[0].style['%s'] = '%s';"
                          prop
                          value)
                  (into-array [el])))

;; elements highlighting

(let [colored (atom {})]

	(defn remove-highlights
		"Back all elements previously colored by highlight fn, to its normal color"
	  []
	  (doseq [[el color] @colored]
      (when-not (stale? el)
        (set-css-property el "background-color" color)))
      (reset! colored {}))

  (defn highlight
    "Highlight given element (or collection of elements) with bright color.
    Before that, remove old marks (by default)"
    [target & {:keys [remove-old-marks] 
    	         :or {remove-old-marks true}}]
    (when remove-old-marks
      (remove-highlights))
    (if (coll? target)
      (dorun (map (fn [el] (highlight el :remove-old-marks false))
                  target))
      (let [old-color (.getCssValue target "background-color")]
        (swap! colored
               (fn [m] (assoc m target old-color)))
        (set-css-property target "background-color" "yellow")))))
