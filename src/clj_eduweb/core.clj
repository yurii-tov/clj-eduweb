(ns clj-eduweb.core
  "Wrapper over Selenium API and other general-purpose stuff"
  (:import
   (org.openqa.selenium.chrome
    ChromeDriver
    ChromeOptions)
   (org.openqa.selenium.firefox
    FirefoxOptions
    FirefoxDriver)
   (org.openqa.selenium
    WebElement
    By
    OutputType
    StaleElementReferenceException)
   org.openqa.selenium.remote.RemoteWebDriver
   org.openqa.selenium.interactions.Actions
   (org.openqa.selenium.support.ui
    FluentWait
    ExpectedCondition
    ExpectedConditions)

   (java.time LocalDate
              Duration)
   (java.time.format DateTimeFormatter)
   java.util.concurrent.TimeUnit
   java.net.URL)
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [clojure.set :as cset]))


;; driver management


(declare ^:dynamic *driver*)


(defn set-driver!
  "Mutate *driver* dynamic variable"
  [driver]
  (def ^:dynamic *driver* driver))


(def driver-config (atom {}))


(defn config-driver!
  "Configure given webdriver instance, store config for further re-definition
   Recognized options:
   :implicit-wait  (implicit wait in seconds)"
  [driver config]
  (let [default-config {:implicit-wait 3}
        config (merge default-config config)]
    (.. driver
        manage
        timeouts
        (implicitlyWait (config :implicit-wait) TimeUnit/SECONDS))
    (swap! driver-config assoc driver config)))


(defmacro with-driver-config
  "Perform body with *driver* configured according to provider config, then restore previous settings
   See config-driver!"
  [config & body]
  `(let [~'old-config ((deref driver-config) *driver*)]
     (config-driver! *driver* ~config)
     (try ~@body
          (finally (config-driver! *driver* ~'old-config)))))


(defmulti start-driver
  "Start new webdriver session
   Return: WebDriver instance
   Args: hashmap
   Recognized keys:
   :browser      ; [keyword]           browser type
   :url          ; [string]            optional url to navigate on start
   :remote-url   ; [string]            url of remote selenium server
   :args         ; [vector of strings] cli switches
   :prefs        ; [map]               'preferences' experimental option
   :capabilities ; [map]               common webdriver settings
   :headless?    ; [boolean]           on/off headless mode
   :anonymous?   ; [boolean]           do not mutate global *driver* var
   :binary       ; [string]            path to browser executable"
  :browser)


(defn startup-config
  "Perform all necessary webdriver startup activities:
   - Configure given webdriver instance
   - Optionally mutate driver global variable
   - Optionally navigate to some url"
  [driver {:keys [url anonymous?] :as options}]
  (when-not anonymous? (set-driver! driver))
  (config-driver! driver options)
  (when url (.get driver url))
  driver)


(defn make-chrome-options
  "Create ChromeOptions instance based on a hashmap of options, see start-driver"
  [{:keys [args prefs capabilities headless? binary]
    :as options}]
  (let [chrome-options (new ChromeOptions)]
    ;; hacks to hide infobars
    (. chrome-options
       setExperimentalOption "useAutomationExtension" false)
    (. chrome-options
       setExperimentalOption "excludeSwitches" ["enable-automation"])
    ;; misc options
    (when binary
      (. chrome-options setBinary binary))
    (when headless? (. chrome-options setHeadless headless?))
    (when (seq args)
      (. chrome-options addArguments args))
    (when prefs
      (. chrome-options setExperimentalOption "prefs" prefs))
    (doseq [[k v] capabilities]
      (. chrome-options setCapability k v))
    chrome-options))


(defmethod start-driver :chrome
  [{:keys [remote-url] :as options}]
  (let [chrome-options (make-chrome-options options)
        chromedriver (if remote-url
                       (new RemoteWebDriver (new URL remote-url) chrome-options)
                       (new ChromeDriver chrome-options))]
    (startup-config chromedriver options)))


(defmethod start-driver :firefox
  [options]
  (let [firefox-options (new FirefoxOptions)]
    (.setCapability firefox-options "marionette" false)
    (let [firefox-driver (new FirefoxDriver firefox-options)]
      (startup-config firefox-driver options))))


(defn quit-driver []
  (swap! driver-config dissoc *driver*)
  (.quit *driver*))


;; browser control


(defn get-base-url []
  "Return base url of current page"
  (let [url (.getCurrentUrl *driver*)]
    (re-find #"\w+:/+[^/]+" url)))


(defn open-url
  "Navigate browser to given url.
  Relative urls (starting with \"/\") also supported"
  [url]
  (if (cstr/starts-with? url "/")
    (open-url (format "%s%s" (get-base-url) url))
    (.get *driver* url)))


(defmacro open-popup
  "Save current set of opened windows, then perform body (assuming new opened windows), then return set of newly opened windows"
  [& body]
  `(let [popups# (set (.. *driver* ~'getWindowHandles))]
     (do ~@body)
     (cset/difference (set (.. *driver* ~'getWindowHandles))
                      popups#)))


(defn execute-javascript [script & args]
  (.executeScript *driver* script (into-array args)))


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


;; frames navigation


(defn switch-to-frame
  "Switch to frame presented as WebElement"
  [el]
  (.. *driver* switchTo (frame el)))


(defn switch-to-parent-context
  "Navigate one nesting level up from current frame"
  []
  (.. *driver* switchTo parentFrame))


(defmacro with-frame
  "Perform actions in a context of frame, then go back to parent context, in despite of any errors"
  [frame & body]
  `(do (switch-to-frame ~frame)
       (try ~@body (finally (switch-to-parent-context)))))


;; perform actions on elements


(defn element-click [el] (. el click))


(defn element-send-keys [input text]
  (. input clear)
  (. input sendKeys (into-array [text])))


(defn element-double-click [el] (.. (new Actions *driver*) (doubleClick el) perform))


(defn element-set-css
  "Set given css property of element.
  Example:
  (element-set-css (find-element (css \"button\"))
                    \"background-color\"
                    \"green\")"
  [el prop value]
  (.executeScript *driver*
                  (format "arguments[0].style['%s'] = '%s';"
                          prop
                          value)
                  (into-array [el])))


;; get information about elements


(defn element-rect
  "Get rectangle data (coordinates and size)"
  [element]
  (let [rect (.getRect element)
        [x y] ((juxt (memfn getX)
                     (memfn getY))
               rect)
        [w h] ((juxt (memfn getWidth)
                     (memfn getHeight))
               rect)]
    {:x x :y y :width w :height h}))


(defn element-text [el] (cstr/trim (. el getText)))


(defn element-displayed? [el]
  (. el isDisplayed))


(defn element-stale?
  "Test if given element is not available in a DOM
        (i.e. request to this element will cause StaleElementReferenceException)"
  [element]
  (try (not (.getLocation element))
       (catch StaleElementReferenceException e true)))


(defn element-attribute [el attr]
  (. el getAttribute attr))


(defn element-inner-html [element]
  (. *driver* executeScript "return arguments[0].innerHTML" (into-array [element])))


(defn element-clean-html [element]
  (cstr/replace (element-inner-html element) #"(<\w+)(\s[^>]+)(>)" "$1$3"))


;; Wait for conditions


(defn wait-for
  "Wait for specific condition.
  Args:
  condition  =>  ExpectedCondition instance
  :timeout   =>  (optional) timeout in seconds (default is driver's implicit-wait value)"
  [condition & {:keys [timeout]}]
  (.. (new FluentWait *driver*)
      (withTimeout (Duration/ofSeconds
                    (or timeout ((@driver-config *driver*) :implicit-wait))))
      (until condition)))


(defmacro condition [body]
  `(proxy [ExpectedCondition] []
     (~'apply [driver#] ~body)
     (~'toString [] (str "condition: " '~body))))


(defn wait-for-stale [el & args]
  (apply wait-for (cons (ExpectedConditions/stalenessOf el) args)))


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


;; elements highlighting


(let [colored (atom {})]

  (defn remove-highlights
    "Back all elements previously colored by highlight fn, to its normal color"
    []
    (doseq [[el color] @colored]
      (when-not (element-stale? el)
        (element-set-css el "background-color" color)))
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
        (element-set-css target "background-color" "yellow")))))
