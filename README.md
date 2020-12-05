# clj-eduweb

## What is it?
Webdriver-based tool made for interaction with e5 web app through UI and exploration of ui automation in general

## Quickstart

### What you will need
1. Clojure (of course:))
2. Leiningen
3. Browser (Chrome, for example)
4. Appropriate web driver (for example, [Chromedriver](https://chromedriver.chromium.org/), installed and configured)

### Start browser and point it to some Wikipedia page
`$ cd clj-eduweb`

`$ lein repl`

```clojure
;; start-driver is a main entry point. It has a lot of options, see (doc start-driver)
clj-eduweb.repl> (start-driver {:browser :chrome
                                :headless? true
                                :url "https://wikipedia.org/wiki/Special:Random"})
```
### Inspect links on a page
```clojure
;; Two basic commands for that: find-element and find-elements
clj-eduweb.repl> (def links (find-elements (css "a")))
clj-eduweb.repl> (map element-text links)
```

### Next steps
Explore namespaces :)
Base functionality is `clj-eduweb.core` and `clj-eduweb.elements`
