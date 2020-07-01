(ns clj-eduweb.qti-test
  "Testing passing of various interaction types on qti server"
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-eduweb.core :refer :all]
            [clj-eduweb.qti :refer :all]))


(use-fixtures
  :once (fn [t]
          (binding [*driver*
                    (start-driver {:browser :chrome
                                   :headless? true
                                   :anonymous? true})]
            (t)
            (quit-driver))))


(defn test-question [description url]
  (open-url url)
  (is (= (:type (solve)) :success)
      description))


(deftest qti-examples-test-0
  (test-question
   "samples/caspian/caspian.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/caspian/caspian.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-1
  (test-question
   "samples/mexicanPresident/mexicanPresident.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/mexicanPresident/mexicanPresident.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-2
  (test-question
   "samples/water/water.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/water/water.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-3
  (test-question
   "samples/grain/grain.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/grain/grain.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-4
  (test-question
   "samples/trigonometry/trigonometry.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/trigonometry/trigonometry.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-5
  (test-question
   "samples/cell/cell.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/cell/cell.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-6
  (test-question
   "samples/vinegar/vinegar.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/vinegar/vinegar.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-7
  (test-question
   "samples/alkane/alkane.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/alkane/alkane.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-8
  (test-question
   "samples/suffixes/suffixes.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/suffixes/suffixes.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-9
  (test-question
   "samples/area/area.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/area/area.xml&showSolution=true&showFeedback=true&maxAttempts=2"))


(deftest qti-examples-test-10
  (test-question
   "samples/lcm/lcm.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/lcm/lcm.xml&showSolution=true&showFeedback=true&maxAttempts=2"))


(deftest qti-examples-test-11
  (test-question
   "samples/inlineTextEntry/inlineTextEntry.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/inlineTextEntry/inlineTextEntry.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-12
  (test-question
   "samples/frac/frac.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/frac/frac.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-14
  (test-question
   "samples/olympic/olympic.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/olympic/olympic.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-15
  (test-question
   "samples/microscopeChoice/microscopeChoice.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/microscopeChoice/microscopeChoice.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-18
  (test-question
   "samples/airportTags/airportTags.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/airportTags/airportTags.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-19
  (test-question
   "samples/microscope/microscope.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/microscope/microscope.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-20
  (test-question
   "samples/circles/circles.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/circles/circles.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-21
  (test-question
   "samples/leaves/leaves.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/leaves/leaves.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-22
  (test-question
   "samples/orderedContainer/orderedContainer.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/orderedContainer/orderedContainer.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-23
  (test-question
   "samples/continents/continents.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/continents/continents.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-24
  (test-question
   "samples/estates/estates.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/estates/estates.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-25
  (test-question
   "samples/langs/langs.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/langs/langs.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-26
  (test-question
   "samples/cat/cat.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/cat/cat.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-27
  (test-question
   "samples/anyContainer/anyContainer.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/anyContainer/anyContainer.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-28
  (test-question
   "samples/verbSuffixes/verbSuffixes.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/verbSuffixes/verbSuffixes.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-29
  (test-question
   "samples/bahrain/bahrain.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/bahrain/bahrain.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-30
  (test-question
   "samples/chessmen/chessmen.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/chessmen/chessmen.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-31
  (test-question
   "samples/formulae/formulae.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/formulae/formulae.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-32
  (test-question
   "samples/polymers/polymers.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/polymers/polymers.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-33
  (test-question
   "samples/db/db.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/db/db.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-34
  (test-question
   "samples/particleProps/particleProps.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/particleProps/particleProps.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-35
  (test-question
   "samples/sentenceError/sentenceError.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/sentenceError/sentenceError.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-38
  (test-question
   "samples/mathjax/mathjax-square/mathjax-square.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/mathjax/mathjax-square/mathjax-square.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-39
  (test-question
   "samples/mathjax/mathjax-mathml/mathjax-mathml.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/mathjax/mathjax-mathml/mathjax-mathml.xml&showSolution=true&showFeedback=true&maxAttempts=3"))


(deftest qti-examples-test-40
  (test-question
   "samples/mathjax/mathjax-tex/mathjax-tex.xml"
   "http://185.12.155.35:8080/QtiPlayer.html?path=samples/mathjax/mathjax-tex/mathjax-tex.xml&showSolution=true&showFeedback=true&maxAttempts=3"))
