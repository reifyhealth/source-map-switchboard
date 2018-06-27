(ns source-map-switchboard.single
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [source-map-switchboard.core :as core]))

(defn -main
  "Performs a single source-map lookup, using a string in the form <filepath>:<line>:<col>"
  [& args]
  (if-let [s (first args)]
    (let [[filepath line column] (str/split s ":")
          source-map-filepath    (str filepath ".map")
          file-info              {:js-filepath         filepath
                                  :source-map-filepath source-map-filepath
                                  :dir                 (core/get-dir filepath)
                                  :js-position         {:line   (js/parseInt line)
                                                        :column (js/parseInt column)}}
          smc-cache              {source-map-filepath (core/get-source-map-consumer source-map-filepath)}]
      (->> file-info
           (core/prepare smc-cache)
           core/decode
           pprint))
    (println "Enter a string in the form <filepath>:<line>:<col> to perform a single source-map lookup")))
