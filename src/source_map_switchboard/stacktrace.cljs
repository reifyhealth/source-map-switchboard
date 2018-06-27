(ns source-map-switchboard.stacktrace
  "Support getting a list of file-info objects from stacktrace data"
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [source-map-switchboard.core :as core]))

(defn consume-stacktrace
  "Use the string `stacktrace`, the url-pattern `server-prefix`, and the directories
  `salk-dir` and `output-dir` to determine which files on disk to inspect for source-map
  mappings (does not actually perform the inspection, or ensure files exist!).

  `stacktrace` is assumed to contain a static file reference per line, where
  `server-prefix` is used to determine the file name, via replacement and re-find.
  `salk-dir` is the directory in which all of the static files that are referenced in
  the stacktrace can be found, with `output-dir` being the cljsbuild option.

  `salk-dir` and `output-dir` are combined with each file name to identify a each file.

  This function returns a vector of hash-maps that represent where to look for
  source-mapping on local disk.

  Each hash-map contains the information:

  `:js-filepath` - string - The path of the js file to look for on disk that represents
                            the file in the stacktrace.
  `:source-map-filepath` - string - The path of the js.map file to look for on disk that
                                    represents the source-map for the file in the stacktrace.
  `:js-position` - hash-map - Contains the `:line` and `:column` from the stacktrace that index
                              into the file at `:js-filepath`, which informs what lookup is
                              needed."
  [stacktrace server-prefix project-dir output-dir]
  (let [stacktrace-lines (str/split stacktrace \newline)
        transform (comp
                   (map #(-> (str server-prefix "([a-zA-Z0-9\\_\\-\\=\\/\\.]+.js:[\\d]+:[\\d]+)")
                             re-pattern
                             (re-find %)
                             second))
                   (filter identity)
                   (map #(str project-dir output-dir %))
                   (map #(let [[js-fp line col] (str/split % ":")]
                           (-> {}
                               (assoc :js-filepath js-fp
                                      :source-map-filepath (str js-fp ".map")
                                      :dir (core/get-dir js-fp)
                                      :js-position {:line   (js/parseInt line)
                                                    :column (js/parseInt col)})))))]
    (transduce transform conj stacktrace-lines)))

(defn build-scm-cache
  "Based on the source-map-filepaths in file-infos, create a cache for source map lookups"
  [file-infos]
  (reduce
   (fn [acc source-map-filepath]
     (if (get acc source-map-filepath)
       acc
       (if-let [smc (core/get-source-map-consumer source-map-filepath)]
         (assoc acc source-map-filepath smc)
         acc)))
   {}
   (mapv :source-map-filepath file-infos)))

(defn decode
  "Traverse a stacktrace and attempt to decode the source-mappings for each item in the call stack"
  [stacktrace server-prefix project-dir output-dir]
  (let [file-infos (consume-stacktrace
                    stacktrace
                    server-prefix
                    project-dir
                    output-dir)
        scm-cache  (build-scm-cache file-infos)]
    (pprint file-infos)
    (->> (mapv (partial core/prepare scm-cache) file-infos)
         (mapv core/decode))))
