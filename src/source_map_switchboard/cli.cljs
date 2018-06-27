(ns source-map-switchboard.cli
  "A command-line-interface to make source-map lookup operations simple"
  (:require [cljs.nodejs :as nodejs]
            [source-map-switchboard.single :as single]))

(defn -main
  [& args]
  (let [command (keyword (first args))]
    (cond
      (= :single command)
      (single/-main (second args))

      :else
      (println "Help message will soon be available"))))

(nodejs/enable-util-print!)
(set! *main-cli-fn* -main)
