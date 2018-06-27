(require '[lumo.build.api :as b])

(b/build
 (b/inputs "src")
 {:output-to "main.js"
  :main "source-map-switchboard.cli"
  :optimizations :none
  :target :nodejs
  :source-map true})
