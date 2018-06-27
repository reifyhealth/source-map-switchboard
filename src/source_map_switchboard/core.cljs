(ns source-map-switchboard.core
  "Prepare file-info data to decode source-maps"
  (:require [clojure.string :as str]
            [goog.object :as go]))

(def colors (js/require "colors"))
(def fs (js/require "fs"))
(def source-map (js/require "source-map"))
(def SourceMapConsumer (go/get source-map "SourceMapConsumer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; source-map interop helpers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-source-map-consumer
  "Attempt to create a SourceMapConsumer with the given `source-map-filepath`.
  Returns nil if the source-map-filepath can't be read."
  [source-map-filepath]
  (try
    (->> source-map-filepath
         (.readFileSync fs)
         (.parse js/JSON)
         (SourceMapConsumer.))
    (catch :default e
      nil)))

(defn original-position-for
  [smc m]
  (.originalPositionFor smc (clj->js m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file read interop helpers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sync-read
  "Perform a buffered read, providing incremental chunks to the caller as the file is processed.
  `buffer-size` can be used to control the size of each chunk, defaulting to the file's blksize stat."
  [{:keys [filepath buffer-size buffer-to-string?]
    :or {buffer-to-string? true}}
   on-buffer-read-fn
   & args]
  (let [stat         (.statSync fs filepath)
        file-size    (go/get stat "size")
        block-size   (go/get stat "blksize")
        fd           (.openSync fs filepath "r")
        buffer-size  (or buffer-size block-size)
        b            (.alloc js/Buffer. buffer-size)
        buffer-tx-fn (if buffer-to-string? #(.toString %) identity)]
    (loop [start-position 0]
      (let [remaining (- file-size start-position)
            remaining (if (pos? remaining) remaining 0)
            bs        (if (>= remaining buffer-size)
                        buffer-size
                        remaining)]
        (when (pos? remaining)
          ;; Fill the buffer
          (.readSync fs fd b 0 bs start-position)
          ;; Read from the buffer.
          ;; NOTE: The .slice is very important wrt the last read where it might not be full!
          (when (apply on-buffer-read-fn
                       (-> b
                           (.slice 0 bs)
                           buffer-tx-fn)
                       args)
            (recur (+ start-position buffer-size))))))))

(defn line-keys
  [line lines-before lines-after]
  (->> (range (- line lines-before 1)
              (+ line lines-after))
       (into [])
       (filterv nat-int?)
       (into #{})))

(defn read-file-content
  "Read an entire file into memory and parse out the lines that are relevant. Trades speed for memory.

  Returns a hash-map where the keys are the line numbers and the values are the lines (including newlines)."
  [{:keys [filepath
           line
           lines-before lines-after]}]
  (let [lines (-> (.readFileSync fs filepath) (str/split "\n"))
        lks (line-keys line lines-before lines-after)]
    (->> lines
         (map-indexed (fn [idx itm]
                        (if (lks idx)
                          {(inc idx) (str itm "\n")}
                          nil)))
         (filterv some?)
         (into {}))))

(defn buffered-read-file-content
  "Do a buffered read of filepath from [(- line lines-before) (+ line lines-after)]

  Returns a hash-map where the keys are the line numbers and the values are the lines (including newlines)."
  [{:keys [filepath
           line
           lines-before lines-after]}]
  (let [state (atom {:line-buffer ""
                     :line-number 1
                     :line-map    {}
                     :line-keys   (line-keys line lines-before lines-after)})]
    (sync-read
     {:filepath    filepath}
     (fn [s state]
       (let [{:keys [line-keys] :as prev} @state

             {:keys [line-map] :as next}
             (reduce
              (fn [acc cur]
                (cond-> acc
                  ;; take a built up line and store it, prepare for next line
                  (= cur "\n")
                  (-> (assoc-in [:line-map (:line-number acc)] (str (:line-buffer acc) cur))
                      (update :line-number inc)
                      (assoc :line-buffer ""))

                  ;; continue to build up the current line
                  (not= cur "\n")
                  (update :line-buffer str cur)

                  ;; only keep lines that matter
                  true
                  (update :line-map select-keys line-keys)))
              prev
              s)

             ;; if we have all that we needed, no reason to keep reading more
             finished? (some->
                        (get line-map (+ line lines-after))
                        (str/ends-with? "\n"))]

         (reset! state next)
         (not finished?)))
     state)
    (-> state deref :line-map)))

(defn format-file-content
  "Create the formatted text for file content and information about where it came from"
  []
  nil)

(defn display-content
  "Actually print the content for the user"
  []
  nil)

(defn get-dir
  "Return the directory part of a filepath"
  [filepath]
  (-> filepath
      (str/split "/")
      butlast
      (->> (str/join "/"))))

(defn prepare
  "Figure out the `original-filepath` and `original-position` using `file-info`"
  [scm-cache {:keys [source-map-filepath js-position dir] :as file-info}]
  (if-let [smc (get scm-cache source-map-filepath)]
    (let [{:keys [source] :as op} (js->clj (original-position-for smc js-position) :keywordize-keys true)]
      (if source
        (let [ofp (str dir "/" (str/replace source #"\?[0-9a-zA-Z\*\=]+$" ""))]
          (assoc file-info
                 :original-position op
                 :original-filepath ofp))
        (assoc file-info ::issue :original-source-map-lookup-nil)))
    (assoc file-info ::issue :source-map-dne)))

(defn decode
  "Read the original file to get the lines of context around the original-position"
  [{:keys [original-filepath original-position] :as file-info}]
  (if original-filepath
    (let [{:keys [line column]} original-position]
      (assoc file-info :original-file-content (read-file-content {:filepath     original-filepath
                                                                  :line         line
                                                                  :column       column
                                                                  :lines-before 5
                                                                  :lines-after  5})))
    file-info))
