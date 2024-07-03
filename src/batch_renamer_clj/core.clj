;(set! *warn-on-reflection* true)
(ns batch-renamer-clj.core
  (:import [java.io File])
  (:require [clojure.string :as string])
  (:require [exif-processor.core])
  (:gen-class))

(def rgx
  {:2dirs #"\w*\/\w*\/(?=\w*\.jpg$)"
   :file #"\w*\.jpg"})

(defn extract-exif-date [f]
  (or
   (try (get (exif-processor.core/exif-for-file f) "Date/Time Digitized")
        (catch Exception _e nil))
   "0"))

(defn get-files [dir]
  (loop [dirs [dir] files []]
    (if (empty? dirs)
      files
      (let [dir-ls (->> (first dirs) (.listFiles) (group-by #(.isDirectory %)))]
        (recur
         (concat (rest dirs) (dir-ls true))
         (conj files (dir-ls false)))))))

(defn get-names [files]
  (for [files files]
    (let [sorted 
          (->> files (filter #(-> % (string/ends-with? ".jpg"))) (sort-by #(extract-exif-date %)))
          [temps news]
          (map
           (fn [func] (map-indexed #(File. (func %1 %2)) (map #(.getAbsolutePath %) sorted)))
           [#(string/replace %2 (:file rgx) (str "TEMP" %1 ".jpg"))
            #(string/replace
              %2
              (:file rgx)
              (str (string/replace (or (re-find (:2dirs rgx) %2) "") "/" "") %1 ".jpg"))])]
      [sorted temps news])))

(defn rename-files [dirs]
  (doseq [[froms temps news] dirs]
    (doseq [[from temp] (map list froms temps)]
      (println (.getPath from) " >> " (.getPath temp))
    ;(.renameTo from temp)
      )
    (doseq [[temp new] (map list temps news)]
      (println (.getPath temp) " >> " (.getPath new))
    ;(.renameTo temp new)
      )))

(defn -main
  [& args]
  (let [root (File. (first args))]
    (if (.exists root)
      (-> root (get-files) (get-names) (rename-files))
      (println (.getAbsolutePath root) " exist. doesnt lol"))))
