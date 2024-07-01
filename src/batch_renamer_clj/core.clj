;(set! *warn-on-reflection* true)
(ns batch-renamer-clj.core
  (:import [java.io File])
  (:require [clojure.string :as string])
  (:require [exif-processor.core])
  (:gen-class))

(def rgx
  {:path  #".*\/(?=\w*\.jpg$)"
   :2dirs #"\w*\/\w*\/(?=\w*\.jpg$)"
   :file #"\w*\.jpg"})

(defn new-name [path i]
  (string/replace
   path
   (:path rgx)
   (str (string/replace (or (re-find (:2dirs rgx) path) "") "/" "") i ".jpg")))

(defn temp-name [path i] (string/replace path (:file rgx) (str "TEMP" i ".jpg")))

(defn generate-names [files]
  (loop [i 0 files files [temps news] []]
    (if (empty? files)
      (map reverse [temps news])
      (recur
       (+ i 1)
       (drop 1 files)
       (let [path (.getAbsolutePath (first files))]
         [(conj temps (File. (temp-name path i)))
          (conj news (File. (new-name path i)))])))))

(defn extract-exif-date [f]
  (or
   (try (get (exif-processor.core/exif-for-file f) "Date/Time Digitized")
        (catch Exception _e nil))
   "0"))

(defn get-files [dir]
  (when (.isDirectory dir)
    (filter #(string/ends-with? % ".jpg") (seq (.listFiles dir)))))

(defn get-paths [dir]
  (let [files (sort-by #(extract-exif-date %) (get-files dir)) [temps news] (generate-names files)]
    [files  temps news]))

(defn get-dirs [dir]
  (loop [dirs [dir] result []]
    (if (empty? dirs)
      result
      (let [this-dir (first dirs)]
        (recur
         (concat (rest dirs) (filter #(.isDirectory %) (.listFiles this-dir)))
         (conj result this-dir))))))

(defn get-new-names [dirs]
  (loop [dirs dirs [rsorted rtemps rnews] []]
    (if (empty? dirs)
      [rsorted rtemps rnews]
      (let [[sorted temps news] (get-paths (first dirs))]
        (recur
         (drop 1 dirs)
         [(concat sorted rsorted)
          (concat temps rtemps)
          (concat news rnews)])))))
    ;(.renameTo temp new))

(defn rename-files [[froms temps news]]
  (doseq [[from temp] (map vector froms temps)]
    (println (.getPath from) " >> " (.getPath temp)))
    ;(.renameTo from temp)
  (doseq [[temp new] (map vector temps news)]
    (println (.getPath temp) " >> " (.getPath new))))


(defn -main
  [& args]
  (let [root (File. (first args))]
    (if (.exists root)
      (-> root (get-dirs) (get-new-names) (rename-files))
      (println (.getAbsolutePath root) " exist. doesnt lol"))))