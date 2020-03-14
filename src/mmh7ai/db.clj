(ns mmh7ai.db
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [datascript.core :as d]
   [mmh7ai.scrap :refer [scrap-db]]))

;;
;; On disk
;;

(def db-file "db/data.edn")

(defn- write-db-file! []
  (println "Start scraping db")
  (let [db (scrap-db)]
    (with-open [w (io/writer db-file)]
      (binding [*print-length* false
                *out* w]
        (pprint/write db))
      (println "Wrote data to:" db-file))))

(defn- read-db-file []
  (with-open [r (java.io.PushbackReader. (io/reader db-file))]
    (binding [*read-eval* false]
      (read r))))

;;
;; In memory
;;

(def ^:dynamic *conn* nil)

(defn init-db! []
  (when (or (nil? *conn*) (not (.exists (io/file db-file))))
    (when (not (.exists (io/file db-file)))
      (write-db-file!))
    (let [schema {}
          conn (d/create-conn schema)]
      (d/transact!
       conn (mapcat (fn [[_faction data]] (:units data)) (read-db-file)))
      (alter-var-root #'*conn* (constantly conn))))
  :done)

(defn query [query]
  (d/q query @*conn*))
