(ns mmh7ai.scrap
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [net.cgrand.enlive-html :as html]
   [clj-http.client :as client]
   [cheshire.core :as json]))

(def wikia "http://mightandmagic.wikia.com")

(def urls
  {:factions
   {:academy    (str wikia "/wiki/Academy_(H7)")
    :dungeon    (str wikia "/wiki/Dungeon_(H7)")
    :fortress   (str wikia "/wiki/Fortress_(H7)")
    :haven      (str wikia "/wiki/Haven_(H7)")
    :necropolis (str wikia "/wiki/Necropolis_(H7)")
    :stronghold (str wikia "/wiki/Stronghold_(H7)")
    :sylvan     (str wikia "/wiki/Sylvan_(H7)")}})

(defmulti scrap
  (fn [type url] type))

(defn resp->html [resp]
  (-> (:body resp)
      (java.io.StringReader.)
      (html/html-resource)))

(defn ->kw [s]
  (-> s str/trim str/lower-case (str/replace #"[ _]" "-") keyword))

(defn partition-by-tag [tag html-coll]
  (->> html-coll
       (drop-while #(not= tag (:tag %)))
       (partition-by #(= tag (:tag %)))
       (partition 2)
       (map #(apply concat %))))

;;
;; Units
;;

(defn tr-unit-stats [tr]
  (let [info (-> tr :content second :content second html/text ->kw)]
    {info
     (condp contains? info
       #{:faction}
       (-> tr :content (nth 2) :content second html/text ->kw)
       #{:tier :movement :size :range}
       (-> tr :content (nth 2) html/text ->kw)
       #{:upgraded}
       (-> tr :content (nth 2) :content second :attrs :alt (= "Yes"))
       #{:hit-points :attack :defense :initiative :speed :morale :destiny :growth-per-week}
       (-> tr :content (nth 2) html/text str/trim Integer/parseInt)
       #{:cost-per-unit}
       (-> tr :content (nth 2) :content
           (->> (filter #(not= :br (:tag %)))
                (partition 2)
                (map (fn [[amount resource]]
                       {(-> resource :attrs :href (str/split #"/") last ->kw)
                        (-> amount html/text str/trim Integer/parseInt)}))
                (apply merge)))
       #{:damage}
       (-> tr :content (nth 2) html/text str/trim (str/split #"-")
           (->> (mapv #(Integer/parseInt %))))
       nil)}))

(defn tr-stats? [html-elem]
  (and (= :tr (:tag html-elem))
       (= 2 (count (html/select html-elem [:td])))))

(defmethod scrap :units [_ url]
  (->> (html/select
        (-> url client/get resp->html)
        [:div#mw-content-text :table])
       first
       :content
       (filter tr-stats?)
       (map tr-unit-stats)
       (apply merge)))

;;
;; Classes
;;

(defn h-content [h]
  (-> h :content first html/text))

(defmethod scrap :classes [_ url]
  (->> (html/select
        (-> url client/get resp->html)
        [:div#mw-content-text])
       first
       :content
       (partition-by-tag :h2)
       (map
        (fn [h2-group]
          (let [category (-> h2-group first h-content ->kw)]
            (case category
              :abilities
              (->> (-> h2-group (html/select [:table :tr]))
                   (partition 2)
                   (map
                    (fn [[level skills]]
                      {(-> (html/select level [:th])
                           first
                           html/text
                           (-> (str/split #" ") first ->kw))
                       (->> (html/select skills [:td [:a (html/attr? :title)]])
                            (map #(-> % html/text ->kw))
                            (into #{}))}))
                   (apply merge)
                   (hash-map :abilities))
              nil))))
       (apply merge)))

;;
;; Factions
;;

(defn ul-data [html-coll]
  (->> html-coll
       (filter #(= :ul (:tag %)))
       (mapcat
        (fn [li]
          (map (fn [elem]
                 {:name (html/text elem)
                  :url (str wikia (-> elem :attrs :href))})
               (html/select li [:a]))))))

(defn map-scrap [type data-coll]
  (map
   (fn [data]
     (-> data
         (merge (scrap type (:url data)))
         (dissoc :url)))
   data-coll))

(defmethod scrap :factions [_ url]
  (println url)
  (->> (html/select
        (-> url client/get resp->html)
        [:div#mw-content-text])
       first
       :content
       (partition-by-tag :h2)
       (map
        (fn [h2-group]
          (let [category (-> h2-group first h-content ->kw)]
            (case category
              :units
              (->> h2-group
                   (partition-by-tag :h3)
                   (mapcat
                    (fn [h3-group]
                      (let [tier (-> h3-group first h-content ->kw)]
                        (when (#{:core :elite :champion} tier)
                          (->> (ul-data h3-group)
                               (map-scrap :units))))))
                   vec
                   (hash-map :units))
              :classes
              (->> h2-group
                   (partition-by-tag :h3)
                   (mapcat
                    (fn [h3-group]
                      (let [class (-> h3-group first h-content ->kw)]
                        (->> (ul-data h3-group)
                             (map #(assoc % :class class))
                             (map-scrap :classes)))))
                   vec
                   (hash-map :classes))
              nil))))
       (apply merge)))

(defn scrap-db []
  (->> (:factions urls)
       (map (fn [[faction url]]
              {faction (scrap :factions url)}))
       (apply merge)))
