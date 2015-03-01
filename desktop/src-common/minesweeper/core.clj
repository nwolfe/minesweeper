(ns minesweeper.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]
            [clojure.pprint :as pprint]))

;; Tiles 128x128

(defn get-entity-at-cursor
  [screen entities]
  (let [coords (input->screen screen (input! :get-x) (input! :get-y))]
    (find-first (fn [{:keys [x y width height] :as entity}]
                  (-> (rectangle x y width height)
                      (rectangle! :contains (:x coords) (:y coords))))
                entities)))

(def tile-coordinates
  {:block [0 0]
   :flag  [0 1]
   :bomb  [0 2]
   :blank [0 3]
   :one   [1 0]
   :two   [1 1]
   :three [1 2]
   :four  [1 3]
   :five  [2 0]
   :six   [2 1]
   :seven [2 2]
   :eight [2 3]})

(defn ->texture
  [tile]
  (let [[row col] (get tile-coordinates tile)]
    (-> (texture "minesweeper_tiles.jpg")
        (texture! :split 128 128)
        (aget row col)
        (texture))))

(defn ->tile
  [tile x y]
  (assoc (->texture tile)
         :tile tile
         :height 128 :width 128
         :x x :y y))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    ;; (label "Hello world!" (color :white))
    (let [flag (->tile :flag 0 0)]
      [flag])
    )

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-touch-down
  (fn [screen entities]
    (when-let [target (get-entity-at-cursor screen entities)]
      (pprint/pprint target)
      (-> (remove (partial = target) entities)
          (conj (->tile (rand-nth (keys (dissoc tile-coordinates
                                                (:tile target))))
                        (:x target) (:y target)))))))

(defgame minesweeper
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
