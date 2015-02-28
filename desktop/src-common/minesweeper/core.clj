(ns minesweeper.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]))


;; Tiles 128x128

(defn get-entity-at-cursor
  [screen entities]
  (let [coords (input->screen screen (input! :get-x) (input! :get-y))]
    (find-first (fn [{:keys [x y width height] :as entity}]
                  (-> (rectangle x y width height)
                      (rectangle! :contains (:x coords) (:y coords))))
                entities)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    ;; (label "Hello world!" (color :white))
    (let [sheet (texture "minesweeper_tiles.jpg")
          tiles (texture! sheet :split 128 128)]
      (assoc (texture (aget tiles 1 2))
             :height 128
             :width 128
             :x 0
             :y 0)
      ))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-touch-down
  (fn [screen entities]
    (let [target (get-entity-at-cursor screen entities)]
      (clojure.pprint/pprint target)
      )
    entities))

(defgame minesweeper
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
