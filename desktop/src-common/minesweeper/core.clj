(ns minesweeper.core
  (:require [clojure.pprint :refer :all]
            [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]))

;; Tiles 128x128
;; Board 8x8

(def ^:const pixels-per-tile 32)

(def tiles
  [:blank
   :one
   :two
   :three
   :four
   :five
   :six
   :seven
   :eight
   :unknown
   :flag
   :mine])

(defn tile-coordinates
  [tile]
  (get {:unknown [0 0]
        :flag    [0 1]
        :mine    [0 2]
        :blank   [0 3]
        :one     [1 0]
        :two     [1 1]
        :three   [1 2]
        :four    [1 3]
        :five    [2 0]
        :six     [2 1]
        :seven   [2 2]
        :eight   [2 3]}
       tile))

(def dimensions
  (let [tile-w 128
        tile-h 128
        tile-cols 20
        tile-rows 12
        mines 40]
    {:tile-w tile-w
     :tile-h tile-h
     :mines mines
     :tile-cols tile-cols
     :tile-rows tile-rows
     :game-w (* tile-w tile-cols)
     :game-h (* tile-h tile-rows)}))

(defn ->texture
  [tile]
  (let [[row col] (tile-coordinates tile)]
    (-> (texture "minesweeper_tiles.jpg")
        (texture! :split (:tile-w dimensions) (:tile-w dimensions))
        (aget row col)
        (texture))))

(defn ->tile
  [texture tile x y]
  (assoc texture
         :tile tile
         :height (:tile-h dimensions)
         :width (:tile-w dimensions)
         :x x
         :y y))

(defn get-entity-at-cursor
  [screen entities]
  (let [coords (input->screen screen (input! :get-x) (input! :get-y))]
    (find-first (fn [{:keys [x y width height] :as entity}]
                  (-> (rectangle x y width height)
                      (rectangle! :contains (:x coords) (:y coords))))
                entities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; board

(defn board->tile
  [board col row]
  (if (and (>= col 0) (>= row 0)
           (< col (count (first board))) (< row (count board)))
    (-> board
        (nth row)
        (nth col))))

(defn mine-count
  [board col row]
  (if (= :mine (board->tile board col row))
    :mine
    (->> (reduce + (for [i (range -1 2)
                         j (range -1 2)]
                     (if (= :mine (board->tile board (+ col i) (+ row j)))
                       1
                       0)))
         (nth tiles))))

(defn ->board
  [cols rows mines]
  (->> (take (* cols rows)
             (concat (take mines (repeat :mine))
                     (repeat :blank)))
       (shuffle)
       (partition cols)
       ((fn [board]
          (for [row (range rows)]
            (for [col (range cols)]
              (mine-count board col row)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; game screens

;; On click, Reveal tile
;; When a tile is Revealed:
;;  * change texture
;;  * change unkown? flag
;;  * and if it's:
;;    - bomb then you lose
;;    - number then nothing more happens
;;    - blank then Reveal all adjacent tiles

(defn reveal-tile
  [tile]
  (assoc tile
         :object (-> tile :tile ->texture :object)
         :unknown? false))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :camera (orthographic)
                          :renderer (stage)
                          :world (box-2d 0 0))
          {:keys [game-w game-h
                  tile-w tile-h
                  tile-cols tile-rows
                  mines]} dimensions
          board (->board tile-cols tile-rows mines)]

      (width! screen game-w)
      (height! screen game-h)

      [(for [col (range tile-cols)
             row (range tile-rows)
             :let [x (* col tile-w)
                   y (+ (* row tile-h)
                        (- game-h (* tile-h tile-rows)))
                   tile (nth (nth board row) col)]]
         (-> (->texture :unknown)
             (->tile tile x y)
             (assoc :unknown? true)))]))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-touch-down
  (fn [screen entities]
    (when-let [target (get-entity-at-cursor screen entities)]
      (if (:unknown? target)
        (map (fn [entity]
               (if (= entity target)
                 (reveal-tile entity)
                 entity))
             entities))))

  :on-resize
  (fn [screen entities]
    (height! screen (:game-h dimensions))
    (width! screen (:game-w dimensions))))

(defgame minesweeper-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!)))

(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                            (catch Exception e
                              (.printStackTrace e)
                              (set-screen! minesweeper-game blank-screen)))))
