(ns minesweeper.core
  (:require [clojure.pprint :refer :all]
            [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]))

;; Tiles 128x128
;; Board 8x8

(def ^:const pixels-per-tile 32)

(declare minesweeper-game main-screen)

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
  [texture tile x y row col]
  (assoc texture
         :tile tile
         :height (:tile-h dimensions)
         :width (:tile-w dimensions)
         :x x
         :y y
         :row row
         :col col))

(defn entity-at-point
  [point entities]
  (find-first (fn [{:keys [x y width height] :as entity}]
                (-> (rectangle x y width height)
                    (rectangle! :contains (:x point) (:y point))))
              entities))

(defn get-entity-at-cursor
  [screen entities]
  (-> screen
      (input->screen (input! :get-x) (input! :get-y))
      (entity-at-point entities)))

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
;;;; tile reveal

(defn adjacent-tiles
  [entities tile]
  (let [y-top    (+ (:y tile) (:height tile) 5)
        y-mid    (+ (:y tile) 5)
        y-bot    (- (:y tile) 5)
        x-left   (- (:x tile) 5)
        x-center (+ (:x tile) 5)
        x-right  (+ (:x tile) (:width tile) 5)]
    (into #{} [(entity-at-point {:y y-top :x x-left} entities)
               (entity-at-point {:y y-top :x x-center} entities)
               (entity-at-point {:y y-top :x x-right} entities)
               (entity-at-point {:y y-mid :x x-left} entities)
               (entity-at-point {:y y-mid :x x-right} entities)
               (entity-at-point {:y y-bot :x x-left} entities)
               (entity-at-point {:y y-bot :x x-center} entities)
               (entity-at-point {:y y-bot :x x-right} entities)])))

(defn blank?
  [tile]
  (= :blank (:tile tile)))

(defn flood-reveal
  [tile entities]
  (loop [reveal #{tile}
         queue [tile]]
    (if (empty? queue)
      reveal
      (let [adjacent (adjacent-tiles entities (first queue))]
        (recur (into reveal adjacent)
               (into (vec (rest queue))
                     (->> adjacent
                          (filter blank?)
                          (remove reveal))))))))

(defn find-revealed
  [tile entities]
  (if (blank? tile)
    (flood-reveal tile entities)
    #{tile}))

(defn reveal-image
  [entity]
  (assoc entity :object (-> entity :tile ->texture :object)))

(defn mark-revealed
  [entity]
  (assoc entity :unknown? false))

(defn reveal-tile
  [tile entities]
  (let [revealed (find-revealed tile entities)]
    (map (fn [entity]
           (if (contains? revealed entity)
             (-> entity reveal-image mark-revealed)
             entity))
         entities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; game screens

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
             (->tile tile x y row col)
             (assoc :unknown? true)))]))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-touch-down
  (fn [screen entities]
    (when-let [target (get-entity-at-cursor screen entities)]
      (if (= :mine (:tile target))
        (set-screen! minesweeper-game main-screen))
      (if (:unknown? target)
        (reveal-tile target entities))))

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
