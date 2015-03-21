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
   :bomb])

(defn tile-coordinates
  [tile]
  (get {:unknown [0 0]
        :flag    [0 1]
        :bomb    [0 2]
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
        tile-cols 8
        tile-rows 8]
    {:tile-w tile-w
     :tile-h tile-h
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
  [tile x y]
  (assoc (->texture tile)
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
  (if (= :bomb (board->tile board col row))
    :bomb
    (->> (reduce + (for [i (range -1 2)
                         j (range -1 2)]
                     (if (= :bomb (board->tile board (+ col i) (+ row j)))
                       1
                       0)))
         (nth tiles))))

(defn ->board
  [cols rows bombs]
  (->> (take (* cols rows)
             (concat (take bombs (repeat :bomb))
                     (repeat :blank)))
       (shuffle)
       (partition cols)
       ((fn [board]
          (for [row (range rows)]
            (for [col (range cols)]
              (mine-count board col row)))))))

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
                  tile-cols tile-rows]} dimensions
          board (->board tile-cols tile-rows 4)]

      (width! screen game-w)
      (height! screen game-h)

      (println (game :width))
      (println (game :height))

      [(for [col (range tile-cols)
             row (range tile-rows)
             :let [x (* col tile-w)
                   y (+ (* row tile-h)
                        (- game-h (* tile-h tile-rows)))
                   tile (nth (nth board row) col)]]
         (->tile tile x y))]))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-touch-down
  (fn [screen entities]
    (println "game height=" (game :height))
    (println "game width=" (game :width))
    (when-let [target (get-entity-at-cursor screen entities)]
      (println "target=" (dissoc target :object))
      (-> (remove (partial = target) entities)
          (conj (->tile (rand-nth (remove (partial = (:tile target)) tiles))
                        (:x target) (:y target))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BELOW IS THE BOARD PADDING CODE THAT I SPENT TOO MUCH TIME ON AND I'M
;; NOT READY TO JUST GET RID OF IT YET...WHAT IF I NEED IT LATER?!? ;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; board

;; (defn pad-blanks
;;   [cols tiles]
;;   (concat (take cols (repeat :blank))
;;           tiles
;;           (take cols (repeat :blank))))


;; (defn pad-board
  ;;   "Takes an MxN board and returns an (M+1)x(N+1)
  ;;   board with blank tiles around the border.

  ;;   This is necessary to make the tile counting algorithm
  ;;   work correctly for edge tiles."
;;   [board]
;;   (let [cols (count (first board))]
;;     (map (fn [row]
;;            (as-> row r
;;              (vec r)
;;              (cons :blank r)
;;              (vec r)
;;              (conj r :blank)))
;;          (->> (flatten board)
;;               (pad-blanks cols)
;;               (partition cols)))))

;; (defn generate-board
;;   [cols rows bombs]
;;   (partition cols (shuffle (take (* cols rows)
;;                                  (concat (take bombs (repeat :bomb))
;;                                          (repeat :blank))))))
;;   (->> (take (* cols rows)
;;              (concat (take bombs (repeat :bomb))
;;                      (repeat :blank)))
;;        (shuffle)
;;        (partition cols)))

;; (defn board->tile
;;      [board col row]
;;      (-> board
;;          (nth row)
;;          (nth col)))

;; (defn tile-count
;;      [board col row]
;;      (reduce  (for [i (range -1 2)
;;                     j (range -1 2)]
;;                 (if (= :bomb (board->tile board ( col i) ( row j)))
;;                   1
;;                   0))))
