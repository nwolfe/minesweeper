(ns minesweeper.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]))

;; Tiles 128x128

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    ;; (label "Hello world!" (color :white))
    (let [sheet (texture "minesweeper_tiles.jpg")
          tiles (texture! sheet :split 128 128)]
      (texture (aget tiles 1 2))
      ))

  :on-wtf-man
  (fn [s v])

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-touch-down
  (fn [screen entities]
    (println "x =" (game :x))
    (println "y =" (game :y))
    ))

(defgame minesweeper
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
