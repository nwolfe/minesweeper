(ns minesweeper.core.desktop-launcher
  (:require [minesweeper.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. minesweeper-game "minesweeper"
                     (/ (:game-w dimensions) 2)
                     (/ (:game-h dimensions) 2))
  (Keyboard/enableRepeatEvents true))
