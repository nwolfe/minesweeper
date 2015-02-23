(ns minesweeper.core.desktop-launcher
  (:require [minesweeper.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. minesweeper "minesweeper" 800 600)
  (Keyboard/enableRepeatEvents true))
