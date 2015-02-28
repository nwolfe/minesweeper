(defproject minesweeper "0.0.1-SNAPSHOT"
  :description "FIXME: write description"

  :dependencies [[com.badlogicgames.gdx/gdx "1.5.3"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.5.3"]
                 [com.badlogicgames.gdx/gdx-box2d "1.5.3"]
                 [com.badlogicgames.gdx/gdx-box2d-platform "1.5.3"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-bullet "1.5.3"]
                 [com.badlogicgames.gdx/gdx-bullet-platform "1.5.3"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-platform "1.5.3"
                  :classifier "natives-desktop"]
                 [org.clojure/clojure "1.6.0"]
                 [play-clj "0.4.4"]]

  :profiles {:dev {:dependencies [[org.clojure/tools.nrepl "0.2.7"]]}}

  :source-paths ["src" "src-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [minesweeper.core.desktop-launcher]
  :main minesweeper.core.desktop-launcher)
