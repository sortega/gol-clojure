(ns gol.gui
  (:use [gol.model])
  (:use [gol.grid])
  (:use [seesaw.core])
  (:use [seesaw.color])
  (:use [seesaw.graphics])
  (:use [seesaw.dev]))

(defn mainframe
  "Builds the main app frame."
  []
  (let [world-widget (canvas :id :canvas
                             :background "#FFF")
        run-btn  (button :text "run" :id :run)
        step-btn (button :text "step" :id :step)
        controls (horizontal-panel :items [run-btn step-btn])]
    (frame :title "Game of life simulator"
      :content (border-panel :center world-widget
                 :south controls)
      :width 400 :height 450
      :on-close :exit)))

(def cell-style (style :foreground "#000"
                       :background "#000"
                       :stroke (stroke :width 0)))

(defn -main []
  (native!)
  (let [f (mainframe),
        worldref (atom #{[-1 0] [0 0] [1 0] [1 1] [0 2] [-10 -10] [10 10]})]
    (letfn [
      (step [e]
        (swap! worldref next-gen)
        (repaint! (select f [:#canvas])))

      (paint-world [c g]
        (let [world @worldref,
              [[r1 r2] [c1 c2] :as ranges] (-> world world-ranges grid-ranges),
              gr (grid (.getWidth c) (.getHeight c) ranges)]
          (doseq [row (range r1 (inc r2)),
                  col (range c1 (inc c2))
                  :when (contains? world [row col])]
            (draw g (apply rect (cellPos gr row col)) cell-style))))

      (click-cell [e]
        (let [c (.getSource e),
              world @worldref,
              ranges (-> world world-ranges grid-ranges),
              gr (grid (.getWidth c) (.getHeight c) ranges)]
          (swap! worldref toggle-cell (cellOn gr (.getX e) (.getY e)))
          (repaint! c)))

    ] (config! (select f [:#canvas]) :paint paint-world)
      (listen (select f [:#step]) :action step)
      (show! f))))