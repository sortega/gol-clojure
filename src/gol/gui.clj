;; ## Swing user interface ##
(ns gol.gui
  (:use [gol.model])
  (:use [gol.grid])
  (:use [seesaw.core])
  (:use [seesaw.color])
  (:use [seesaw.graphics])
  (:use [seesaw.dev]))

;; It is completely declarative and similar to an HTML tree in which some
;; components are marked with unique ids.
(defn mainframe
  "Builds the main app frame."
  []
  (let [world-widget (canvas :id :canvas
                             :background "#FFF")
        step-btn (button :text "step" :id :step)
        run-btn  (button :text "run" :id :run)
        controls (horizontal-panel :items [step-btn run-btn])]
    (frame :title "Game of life simulator"
      :content (border-panel :center world-widget
                 :south controls)
      :width 400 :height 450
      :on-close :exit)))

;; **Pencil** for painting the cells.
(def cell-style (style :foreground "#000"
                       :background "#000"
                       :stroke (stroke :width 0)))

(defn -main []
  (native!) ;; As native as possible
  (let [f (mainframe),
        world-ref (atom #{[-1 0] [0 0] [1 0] [1 1] [0 2]}),
        timer-fn-ref (atom nil)
        timer (timer (fn [e] (@timer-fn-ref e))
                     :delay 250
                     :start? false)]
    (letfn [
      (step
        "Triggers a new generation of the world and repaint the canvas."
        [e]
        (swap! world-ref next-gen)
        (repaint! (select f [:#canvas])))

      (paint-world
        "Paint the world."
        [c g]
        (let [world @world-ref,
              [[r1 r2] [c1 c2] :as ranges] (-> world world-ranges grid-ranges),
              gr (grid (.getWidth c) (.getHeight c) ranges)]
          (doseq [row (range r1 (inc r2)),
                  col (range c1 (inc c2))
                  :when (contains? world [row col])]
            (draw g (apply rect (cellPos gr row col)) cell-style))))

      (click-cell
        "Toggle alife/dead the cell at the pointer position."
        [e]
        (let [c (.getSource e),
              world @world-ref,
              ranges (-> world world-ranges grid-ranges),
              gr (grid (.getWidth c) (.getHeight c) ranges)]
          (swap! world-ref toggle-cell (cellOn gr (.getX e) (.getY e)))
          (repaint! c)))

      (toggle-run
        "Starts/stops a timer to update the world."
        [e]
        (swap! timer-fn-ref
          (fn [fn]
            (let [run-btn (select f [:#run])
                  step-btn (select f [:#step])]
              (if (nil? fn)
                (do ; run
                  (config! run-btn :text "stop")
                  (config! step-btn :enabled? false)
                  (.start timer)
                  step)
                (do ; stop
                  (config! run-btn :text "run")
                  (config! step-btn :enabled? true)
                  (.stop timer)
                  nil
                  )
              )))))

    ] ;; Configure behaviour for the interface
      (config! (select f [:#canvas]) :paint paint-world)
      (listen (select f [:#canvas]) :mouse-clicked click-cell)
      (listen (select f [:#step]) :action step)
      (listen (select f [:#run]) :action toggle-run)
      (show! f)
      nil)))
