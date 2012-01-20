(ns gol.gui
  (:use [gol.model])
  (:use [seesaw.core])
  (:use [seesaw.color])
  (:use [seesaw.graphics])
  (:use [seesaw.dev]))

(def min-radius 10)
(def max-cell-size 10)

(defn world-ranges [cells]
  (let [rows (map first cells),
        cols (map second cells),
        get-range #(vector (apply min (- min-radius) %)
                           (apply max    min-radius  %))]
    [(get-range rows)
     (get-range cols)]))


(defn cell-size [space cells]
  (min max-cell-size (/ space cells)))

(def cell-style (style :foreground "#000"
                       :background "#000"
                       :stroke (stroke :width 0)))

(defn index-of [cell-size space pos]
  (quot
    (+ pos
       (- (/ space 2))
       (/ cell-size 2))
    cell-size))

(defn world-widget [worldref]
  (letfn [
    (paint-world [c g]
      (let [world @worldref,
            w (.getWidth c),
            h (.getHeight c),
            [[r1 r2] [c1 c2]] (world-ranges world),
            cell-size (min (cell-size h (inc (- r2 r1)))
                           (cell-size w (inc (- c2 c1))))]

        (translate g (/ w 2) (/ h 2))
        (scale g cell-size)
        (translate g -0.5 -0.5)
        (doseq [row (range r1 (inc r2)),
                col (range c1 (inc c2))
                :when (contains? world [row col])]
          (draw g (rect row col 1) cell-style))))

    (click-cell [e]
      (let [x (.getX e),
            y (.getY e),
            c (.getSource e),
            w (.getWidth c),
            h (.getHeight c),
            [[r1 r2] [c1 c2]] (world-ranges @worldref),
            cell-size (min (cell-size h (inc (- r2 r1)))
                           (cell-size w (inc (- c2 c1))))]
        (swap! worldref toggle-cell [(index-of cell-size h x)
                                     (index-of cell-size w y)])
        (repaint! c)))
    ] (canvas :background "#FFF"
              :paint paint-world
              :listen [:mouse-clicked click-cell])))

(defn mainframe []
  (let [worldref (atom #{[-1 0] [0 0] [1 0] [2 2] [-10 -10] [10 10]})
        world-widget (world-widget worldref)
        step     (fn [e]
                    (swap! worldref next-gen)
                    (repaint! world-widget))
        run-btn  (button :text "run")
        step-btn (button :text "step" :listen [:action step])
        controls (horizontal-panel :items [run-btn step-btn])]

    (frame :title "Game of life simulator"
           :content (border-panel :center world-widget
                                  :south controls)
           :width 400 :height 450
           :on-close :exit)))

(defn -main []
  (native!)
  (show! (mainframe)))