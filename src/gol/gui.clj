(ns gol.gui
  (:use [gol.model])
  (:use [seesaw.core])
  (:use [seesaw.color])
  (:use [seesaw.graphics])
  (:use [seesaw.dev]))

(def min-radius 10)
(def max-cell-size 10)

(defn grid-ranges [world-ranges]
  (letfn [(grid-range [[from to]]
            [(min from (- min-radius))
             (max to min-radius)])]
    (map grid-range world-ranges)))

(defn world-ranges [cells]
  (let [rows (map first cells),
        cols (map second cells),
        get-range #(vector (apply min %)
                           (apply max %))]
    (if (empty? cells)
      [[0 0] [0 0]]
      [(get-range rows) (get-range cols)])))

(defn range-size [[from to]]
  (inc (- to from)))

(defprotocol Grid
  (cellSize [this])
  (cellPos [this row col])
  (cellOn [this x y]))

(defn grid [w h ranges]
  ; TODO: consider centering when extra space
  (let [[[r1 _] [c1 _]] ranges,
        [rows cols] (map range-size ranges),
        cell-size (min (/ h rows) (/ w cols))]
    (reify Grid
      (cellSize [this] cell-size)
      (cellPos [this row col]
        [(* (- col c1) cell-size)
         (* (- row r1) cell-size)
         cell-size
         cell-size])
      (cellOn [this x y]
        [(+ (quot y cell-size) r1)
         (+ (quot x cell-size) c1)]))))

(def cell-style (style :foreground "#000"
                       :background "#000"
                       :stroke (stroke :width 0)))

(defn world-widget [worldref]
  (letfn [
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