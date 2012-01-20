(ns gol.grid)

(def min-radius 10)

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
