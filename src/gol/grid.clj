;; # Game of life simulator #
;;
;; The following listing shows how to simulate and present as a Swing
;; application the game of life.
(ns gol.grid)

;; ## Grid coordinates translation ##
;;

(defn world-ranges
  "Extract rows and cols ranges of a given world (cell set)."
  [cells]
  (let [rows (map first cells),
        cols (map second cells),
        get-range #(vector (apply min %)
                     (apply max %))]
    (if (empty? cells)
      [[0 0] [0 0]]
      [(get-range rows) (get-range cols)])))

;; Show 21 x 21 cells at least (a radius of 10 cells).
(def min-radius 10)
(defn grid-ranges [world-ranges]
  (letfn [(grid-range [[from to]]
            [(min from (- min-radius))
             (max to min-radius)])]
    (map grid-range world-ranges)))

(defn range-size
  "Length of a [from to] range."
  [[from to]]
  (inc (- to from)))

(defprotocol Grid
  (cellSize [this] "Cell side in pixels")
  (cellPos [this row col] "Cell top-left corner, width and height")
  (cellOn [this x y] "Cell on a given pixel coordinates"))

;; Allows decoupling coordinate transformation from GUI code.
(defn grid
  "Define a grid given some pixel dimensions and a range of cells to contain."
  [w h ranges]
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
