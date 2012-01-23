(ns gol.model)

(defn neighbours
  "Neighbours of a given cell."
  [cell]
  (map
    (fn [delta] (map + cell delta)) [[-1 -1] [-1 0] [-1 1]
                                     [ 0 -1]        [ 0 1]
                                     [ 1 -1] [ 1 0] [ 1 1]]))
(defn survive?
  "A cell with 2 to 3 neighbours survives."
  [neigs]
  (and (>= neigs 2) (<= neigs 3)))

(defn newborn?
  "A space with 3 neighbours will hold a new one."
  [neigs]
  (== neigs 3))

(defn next-gen
  "Apply survive and newborn rules to a set of cells."
  [cells]
  (let [neigs     (frequencies (mapcat neighbours cells))
        survivors (filter (fn [cell] (survive? (get neigs cell 0))) cells)
        newborns  (map first (filter #(newborn? (second %)) neigs))]
       (set (concat survivors newborns))))

(defn gen-seq
  "Lazy sequence of world states"
  [world]
  (iterate next-gen world))

(defn toggle-cell
  "Kills/Creates a cell in a given position."
  [world cell]
  (if (contains? world cell)
    (disj world cell)
    (conj world cell)))