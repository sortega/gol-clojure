(ns gol.model)

(defn neighbours [cell]
  (map
    (fn [delta] (map + cell delta)) [[-1 -1] [-1 0] [-1 1]
                                     [ 0 -1]        [ 0 1]
                                     [ 1 -1] [ 1 0] [ 1 1]]))
(defn survive? [neigs]
  (and (>= neigs 2) (<= neigs 3)))

(defn newborn? [neigs]
  (== neigs 3))

(defn next-gen [cells]
  (let [neigs     (frequencies (mapcat neighbours cells))
        survivors (filter (fn [cell] (survive? (get neigs cell 0))) cells)
        newborns  (map first (filter #(newborn? (second %)) neigs))]
       (set (concat survivors newborns))))

(defn gen-seq [world]
  (iterate next-gen world))

(defn toggle-cell [world cell]
  (if (contains? world cell)
    (disj world cell)
    (conj world cell)))