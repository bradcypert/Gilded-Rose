(ns gilded-rose.core)

; Legendary Items don't change in quality
(def legendary-item-names
  '("Sulfuras, Hand Of Ragnaros"))

; I won't touch this function no matter how badly I want to.
(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})


(defn- expired? [item]
  (< (:sell-in item) 0))


(defn- update-sell-in [item]
  (update item :sell-in dec))


; We can leverage update-quality for each item
; and just specify how certain ones (aged brie)
; deviate from the from the default.
(defmulti update-quality
  (fn [item]
    (:name item)))


(defmethod update-quality "Aged Brie"
  [item]
  (let [quality (:quality item)]
      (if (< quality 50)
        (if (expired? item)
          (update item :quality + 2)
          (update item :quality inc))
        (assoc item :quality 50))))


(defmethod update-quality "Backstage passes to a TAFKAL80ETC concert"
  [item]
  (let [quality (:quality item)]
      (if (< quality 50)
        (cond
          (expired? item)
            (assoc item :quality 0)
          (< (:sell-in item) 5)
            (update item :quality + 3)
          (< (:sell-in item) 10)
            (update item :quality + 2)
          :else
            (update item :quality inc))
        (assoc item :quality 50))))


(defmethod update-quality :default
  [item]
  (if-not (contains? (set legendary-item-names) (:name item))
    (cond
      (expired? item)
        (update item :quality - 2)
      :else
        (update item :quality dec))
    item))


(defn- handle-negative-qualities
  "Resets a quality to 0 provided it is below legal bounds"
  [item]
  (if (< (:quality item) 0)
    (assoc item :quality 0)
    item))


(defn- handle-conjured
  "If the item is conjured, we go ahead and trigger update quality once"
  [item]
  (if (some #{:conjured} (:tags item))
    (update-quality item)
    item))


(defn- update-item
  "pipeline for handline item updates w/ quality and sell-in"
  [item]
  (-> item
    handle-conjured
    update-quality
    update-sell-in
    handle-negative-qualities))


(defn update-inventory
  "Updates a list of items"
  [inventory]
  (map update-item inventory))


(defn update-current-inventory
  "Helper function to get build an inventory and update the quality of it."
  []
  (let [inventory
    [(item "+5 Dexterity Vest" 10 20)
     (item "Aged Brie" 2 0)
     (item "Elixir of the Mongoose" 5 7)
     (item "Sulfuras, Hand Of Ragnaros" 0 80)
     (item "Backstage passes to a TAFKAL80ETC concert" 15 20)]]
    (update-inventory inventory)))
