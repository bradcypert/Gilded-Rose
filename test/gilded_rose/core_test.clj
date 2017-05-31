;the specs

(ns gilded-rose.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [gilded-rose.core :as core]))

(defn do-x-times [func input x]
  (nth (iterate func input) x))

; Running out of time, these could be cleaner and broken into individual tests
; or we could use Midje or Speclj
(deftest single-run
  (let [inventory (core/update-current-inventory)
        [vest brie elixir sulfuras tickets] inventory]
    (testing "sulfuras should maintain 80"
      (is (= 80 (:quality sulfuras))))
    (testing "brie should go up to 1"
      (is (= 1 (:quality brie))))
    (testing "tickets should go up to 21"
      (is (= 21 (:quality tickets))))
    (testing "elixir should go down to 6"
      (is (= 6 (:quality elixir))))
    (testing "vest should go down to 19"
      (is (= 19 (:quality vest))))))


(deftest multi-run
  (let [inventory (core/update-current-inventory)
        inventory (do-x-times core/update-inventory inventory 3)
        [vest brie elixir sulfuras tickets] inventory]
    (testing "sulfuras should maintain 80"
      (is (= 80 (:quality sulfuras))))
    (testing "brie should go up to 5"
      (is (= 5 (:quality brie))))
    (testing "tickets should go up to 24"
      (is (= 24 (:quality tickets))))
    (testing "elixir should go down to 3"
      (is (= 3 (:quality elixir))))
    (testing "vest should go down to 16"
      (is (= 16 (:quality vest))))))


; Probing at edge cases, these should be cleaner, refactor in the future
(deftest x-run
  (let [inventory (core/update-current-inventory)
        inventory (do-x-times core/update-inventory inventory 10)
        [vest brie elixir sulfuras tickets] inventory]
    (testing "sulfuras should maintain 80"
      (is (= 80 (:quality sulfuras))))
    (testing "brie should go up to 5"
      (is (= 19 (:quality brie))))
    (testing "tickets should go up to 36"
      (is (= 36 (:quality tickets))))
    (testing "elixir should not go below 0"
      (is (= 0 (:quality elixir))))
    (testing "vest should go down to 9"
      (is (= 9 (:quality vest))))))


(deftest un-brie-leviable
  (let [inventory (core/update-current-inventory)
        inventory (do-x-times core/update-inventory inventory 50)
        [vest brie elixir sulfuras tickets] inventory]
    (testing "brie should should go no higher than 50"
      (is (= 50 (:quality brie))))))


(deftest conjured
  (let [inventory
        [(core/item "+5 Dexterity Vest" 10 20)
         (core/item "Aged Brie" 2 0)
         (core/item "Elixir of the Mongoose" 5 7)
         (core/item "Sulfuras, Hand Of Ragnaros" 0 80)
         (core/item "Backstage passes to a TAFKAL80ETC concert" 15 20)]
        inventory (map #(assoc % :tags [:conjured]) inventory)
        inventory (core/update-inventory inventory)
        [vest brie elixir sulfuras tickets] inventory]
    (testing "sulfuras should maintain 80"
      (is (= 80 (:quality sulfuras))))
    (testing "brie should go up to 2"
      (is (= 2 (:quality brie))))
    (testing "tickets should go up to 22"
      (is (= 22 (:quality tickets))))
    (testing "elixir should go down to 5"
      (is (= 5 (:quality elixir))))
    (testing "vest should go down to 18"
      (is (= 18 (:quality vest))))))
