(ns snakes.core
    (:require
      [reagent.core :as reagent :refer [atom]]
      [clojure.string :as string]))

(enable-console-print!)

(defonce game-size 20)
(defonce default-direction :down)

(def initial-snakes [
          {:color "blue" :pos [[2 4] [2 5] [2 6] [2 7]] :dir default-direction :owner :player}
          {:color "green" :pos [[2 2] [3 2]] :dir default-direction}
          ; {:color "red" :pos [[3 11] [4 11] [4 12]] :dir :down}
          ])

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {
                      :title "Snakes"
                      :curr-key default-direction
                      :snakes initial-snakes}))

(defn position-valid?
  [block]
  (and (<= 0 (first block) (- game-size 1))
       (<= 0 (second block) (- game-size 1))))

(defn increment-for-dir
  [direction]
  (case direction
    :down   [0 1]
    :up     [0 -1]
    :right  [1 0]
    :left   [-1 0]
    ))

(defn mv-snake
  [snake]
  (let [positions (:pos snake)
        direction (:dir snake)
        increment (increment-for-dir direction)
        lastblock (last positions)
        newblock (vec (map #(+ %1 %2) lastblock increment))
        newpositions (vec (drop 1 (concat positions [newblock])))
       ]
        (if (position-valid? newblock)
          (assoc snake :pos newpositions)
          snake)))

(defn mv-snakes
  [snakes]
  (vec (map mv-snake snakes)))

(defn update-game []
  (let
    [updated-snakes (mv-snakes (:snakes @app-state))]
    (prn "... update-game")
    (prn "snakes: " (:snakes @app-state))
    (prn "snakes updated " updated-snakes)
    (swap! app-state assoc :snakes updated-snakes)))

(defn block [x y color]
  [:rect {:x x
          :y y
          :width 1
          :height 1
          :stroke "black"
          :stroke-width 0.01
          :rx 0.1
          :fill color}])

(defn paint-snake
  [snake]
  (let [color (:color snake)
        positions (:pos snake)]
  (for [pos positions]
      [block (first pos) (second pos) color])))

(defn snakes-game-view []
  [:div
   [:h3 (:title @app-state)]
   (into
     [:svg {:style {:border "1px solid black"
                        :width 400
                        :height 400}
                :view-box (string/join " " [0 0 game-size game-size])}]
      (for [snake (:snakes @app-state)]
           (do
             (prn "painting snake: " snake)
             (paint-snake snake))))])

(def key-to-direction
 {37 :left
  38 :up
  39 :right
  40 :down})

(defn update-user-direction
  [direction]
  (swap! app-state assoc-in [:snakes 0 :dir] direction))

(defn handle-keydown [e]
   (when-let
     [direction (key-to-direction (.-keyCode e))]
     (update-user-direction direction)))

(defn init []
 (reagent/render-component [snakes-game-view]
                           (. js/document (getElementById "app")))
 (.addEventListener js/document "keydown" handle-keydown)
 (js/setInterval update-game 500)
)

(defonce start
  (init))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state assoc :snakes initial-snakes)
)