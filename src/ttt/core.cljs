(ns ttt.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [figwheel.client :as fw]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
              [clojure.string :as str]
            [cljs.core.async :refer [put! chan <!]]))

;; game logic
(defn new-board [] "---------")

(defn matches [matcher board]
  (map first (filter #(= (second %) matcher)
             (map-indexed vector board))))

(defn win? [board]
  (def winners [[0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6] [0 1 2] [3 4 5] [6 7 8]])
  (some true?
         (into (map (fn [winner] (every? (set (matches \x board)) winner)) winners)
               (map (fn [winner] (every? (set (matches \o board)) winner)) winners))))

(defn draw? [board]
  (and (< (count (matches \- board)) 3)
       (not (some true? (map (fn [x] (win? (str/replace board "-" x))) ["x" "o"])))))

;; browser interaction
(def app-state (atom {:player (rand-nth ["x" "o"]) :board (new-board)}))

(defn move [board place player human?]
  (let [board-after-human
    (if (not= \- (get board place))
      (do (js/alert "Invalid move") board)
      (let [board-after-move (str (subs board 0 place) player (subs board (inc place)))]
        (if (win? board-after-move)
          (do (js/alert (str "Player ["player"] wins!")) (swap! app-state assoc :board (new-board)))
          (if (draw? board-after-move)
            (do (js/alert (str "It's a Draw!")) (swap! app-state assoc :board (new-board)))
            (do (swap! app-state assoc :player (if (= "x" player) "o" "x")) ;; change player for the next move
                (swap! app-state assoc :board board-after-move))))))]
  (if human?
    (do (move (:board board-after-human) (rand-nth (vec (matches \- (:board board-after-human)))) (if (= "x" player) "o" "x") false))
    board-after-human)))

(defn game [data owner]
  (reify
    om/ICheckState
    om/IInitState
    (init-state [_]
      {})
    om/IRenderState
    (render-state [_ {}]
      (dom/div nil
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 0 (:player data) true))} (subs (:board data) 0 1))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 1 (:player data) true))} (subs (:board data) 1 2))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 2 (:player data) true))} (subs (:board data) 2 3))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 3 (:player data) true))} (subs (:board data) 3 4))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 4 (:player data) true))} (subs (:board data) 4 5))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 5 (:player data) true))} (subs (:board data) 5 6))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 6 (:player data) true))} (subs (:board data) 6 7))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 7 (:player data) true))} (subs (:board data) 7 8))
        (dom/button #js {:onClick #(om/set-state! owner :board (move (str (:board data)) 8 (:player data) true))} (subs (:board data) 8 9))))))

(om/root game app-state
  {:target (.getElementById js/document "app")})
