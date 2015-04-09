(ns ttt.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [figwheel.client :as fw]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [clojure.data :as dt]
            [jayq.core :refer [$ css html]]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

;; game logic
(defn new-board [] "---------")

(defn matches [matcher board]
  (map first (filter #(= (second %) matcher)
             (map-indexed vector board))))

(def winners [[0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6] [0 1 2] [3 4 5] [6 7 8]])

(defn win? [board]
  (some true?
         (into (map (fn [winner] (every? (set (matches \x board)) winner)) winners)
               (map (fn [winner] (every? (set (matches \o board)) winner)) winners))))

(defn draw? [board]
  (and (< (count (matches \- board)) 3)
       (not (some true? (map (fn [x] (win? (str/replace board "-" x))) ["x" "o"])))))

;; browser interaction
(def app-state (atom {:player "x" :board (new-board)}))

(defn robot-move [board]
  (let [x-moves (vec (matches \x board))
        o-moves (vec (matches \o board))
        possible-moves (vec (matches \- board))
        corners-and-center (vec (filter (fn [x] (some #{0 2 4 6 8} [x])) possible-moves))]
    (if (< 6 (count possible-moves))
      (rand-nth corners-and-center)
      (do
          (let [win-move   (filter #(not (nil? %))
                             (map (fn [y] (some y possible-moves))
                                 (filter #(not (nil? %))
                                         (map (fn [x] (if (= 1 (count (second (dt/diff (set o-moves) (set x))))) (second (dt/diff (set o-moves) (set x)))))
                                               winners))))
                block-move (filter #(not (nil? %))
                             (map (fn [y] (some y possible-moves))
                                 (filter #(not (nil? %))
                                         (map (fn [x] (if (= 1 (count (second (dt/diff (set x-moves) (set x))))) (second (dt/diff (set x-moves) (set x)))))
                                               winners))))]
            (if (< 0 (count win-move))
              (rand-nth win-move)
              (if (< 0 (count block-move))
                (rand-nth block-move)
                (rand-nth possible-moves))))))))

(defn move [board place player human?]
  (let [board-after-human
    (if (not= \- (get board place))
      (do (js/alert "Invalid move") board)
      (let [board-after-move (str (subs board 0 place) player (subs board (inc place)))]
            (do (swap! app-state assoc :player (if (= "x" player) "o" "x")) ;; change player for the next move
                (swap! app-state assoc :board board-after-move))))]
  (if human?
    (if (not (win? (:board board-after-human)))
    (do (move (:board board-after-human) (robot-move (:board board-after-human)) (if (= "x" player) "o" "x") false))
    board-after-human))))

(defn restart [data]
  (swap! app-state assoc :player (if (= "x" (:player data)) "o" "x"))
  (let [board (if (= "x" (:player data))
                (do
                  (swap! app-state assoc :player "x")
                  (rand-nth ["o--------" "------o--" "----o----" "--o------" "--------o"]))
                (new-board))]
    (println board)
    (swap! app-state assoc :board board)))

(defn show-board [owner data]
    (do
      (dom/div nil
        ;; (if (not (win? (:board data)))
        (if (win? (:board data))
          (do
              (dom/div #js {:className "alert"}
                      (dom/div #js {:className "finalMessage animated tada"} (str (if (= "x" (:player data)) (rand-nth ["What a shame!" "SuperBot wins!" "It was easy!"]) (rand-nth ["Adrian, we did it!" "This is happiness!" "Sweet!"]))))
                      (dom/button #js {:className "restart animated infinite pulse" :onClick #(om/set-state! owner :board (restart data))} "Play Again!"))))
        (if (draw? (:board data))
          (do
              (dom/div #js {:className "col-lg-8 col-lg-offset-2 alert"}
                      (dom/div #js {:className "finalMessage animated tada"} "It is a Draw!")
                      (dom/button #js {:className "restart animated infinite pulse" :onClick #(om/set-state! owner :board (restart data))} "Play Again!"))))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4 col-md-offset-3 col-lg-offset-3"}
                (dom/button #js {:className (subs (:board data) 0 1)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 0 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4"}
                (dom/button #js {:className (subs (:board data) 1 2)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 1 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4"}
                (dom/button #js {:className (subs (:board data) 2 3)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 2 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4 col-md-offset-3 col-lg-offset-3"}
                (dom/button #js {:className (subs (:board data) 3 4)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 3 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4"}
                (dom/button #js {:className (subs (:board data) 4 5)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 4 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4"}
                (dom/button #js {:className (subs (:board data) 5 6)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 5 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4 col-md-offset-3 col-lg-offset-3"}
                (dom/button #js {:className (subs (:board data) 6 7)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 6 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4"}
                (dom/button #js {:className (subs (:board data) 7 8)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 7 (:player data) true))}))
        (dom/div #js {:className "col-lg-2 col-md-2 col-sm-4 col-xs-4"}
                (dom/button #js {:className (subs (:board data) 8 9)
                                  :onClick #(om/set-state! owner :board (move (str (:board data)) 8 (:player data) true))})))))

(defn game [data owner]
  (reify
    om/ICheckState
    om/IInitState
    (init-state [_]
      {})
    om/IRenderState
    (render-state [_ {}]
      (show-board owner data))))

(om/root game app-state
  {:target (.getElementById js/document "game")})
