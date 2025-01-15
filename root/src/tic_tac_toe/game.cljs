(ns tic-tac-toe.game
  (:require [helix.core :refer [defnc $]]
            [helix.dom :as d]
            [refx.alpha :refer [use-sub dispatch]]))

(defn check-winner [current-board]
  (let [board (for [row current-board
                    col [:col1 :col2 :col3]]
                (get row col))
        rows (partition 3 board)
        cols (apply map vector rows)
        diag1 [(nth board 0) (nth board 4) (nth board 8)]
        diag2 [(nth board 2) (nth board 4) (nth board 6)]
        all-lines (concat rows cols [diag1 diag2])]
    (some #(when (and (apply = %) (not= (first %) "-")) (first %)) all-lines)))

(defn check-draw [current-board]
  (every? #(not= "-" %) (flatten (map #(vals (dissoc % :id)) current-board))))

(defn get-available-moves [current-board]
  (for [row (range 3)
        col (range 3)
        :let [row-data (nth current-board row)
              cell-value (get row-data (keyword (str "col" (inc col))))]
        :when (= "-" cell-value)]
    [row (keyword (str "col" (inc col)))]))

(defn make-move-on-board [current-board row col-key player]
  (map-indexed (fn [idx row-data]
                 (if (= idx row)
                   (assoc row-data col-key player)
                   row-data))
               current-board))

(defn evaluate-board [current-board player]
  (let [winner (check-winner current-board)
        opponent (if (= player "X") "O" "X")]
    (cond
      (= winner player) (+ 10 (count (get-available-moves current-board)))
      (= winner opponent) (- -10 (count (get-available-moves current-board)))
      :else 0)))

(def memo-evaluate-board (memoize evaluate-board))
(def memo-check-winner (memoize check-winner))

(defn minimax [current-board player depth maximizing? alpha beta]
  (let [winner (memo-check-winner current-board)
        moves (get-available-moves current-board)]
    (cond
      (or winner (zero? depth) (empty? moves))
      [(memo-evaluate-board current-board player) nil]

      maximizing?
      (let [results
            (loop [moves-left moves
                   best-score js/Number.NEGATIVE_INFINITY
                   best-moves []
                   alpha alpha]
              (if (or (empty? moves-left) (> alpha beta))
                [best-score (when (seq best-moves)
                              (rand-nth best-moves))]
                (let [[row col-key] (first moves-left)
                      new-board (make-move-on-board current-board row col-key player)
                      [score _] (minimax new-board player (dec depth) false alpha beta)
                      new-best-moves (cond
                                       (> score best-score) [[row col-key]]
                                       (= score best-score) (conj best-moves [row col-key])
                                       :else best-moves)
                      new-best-score (max best-score score)
                      new-alpha (max alpha new-best-score)]
                  (recur (rest moves-left)
                         new-best-score
                         new-best-moves
                         new-alpha))))]
        results)

      :else
      (let [opponent (if (= player "X") "O" "X")
            results
            (loop [moves-left moves
                   best-score js/Number.POSITIVE_INFINITY
                   best-moves []
                   beta beta]
              (if (or (empty? moves-left) (< beta alpha))
                [best-score (when (seq best-moves)
                              (rand-nth best-moves))]
                (let [[row col-key] (first moves-left)
                      new-board (make-move-on-board current-board row col-key opponent)
                      [score _] (minimax new-board player (dec depth) true alpha beta)
                      new-best-moves (cond
                                       (< score best-score) [[row col-key]]
                                       (= score best-score) (conj best-moves [row col-key])
                                       :else best-moves)
                      new-best-score (min best-score score)
                      new-beta (min beta new-best-score)]
                  (recur (rest moves-left)
                         new-best-score
                         new-best-moves
                         new-beta))))]
        results))))

(defn get-computer-move [current-board]
  (let [[_ best-move] (minimax current-board "O" 9 true js/Number.NEGATIVE_INFINITY js/Number.POSITIVE_INFINITY)]
    (when best-move
      (let [[row col-key] best-move]
        {:row (nth current-board row)
         :col col-key}))))

(defn make-move [row col-key state] 
  (let [new-board-data (map #(if (= (:id %) (:id row))
                               (assoc % col-key (:current-player state))
                               %)
                            (:board-data state))
        winner (check-winner new-board-data) 
        is-draw (check-draw new-board-data)
        game-over (or winner is-draw)
        next-player (if (= (:current-player state) "X") "O" "X")]
    (cond-> state
      true (update :turns inc)
      true (assoc :board-data new-board-data) 
      (not game-over) (assoc :current-player next-player)
      game-over (-> (assoc :game-over true)
                    (update :total-games inc))
      (and is-draw (= winner nil)) (update :draws inc)
      (= winner "X") (update :player1-wins inc)
      (= winner "O") (update :player2-wins inc))))

(defnc game-mode-selection []
  (let [state (use-sub [:game-state])] 
    (d/div {:class "mb-4"}
           (when (not (state :game-mode))
             (d/h2 {:class "text-xl mb-2"} "Select Game Mode"))
           (when (not (:setup-complete state))
             (d/div {:class "space-x-4"}
                    (d/button
                     {:class "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                      :on-click #(dispatch [:select-game-mode "1p"])}
                     "1 Player Game")
                    (d/button
                     {:class "px-4 py-2 bg-green-500 text-white rounded hover:bg-blue-600"
                      :on-click #(dispatch [:select-game-mode "2p"])}
                     "2 Player Game"))))))

(defnc mobile-game-mode-selection []
  (let [state (use-sub [:game-state])]
    (d/div {:class "mb-4"}
           (when (not (state :game-mode))
             (d/h6 {:class "text-xl mb-2"} "Select Game Mode"))
           (when (not (:setup-complete state))
             (d/div {:class "space-x-4"}
                    (d/button
                     {:class "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
                      :on-click #(dispatch [:select-game-mode "1p"])}
                     "1 Player Game")
                    (d/button
                     {:class "px-4 py-2 bg-green-500 text-white rounded hover:bg-blue-600"
                      :on-click #(dispatch [:select-game-mode "2p"])}
                     "2 Player Game"))))))

(defnc player-name-input []
  (let [state (use-sub [:game-state])
        player1-name (or (:player1-name state) "")
        player2-name (or (:player2-name state) "")
        set-player1-name #(dispatch [:update-player-name 1 %])
        set-player2-name #(dispatch [:update-player-name 2 %])]
    (when (and (:game-mode state) (not (:setup-complete state)))
      (d/div {:class "mb-4"}
             (d/h2 {:class "text-xl mb-2"} "Enter Player Names")
             (d/div {:class "space-y-2"}
                    (d/input {:type "text" 
                              :class "border p-2 rounded mr-2"
                              :value (or (:player1-name state) "")
                              :on-change #(set-player1-name (.. % -target -value))
                              :on-focus #(set-player1-name "")
                              :on-blur #(when (empty? player1-name) (set-player1-name ""))})
                    (when (= (:game-mode state) "2p")
                      (d/input {:type "text" 
                                :class "border p-2 rounded"
                                :value (or (:player2-name state) "Player 2")
                                :on-change #(set-player2-name (.. % -target -value))
                                :on-focus #(set-player2-name "")
                                :on-blur #(when (empty? player2-name) (set-player2-name ""))})))
             (d/button {:class "px-4 py-2 bg-purple-500 text-white rounded hover:bg-purple-600 mt-2"
                        :disabled (or (empty? (:player1-name state))
                                      (and (= (:game-mode state) "2p")
                                           (empty? (:player2-name state))))
                        :on-click #(dispatch [:start-game])}
                       "Start Game")))))

(defnc mobile-player-name-input []
  (let [state (use-sub [:game-state])
        player1-name (or (:player1-name state) "")
        player2-name (or (:player2-name state) "")
        set-player1-name #(dispatch [:update-player-name 1 %])
        set-player2-name #(dispatch [:update-player-name 2 %])]
    (when (and (:game-mode state) (not (:setup-complete state)))
      (d/div {:class "mb-4"}
             (d/h4 {:class "text-xl mb-2"} "Enter Player Names")
             (d/div {:class "space-y-2"}
                    (d/input {:type "text"
                              :class "border p-2 rounded mr-2"
                              :value (or (:player1-name state) "")
                              :on-change #(set-player1-name (.. % -target -value))
                              :on-focus #(set-player1-name "")
                              :on-blur #(when (empty? player1-name) (set-player1-name ""))})
                    (when (= (:game-mode state) "2p")
                      (d/input {:type "text"
                                :class "border p-2 rounded"
                                :value (or (:player2-name state) "Player 2")
                                :on-change #(set-player2-name (.. % -target -value))
                                :on-focus #(set-player2-name "")
                                :on-blur #(when (empty? player2-name) (set-player2-name ""))})))
             (d/button {:class "px-4 py-2 bg-purple-500 text-white rounded hover:bg-purple-600 mt-2"
                        :disabled (or (empty? (:player1-name state))
                                      (and (= (:game-mode state) "2p")
                                           (empty? (:player2-name state))))
                        :on-click #(dispatch [:start-game])}
                       "Start Game")))))

(defnc statistics []
  (let [state (use-sub [:game-state])]
    (when (:setup-complete state)
      (d/div {:id "stats-and-winning-message"}
      (d/div {:id "stats"}
             (d/div {:id "left-stats"}
                    (d/p (str "Total Games Played: " (:total-games state))) 
                    (d/p (str (:player1-name state) " (X): " (:player1-wins state) " wins"))
                    (d/p (str (:player2-name state) " (O): " (:player2-wins state) " wins")))
             (d/div {:id "right-stats"}
                    (d/p (str "Current Turns: " (:turns state)))
                    (d/p (str "Draws: " (:draws state)))
                    (d/p (str "Current Player: "
                              (if (:game-over state)
                                "Game Over"
                                (str (if (= (:current-player state) "X")
                                       (:player1-name state)
                                       (:player2-name state))
                                     " (" (:current-player state) ")"))))))
             (when (:game-over state)
               (let [winner (check-winner (:board-data state))]
                 (if winner
                   (d/p {:class "text-xl font-bold"}
                        (str (if (= winner "X")
                               (:player1-name state)
                               (:player2-name state))
                             " wins!"))
                   (d/p {:class "text-xl font-bold"} "It's a draw!"))))))))

(defnc mobile-statistics []
  (let [state (use-sub [:game-state])]
    (when (:setup-complete state)
      (d/div {:id "stats-and-winning-message"}
             (d/div {:id "stats"
                     :style {:font-size "11pt"}}
                    (d/div {:id "left-stats"}
                           (d/p (str "Total Games Played: " (:total-games state)))
                           (d/p (str (:player1-name state) " (X): " (:player1-wins state) " wins"))
                           (d/p (str (:player2-name state) " (O): " (:player2-wins state) " wins")))
                    (d/div {:id "right-stats"}
                           (d/p (str "Current Turns: " (:turns state)))
                           (d/p (str "Draws: " (:draws state)))
                           (d/p (str "Current Player: "
                                     (if (:game-over state)
                                       "Game Over"
                                       (str (if (= (:current-player state) "X")
                                              (:player1-name state)
                                              (:player2-name state))
                                            " (" (:current-player state) ")"))))))
             (when (:game-over state)
               (let [winner (check-winner (:board-data state))]
                 (if winner
                   (d/p {:class "text-xl font-bold"}
                        (str (if (= winner "X")
                               (:player1-name state)
                               (:player2-name state))
                             " wins!"))
                   (d/p {:class "text-xl font-bold"} "It's a draw!"))))))))

(defnc tic-tac-table []
  (let [state (use-sub [:game-state])]
    (when (:setup-complete state)
      (d/div {:id "tic-tac-table"}
             (d/div {:id "board"}
                    (for [row (:board-data state)]
                      (for [col-key [:col1 :col2 :col3]]
                        (d/div {:key (str (:id row) (name col-key))
                                :class "cell"
                                :on-click #(when (and (= "-" (get row col-key))
                                                      (not (:game-over state)))
                                             (dispatch [:make-move row col-key]))}
                               (get row col-key)))))))))

(defnc new-game-buttons []
  (let [state (use-sub [:game-state])]
    (when (state :game-mode)
      (d/div {:class "flex"}
             (d/button
              {:class "px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600 mt-4"
               :on-click #(dispatch [:reset-game])}
              "Reset Game")
             (d/button
              {:class "px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 mt-4 ml-2"
               :on-click #(dispatch [:new-game])}
              "New Game")))))

(defnc mobile-page-heading []
  (d/div {:id "mobile-page-heading"}
         (let [state (use-sub [:game-state])]
           (if (not (state :game-mode))
             (d/h3 "Tic Tac Toe!")
             (d/h4 {:style {:padding "0" :margin-top "50px" :margin-bottom "0px"}}
              (state :player1-name) " vs " (if (state :player2-name)
                                                  (state :player2-name)
                                                  "Computer"))))))

(defnc page-heading [] 
  (d/div {:id "page-heading"}
         (let [state (use-sub [:game-state])]
           (if (not (state :game-mode))
             (d/h1 {:style {:padding "0" :margin-bottom "0"}} "Tic Tac Toe")
             (d/h4 {:style {:padding "0" :margin-bottom "0"}}
                   (state :player1-name) " vs " (if (state :player2-name)
                                                  (state :player2-name)
                                                  "Computer"))))))

(defnc toggle-theme-button []
  (d/div {:id "theme-holster"
          :class "relative min-h-screen"}
         (d/button {:id "theme-button"
                    :class "theme-button"
                    :on-click #(dispatch [:toggle-background])}
                   "Toggle Background")))

(defnc mobile-game-container []
  (let [state (use-sub [:game-state])]
    (d/div  {:id "mobile-game-container"}
            ($ mobile-page-heading)
            ($ mobile-game-mode-selection)
            ($ mobile-player-name-input)
            ($ mobile-statistics)
            ($ tic-tac-table)
            ($ new-game-buttons))))

(defnc game-container []
  (d/div {:id "game-container"} 
         ($ page-heading)
         ($ game-mode-selection)
         ($ player-name-input)
         ($ statistics)
         ($ tic-tac-table)
         ($ new-game-buttons)))