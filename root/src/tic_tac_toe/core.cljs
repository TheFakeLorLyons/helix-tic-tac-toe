(ns tic-tac-toe.core
  (:require ["react-dom/client" :as rdom]
            [helix.core :refer [defnc $ defhook]]
            [helix.dom :as d]
            [helix.hooks :as hooks]
            [refx.alpha :as refx :refer [reg-event-db reg-event-fx reg-sub dispatch-sync use-sub dispatch]]
            [tic-tac-toe.game :as game]
            [tic-tac-toe.art :as art]))

(defonce root (atom nil))

(def initial-game-state
  {:board-data [{:id 1 :col1 "-" :col2 "-" :col3 "-"}
                {:id 2 :col1 "-" :col2 "-" :col3 "-"}
                {:id 3 :col1 "-" :col2 "-" :col3 "-"}]
   :player1-name "Player"
   :player2-name "Player 2"
   :turns 0
   :total-games 0
   :player1-wins 0
   :player2-wins 0
   :draws 0
   :game-mode nil
   :current-player "X"
   :game-over false
   :setup-complete false
   :show-background true
   :mobile? false})

(defonce app-db (atom initial-game-state))

(defhook use-mobile-check []
  (let [[is-mobile set-is-mobile] (hooks/use-state false)
        check-mobile (hooks/use-callback
                      []
                      (fn []
                        (let [user-agent (.-userAgent js/navigator)
                              mobile-check (or (re-find #"Mobi" user-agent)
                                               (re-find #"Android" user-agent)
                                               (re-find #"iPhone" user-agent)
                                               (re-find #"iPad" user-agent)
                                               (< (.-innerWidth js/window) 768))]
                          (set-is-mobile mobile-check))))]
        (hooks/use-effect
       []
       (check-mobile)
       (let [handle-resize #(check-mobile)]
         (.addEventListener js/window "resize" handle-resize)
         #(.removeEventListener js/window "resize" handle-resize)))
    
      is-mobile))

(reg-event-db
 :initialize-game
 (fn [_ _]
   initial-game-state))

(reg-event-db
 :select-game-mode
 (fn [db [_ mode]]
   (-> db
       (assoc :game-mode mode)
       (assoc :player2-name (when (= mode "1p") "Computer")))))

(reg-event-db
 :new-game
 (fn [db _]
   (-> db
       (assoc :board-data [{:id 1 :col1 "-" :col2 "-" :col3 "-"}
                           {:id 2 :col1 "-" :col2 "-" :col3 "-"}
                           {:id 3 :col1 "-" :col2 "-" :col3 "-"}])
       (assoc :current-player "X")
       (assoc :game-over false)
       (assoc :turns 0))))

(reg-event-db
 :start-game
 (fn [db _]
   (assoc db :setup-complete true)))

(reg-event-db
 :update-player-name
 (fn [db [_ player-num name]]
   (assoc db (keyword (str "player" player-num "-name")) name)))

(reg-event-fx
 :make-move
 (fn [{:keys [db]} [_ row col-key]]
   (let [new-state (game/make-move row col-key db)]
     (merge
      {:db new-state}
      (when (and (= (:game-mode new-state) "1p")
                 (not (:game-over new-state))
                 (= (:current-player new-state) "O"))
        {:dispatch [:computer-move]})))))

(reg-event-db
 :computer-move
 (fn [db _]
   (let [computer-move (game/get-computer-move (:board-data db))
         row (first (filter #(= (:id %) (get-in computer-move [:row :id])) (:board-data db)))
         col (:col computer-move)]
     (game/make-move row col db))))

(reg-event-db
 :reset-game
 (fn [db _]
   (-> initial-game-state
       (assoc :show-art-scene (:show-art-scene db)))))

(reg-event-db
 :toggle-background
 (fn [db _]
   (update db :show-background not)))

(reg-sub
 :game-state
 (fn [db _]
   db))

(defnc app []
  (let [state (use-sub [:game-state])
        is-mobile (use-mobile-check)]

    (hooks/use-effect
     []
     (when is-mobile
       (try
         (js/screen.orientation.lock "portrait")
         (catch :default e
           (js/console.log "Orientation lock failed:" e))))

     (d/div {:id "global-body"
             :class "relative min-h-screen"}
            ($ game/toggle-theme-button)
            (when (:show-background state)
              ($ art/scene {:turns (:turns state)}))
            (if is-mobile
              ($ game/mobile-game-container)
              ($ game/game-container)))))

(defn ^:dev/after-load after-load []
  (.render @root ($ app)))

(defn run-app [container]
  (if-let [root-element @root]
    (.render root-element ($ app))
    (do
      (reset! root (rdom/createRoot container))
      (.render @root ($ app)))))

(defn ^:export init []
  (dispatch-sync [:initialize-game])
  (when-let [container (.getElementById js/document "tic-tac-toe")]
    (run-app container)))

(defn ^:export main []
  (init))