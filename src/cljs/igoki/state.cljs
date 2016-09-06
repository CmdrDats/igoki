(ns igoki.state
  (:require-macros
    [reagent.ratom :as ra])
  (:require
    [re-frame.core :as rf]
    [reagent.ratom :as ra]
    [reagent.core :as r]
    [igoki.comms :as comms]
    [schema.core :as s]))


(comment
  (def Game
    {:id       s/Num
     :cameraid s/Num})

  (def Camera
    {:id        s/Num
     :slot      s/Num
     :viewcount s/Num
     :viewing   (s/enum :opening :start :stop :error)
     :corners   [[s/Num]]})

  (def Config
    {:visible s/Bool
     :tab     (s/enum :info :online :camera)})

  (def DbState
    {:current Game
     :config  Config
     :games   [Game]
     :cameras {s/Num Camera}}))

;; Game camera management
(rf/register-handler
  :init
  (fn [db]
    (assoc db
      :current {}
      :config {:tab :info :visible false}
      :games []
      :cameras {})))

(rf/register-handler
  :camera/list
  (fn [db _]
    (println "Requesting camera list")
    (comms/send :camera/list {})
    db))

(rf/register-handler
  :camera/select
  (fn [db [_ id]]
    (println "Selected Camera: " id)
    (comms/send :camera/select {})
    (assoc-in db [:forms :config :selected-camera] id)))

(rf/register-handler
  :camera/updatelist
  (fn [db [ev cameralist :as e]]
    (println "Update cameras: " (pr-str e))
    (assoc db :cameras cameralist)))

(rf/register-handler
  :camera/new
  (fn [db _]
    (comms/send :camera/new {})
    db))

(rf/register-handler
  :camera/remove
  (fn [db [_ id]]
    (comms/send :camera/remove {:id id})
    db))

(rf/register-handler
  :camera/corner-move
  (fn [db [_ pt]]
    db))

(rf/register-sub :cameralist (fn [db _] (ra/reaction (:cameras @db))))

;; Form management
(rf/register-handler
  :form/update
  (fn [db [_ form-id path value]]
    (println "Updating " (concat [:forms form-id] path) " to  " value)
    (assoc-in db (vec (concat [:forms form-id] path)) value)))

(rf/register-sub :forms (fn [db _] (ra/reaction (:forms @db))))



(comment


  (rf/register-handler
    :camera/next
    (fn [db _]
      (let [slot (get-in db [:camera :slot] 1)]
        (comms/send :camera/next slot))))

  (rf/register-handler
    :camera/close
    (fn [db _]
      (comms/send :camera/close {:gameid (-> db :currrent :id)})
      (rf/dispatch [:camera/stop-view])
      ))

  (rf/register-handler
    :camera/view
    (rf/path :current)
    (fn [game type]
      (when (#{:start :stop} type)
        (comms/send :camera/view {:gameid (-> db :current :id) :type type}))
      (assoc-in db [:camera :viewing] type)))

  (rf/register-handler
    :camera/update-view
    (fn [db _]
      (update-in db [:current :camera :viewcount] (fnil inc 0)))))

