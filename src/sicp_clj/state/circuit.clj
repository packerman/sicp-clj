(ns sicp-clj.state.circuit
  (:require [sicp-clj.core :refer :all]
            [clojure.test :refer :all])
  (:import (clojure.lang PersistentQueue)))

(declare make-wire
         or-gate
         inverter
         and-gate)

(defn half-adder [a b s c]
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defn full-adder [a b c-in sum c-out]
  (let [c1 (make-wire)
        c2 (make-wire)
        s (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(declare get-signal
         set-signal!
         add-action!
         after-delay)

(def ^:const inverter-delay 2)

(defn inverter [input output]
  (letfn [(logical-not [s]
            (cond (= s 0) 1
                  (= s 1) 0
                  :else (error "Invalid signal" s)))
          (invert-input []
            (let [new-value (logical-not (get-signal input))]
              (after-delay
                inverter-delay
                (fn []
                  (set-signal! output new-value)))))]
    (add-action! input invert-input)
    'ok))

(def ^:const and-gate-delay 3)

(defn and-gate [a1 a2 output]
  (letfn [(logical-and [s t]
            (condp = [s t]
              [1 1] 1 [1 0] 0
              [0 1] 0 [0 0] 0
              (error "Invalid signal" [s t])))
          (and-action []
            (let [new-value (logical-and (get-signal a1)
                                         (get-signal a2))]
              (after-delay
                and-gate-delay
                (fn []
                  (set-signal! output new-value)))))]
    (add-action! a1 and-action)
    (add-action! a2 and-action)
    'ok))

(def ^:const or-gate-delay 5)

(defn or-gate [o1 o2 output]
  (letfn [(logical-or [s t]
            (condp = [s t]
              [1 1] 1 [1 0] 1
              [0 1] 1 [0 0] 0
              (error "Invalid signal" [s t])))
          (and-action []
            (let [new-value (logical-or (get-signal o1)
                                        (get-signal o2))]
              (after-delay
                or-gate-delay
                (fn []
                  (set-signal! output new-value)))))]
    (add-action! o1 and-action)
    (add-action! o2 and-action)
    'ok))

(declare call-each)

(defn make-wire []
  (atom {:signal  0
         :actions '()}))

(defn get-signal [wire]
  (:signal @wire))

(defn set-signal! [wire new-value]
  (let [{:keys [signal actions]} @wire]
    (if (not= signal new-value)
      (do
        (swap! wire assoc :signal new-value)
        (call-each actions))
      'done)))

(defn add-action! [wire action]
  (swap! wire
         update :actions
         conj action)
  (action))

(defn call-each [procedures]
  (doseq [procedure procedures]
    (procedure)))

(declare current-time the-agenda add-to-agenda! empty-agenda?
         make-agenda first-agenda-item remove-first-agenda-item!)

(defn after-delay [delay action]
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(defn propagate []
  (if (empty-agenda? the-agenda)
    'done
    (let [first-item (first-agenda-item the-agenda)]
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (recur))))

(defn probe [name wire]
  (add-action!
    wire
    (fn []
      (println)
      (println name (current-time the-agenda) "New-value =" (get-signal wire)))))

(defn make-agenda []
  (atom
    {:current-time 0
     :queues       (sorted-map)}))

(defn current-time [agenda]
  (:current-time @agenda))

(defn set-current-time! [agenda time]
  (swap! agenda assoc :current-time time))

(defn empty-agenda? [agenda]
  (empty? (:queues @agenda)))

(defn add-to-agenda! [time action agenda]
  (swap! agenda update-in [:queues time]
         (fn [queue]
           (conj (or queue (PersistentQueue/EMPTY))
                 action))))

(defn remove-first-agenda-item! [agenda]
  (let [[time queue] (first (:queues @agenda))
        new-queue (pop queue)]
    (if (empty? new-queue)
      (swap! agenda update :queues dissoc time)
      (swap! agenda assoc-in [:queues time] new-queue))))

(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
    (error "Agenda is empty")
    (let [[time queue] (first (:queues @agenda))]
      (set-current-time! agenda time)
      (peek queue))))

(def the-agenda (make-agenda))

(deftest simulation
  (testing "Simple simulation"
    (let [input-1 (make-wire)
          input-2 (make-wire)
          sum (make-wire)
          carry (make-wire)]
      (probe 'sum sum)
      (probe 'carry carry)
      (half-adder input-1 input-2 sum carry)
      (set-signal! input-1 1)
      (propagate)
      (set-signal! input-2 1)
      (propagate))))
