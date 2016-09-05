(ns sicp-clj.state.circuit
  (:require [sicp-clj.core :refer :all]))

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
                  (error "Invalid signal" s)))
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

(declare call-each)

(defn make-wire []
  (atom {:signal 0
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
         conj action))

(defn call-each [procedures]
  (doseq [procedure procedures]
    (procedure)))

;TODO or-gate
;TODO agenda
