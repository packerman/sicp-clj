(ns sicp-clj.state.constraints
  (:require [clojure.test :refer :all]
            [sicp-clj.core :refer :all]))

(defprotocol Constraint
  (inform-about-value [this])
  (inform-about-no-value [this]))

(defn make-connector []
  (atom {:value       nil
         :informant   nil
         :constraints #{}}))

(defn has-value? [connector] (boolean (:informant @connector)))

(defn get-value [connector] (:value @connector))

(defn- for-each-except [exception procedure list]
  (doseq [item list :when (not= item exception)]
    (procedure item)))

(defn set-value! [connector new-value informant]
  (let [{:keys [value constraints]} @connector]
    (cond
      (not (has-value? connector)) (do
                                     (swap! connector assoc
                                            :value new-value
                                            :informant informant)
                                     (for-each-except informant
                                                      inform-about-value
                                                      constraints))
      (not= value new-value) (error "Contradiction" (list value new-value))
      :else 'ignored)))

(defn forget-value! [connector retractor]
  (if (= retractor (:informant @connector))
    (do
      (swap! connector assoc :informant nil)
      (for-each-except retractor
                       inform-about-no-value
                       (:constraints @connector)))
    'ignored))

(defn connect [connector new-constraint]
  (swap! connector update :constraints conj new-constraint)
  (when (has-value? connector)
    (inform-about-value new-constraint))
  'done)

(defn adder [a1 a2 sum]
  (let [constraint (reify Constraint
                     (inform-about-value [this]
                       (cond
                         (and (has-value? a1)
                              (has-value? a2)) (set-value! sum
                                                           (+ (get-value a1) (get-value a2))
                                                           this)
                         (and (has-value? a1)
                              (has-value? sum)) (set-value! a2
                                                            (- (get-value sum) (get-value a1))
                                                            this)
                         (and (has-value? a2)
                              (has-value? sum)) (set-value! a1
                                                            (- (get-value sum) (get-value a2))
                                                            this)))
                     (inform-about-no-value [this]
                       (forget-value! sum this)
                       (forget-value! a1 this)
                       (forget-value! a2 this)
                       (inform-about-value this)))]
    (connect a1 constraint)
    (connect a2 constraint)
    (connect sum constraint)
    constraint))

(defn multiplier [m1 m2 product]
  (let [constraint (reify Constraint
                     (inform-about-value [this]
                       (cond
                         (or (and (has-value? m1)
                                  (zero? (get-value m1)))
                             (and (has-value? m2)
                                  (zero? (get-value m2)))) (set-value! product 0 this)
                         (and (has-value? m1)
                              (has-value? m2)) (set-value! product
                                                           (* (get-value m1) (get-value m2))
                                                           this)
                         (and (has-value? m1)
                              (has-value? product)) (set-value! m2
                                                                (/ (get-value product) (get-value m1))
                                                                this)
                         (and (has-value? m2)
                              (has-value? product)) (set-value! m1
                                                                (/ (get-value product) (get-value m2))
                                                                this)))
                     (inform-about-no-value [this]
                       (forget-value! product this)
                       (forget-value! m1 this)
                       (forget-value! m2 this)
                       (inform-about-value this)))]
    (connect m1 constraint)
    (connect m2 constraint)
    (connect product constraint)
    constraint))

(defn constant [value connector]
  (let [constraint (reify Constraint
                     (inform-about-value [_] (error "Unknown request: CONSTANT"))
                     (inform-about-no-value [_] (error "Unknown request: CONSTANT")))]
    (connect connector constraint)
    (set-value! connector value constraint)
    constraint))

(defn probe [name connector]
  (letfn [(print-probe [value]
            (println "Probe:" name "=" value))]
    (let [constraint (reify Constraint
                       (inform-about-value [_]
                         (print-probe (get-value connector)))
                       (inform-about-no-value [_]
                         (print-probe "?")))]
      (connect connector constraint)
      constraint)))

(defn celsius-fahrenheit-converter [c f]
  (let [u (make-connector)
        v (make-connector)
        w (make-connector)
        x (make-connector)
        y (make-connector)]
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(deftest simple-simulation
  (let [C (make-connector)
        F (make-connector)]
    (celsius-fahrenheit-converter C F)
    (probe "Celsius temp" C)
    (probe "Fahrenheit temp" F)
    (set-value! C 25 'user)
    (is (= 77 (get-value F)))
    (is (thrown? Exception (set-value! F 212 'user)))
    (is (= 77 (get-value F)))
    (forget-value! C 'user)
    (is (not (has-value? C)))
    (set-value! F 212 'user)
    (is (= 100 (get-value C)))))
