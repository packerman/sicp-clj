(ns sicp-clj.state.concurrency
  (:require [clojure.test :refer :all]))

(defn make-account [balance]
  (ref balance))

(defn balance [account]
  @account)

(defn withdraw [account amount]
  (if (>= @account amount)
    (alter account - amount)
    "Insufficient funds"))

(defn deposit [account amount]
  (alter account + amount))

(defn- parallel-execute [ps]
  (doseq [f (-> (map future-call ps) doall)]
    (deref f)))

(defn exchange [account1 account2]
  (dosync
    (let [difference (- (balance account1) (balance account2))]
      (withdraw account1 difference)
      (deposit account2 difference))))

(deftest account
  (testing "Basic account properties"
    (let [account (make-account 100)]
      (dosync (is (= 75 (withdraw account 25))))
      (dosync (is (= 50 (withdraw account 25))))))
  (testing "Exchange"
    (let [acc1 (make-account 100)
          acc2 (make-account 60)]
      (exchange acc1 acc2)
      (is (= 60 (balance acc1)))
      (is (= 100 (balance acc2))))))


