(ns ricker.ricker-helpers
    (:require [ricker.ricker-config :as config]))

;; custom iterator for inclusive take-while behavior
(defn take-through
  [pred xs]
  (lazy-seq
   (when-let [[x & more] (seq xs)]
     (cons x
           (when-not (pred x)
             (take-through pred more))))))

;; append population size to state history
(defn update-history [history n]
  (->> (conj history n)
       (take-last config/history-length)
       vec))

;; helpers for finding equilibria and periodicity
(defn relative-difference [a b]
  (/ (Math/abs (- a b))
     (max 1e-5 (Math/abs a))))

(defn period?
  [state period]
  (let [values   (conj (:state-history state) (:n state))
        required (* period config/cycle-repetitions)]
    (when (>= (count values) required)
      (let [recent (vec (take-last required values))]
        (every?
         #(< % config/stability-tolerance)
         (map relative-difference
              (drop period recent)
              recent))))))

(defn cycle-period
  [state]
  (some (fn [period]
          (when (period? state period)
            period))
        (range 2 (inc config/max-cycle-period))))

;; explicitly generate r-values instead of relying on native sweep
(def r-values
  (map #(+ config/r-min (* % config/r-step))
       (range (inc (int (/ (- config/r-max config/r-min)
                           config/r-step))))))

;; scaled burn-in
(defn burn-in-steps [r]
  (if (pos? r)
    (-> (/ config/burn-in-scale r)
        Math/ceil
        long
        (max config/burn-in)
        (min config/max-burn-in))
    config/burn-in))

;; render helpers
(defn round-to [x step]
  (* step (Math/round (/ x step))))

;; bound infiite values to lyapunov limits for rendering
(defn sanitize-lambda
  [x]
  (cond
    (Double/isNaN x) nil
    (= x Double/NEGATIVE_INFINITY) config/lyapunov-floor
    (= x Double/POSITIVE_INFINITY) config/lyapunov-ceiling
    :else x))
