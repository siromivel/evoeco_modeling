(ns ricker-model
    (:require [ricker-config :as config])
    (:require [ricker-helpers :as helpers]))

;; Basic Ricker model
;; where n = initial population size
;;       r = reproduction rate
;;       k = carrying capacity
(defn ricker
  [n r k]
  (* n
     (Math/exp
      (* r (- 1.0 (/ n k))))))

;; d(R)/dn
(defn ricker-prime
  [n r k]
  (* (Math/exp
      (* r (- 1.0 (/ n k))))
     (- 1.0 (* r (/ n k)))))

(defn step
  [{:keys [n r k state-history] :as state}]
  (assoc state
         :n (ricker n r k)
         :state-history (helpers/update-history state-history n)))

;; Generate bifurcations and compute lyapunov exps
(defn bifurcation-values
  [r-value]
  (->> (iterate #(ricker
                  %
                  r-value
                  (:k config/initial-state))
                (:n config/initial-state))
       (drop (helpers/burn-in-steps r-value))
       (take config/samples)))

(defn lyapunov-exponent
  [r-value]
  (let [k (:k config/initial-state)
        n0 (:n config/initial-state)
        trajectory
        (->> (iterate #(ricker % r-value k) n0)
             (drop (helpers/burn-in-steps r-value))
             (take config/samples))]
    (/ (reduce +
               (map (fn [n]
                      (Math/log
                       (Math/abs
                        (ricker-prime n r-value k))))
                    trajectory))
       config/samples)))

;; Look for extinction, equilibria and cycling behavior
(defn extinct?
  [{:keys [n]}]
  (< n 1e-10))

(defn equilibrium?
  ([state] (equilibrium? state config/stability-tolerance))
  ([{:keys [n k]} epsilon]
   (< (helpers/relative-difference k n) epsilon)))

(defn cycling?
  [state]
  (some? (helpers/cycle-period state)))

(defn terminal? [state]
  (or (extinct? state)
      (equilibrium? state)
      (cycling? state)))

;; run model through terminal conditions or step limit
(defn trajectory [r]
  (->> (iterate step (assoc config/initial-state :r r))
       (take config/max-steps)
       (helpers/take-through terminal?)
       vec))

(defn outcome [traj]
  (let [s (peek traj)]
    (cond
      (extinct? s)     "extinct"
      (equilibrium? s) "equilibrium"
      :else (if-let [p (helpers/cycle-period s)]
              (str "period " p)
              "no cycle detected"))))
