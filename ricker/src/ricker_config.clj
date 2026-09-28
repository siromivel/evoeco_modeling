(ns ricker-config)

(def max-steps 150)
(def history-length 32)
(def stability-tolerance 1.0e-6)
(def cycle-repetitions 3)
(def max-cycle-period 8)
(def initial-state
  {:n 100.0
   :r 2.2
   :k 500.0
   :state-history []})
(def r-min 0.0)
(def r-max 4.0)
(def r-step 0.005)
(def timeseries-r-values [1.5 2.2 2.6 3.0])

(def burn-in 500)            ; minimum
(def burn-in-scale 15.0)     ; ~e^-15 residual transient
(def max-burn-in 50000)      ; cap; also covers r = 0

(def samples 1000)

(def lyapunov-floor -10.0)
(def lyapunov-ceiling 10.0)

(def vega-lite-schema
  "https://vega.github.io/schema/vega-lite/v5.json")
