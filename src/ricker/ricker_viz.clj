(ns ricker.ricker-viz
  (:require [ricker.ricker-config :as config]
            [ricker.ricker-helpers :as helpers]
            [ricker.ricker-model :as ricker]
            [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.kind :as kind]))

;; data prep
;; bifurcation points are rounded before de-duplication; one pixel on the
;; y-axis is already ~6 population units, so 1.0 costs nothing visually
(def bifurcation-resolution 1.0)

(defn lyapunov-data []
  (->> helpers/r-values
       (map (fn [r-value]
              {:r r-value
               :lambda (helpers/sanitize-lambda
                        (ricker/lyapunov-exponent r-value))}))
       (remove (comp nil? :lambda))))

(defn bifurcation-data []
  (mapcat
   (fn [r-value]
     (->> (ricker/bifurcation-values r-value)
          (map #(helpers/round-to % bifurcation-resolution))
          distinct
          (map (fn [n] {:r r-value :n n}))))
   helpers/r-values))

(defn timeseries-data []
  (mapcat
   (fn [r-value]
     (let [traj  (ricker/trajectory r-value)
           label (str "r = " r-value " · " (ricker/outcome traj))]
       (map-indexed (fn [t {:keys [n]}]
                      {:r r-value :label label :t t :n n})
                    traj)))
   config/timeseries-r-values))

;; chart specs
(def panel-size
  {:height 400
   :width 500})

(def r-axis
  {:field :r
   :type :quantitative
   :title "Reproduction rate (r)"})

(defn quantitative-axis
  [field title]
  {:field field
   :type :quantitative
   :title title})

(def carrying-capacity-rule
  {:mark {:type :rule
          :strokeDash [6 4]}
   :encoding
   {:y {:datum (:k config/initial-state)}}})

(defn parameter-sweep-spec
  [data body]
  (merge
   {:data {:values data}}
   panel-size
   body))

(defn bifurcation-spec [data]
  (parameter-sweep-spec
   data
   {:mark {:type :point
           :filled true
           :size 3
           :opacity 0.4}
    :encoding
    {:x r-axis
     :y (quantitative-axis :n "Population size")}}))

(defn lyapunov-spec [data]
  (parameter-sweep-spec
   data
   {:layer
    [{:mark {:type :line}
      :encoding
      {:x r-axis
       :y (quantitative-axis :lambda "Lyapunov exp")}}

     {:mark {:type :rule
             :strokeDash [6 4]}
      :encoding
      {:y {:datum 0.0}}}]}))

(defn timeseries-spec [data]
  {:data {:values data}
   :resolve {:scale {:x :independent}}
   :facet
   {:column {:field :label
             :type :nominal
             :title nil
             :sort {:field :r :op :min}}}
   :spec
   {:width 230
    :height 150
    :layer
    [{:mark {:type :line}
      :encoding
      {:x (quantitative-axis :t "Time step")
       :y (quantitative-axis :n "Population size")}}

     {:mark {:type :point
             :filled false
             :size 30}
      :encoding
      {:x {:field :t :type :quantitative}
       :y {:field :n :type :quantitative}}}

     carrying-capacity-rule]}})

(defn combined-spec []
  (kind/vega-lite
   {:$schema config/vega-lite-schema
    :vconcat
    [{:hconcat [(bifurcation-spec (bifurcation-data))
                (lyapunov-spec (lyapunov-data))]
      :spacing 30
      :resolve {:scale {:y :independent}}}
     (timeseries-spec (timeseries-data))]
    :spacing 40}))

;; entry points / usage
(defn render! []
  (clay/make! {:single-value (combined-spec)}))

(defn -main [& _]
  (render!))

(comment
  (render!)
  (take 5 (lyapunov-data))
  (count (bifurcation-data))
  (ricker/outcome (ricker/trajectory 2.6)))
