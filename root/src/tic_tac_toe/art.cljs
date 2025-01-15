(ns tic-tac-toe.art
  (:require [helix.core :refer [defnc $]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            [react :as react]))

(def SPACE_RADIUS 500)
(def DEFAULT_OPACITY 25)
(defn peak-opacity [value]
  (- 1 (* 2 (Math/abs (- value 0.5)))))

(def dark-purple [29 18 22])
(def navy [25 8 63])
(def sky-blue [135 206 235])
(def light-blue [173 216 230])
(def twilight-peach [235 153 135])
(def white [255 255 255])
(def yellow [255 255 0])

(def test-colors [[255 0 0] [0 255 0] [0 0 255] [255 255 0][135 206 235][255 255 255]])
(def sky-colors [navy
                 sky-blue
                 light-blue
                 sky-blue
                 twilight-peach
                 dark-purple])

(def num-sky-colors (- (count sky-colors) 1))
(def segment-margins (float (/ (/ 1 num-sky-colors ) 2)))

(defn ring-template [y1 y2 stroke-width]
  (d/svg
   (d/line 
    {:x1  "0"  :x2 "100%" 
      :y1 y1 :y2 y2 
      :stroke "rgba(255,255,255,0.2)"
      :stroke-width stroke-width})))

(defn rings []
  (d/svg {:id "rings"
          :width "100%"
          :height "100%"}
         (ring-template "51%" "69%" "4")
         (ring-template "54%" "70%" "2.5")
         (ring-template "57%" "70%" "2")
         (ring-template "59%" "70.5%" "1")
         (ring-template "63%" "70.75%" "0.5")))


(defn sky-background []
  (d/svg {:id "background-texture"
          :width "100%"
          :height "100%"}
         (d/defs
           (helix.core/create-element
            "filter"
            #js {:id "sky"
                 :x "0"
                 :y "0"
                 :width "100%"
                 :height "100%"}
            (helix.core/create-element
             "feTurbulence"
             #js {:type "fractalNoise"
                  :baseFrequency "0.01"
                  :numOctaves "5"
                  :result "noise"})
            (helix.core/create-element
             "feDiffuseLighting"
             #js {:in "noise"
                  :lightingColor "#1a1147"
                  :surfaceScale "5"}
             (helix.core/create-element
              "feDistantLight"
              #js {:azimuth "45"
                   :elevation "60"}))))
         (d/rect
          {:width "100%"
           :height "100%"
           :fill "#87CEEB"
           :filter "url(#sky)"})))

(defn deep-space []
  (d/svg {:id "deep-space-texture"
          :width "100%"
          :height "100%"}
         (d/defs
           (helix.core/create-element
            "filter"
            #js {:id "deep-space" 
                 :x "0"
                 :y "0"
                 :width "100%"
                 :height "100%"}

        ;; Strong effect
            (helix.core/create-element
             "feTurbulence"
             #js {:type "fractalNoise"
                  :baseFrequency ".7" 
                  :numOctaves "3"    
                  :result "strong-noise"})
            (helix.core/create-element
             "feDiffuseLighting"
             #js {:in "strong-noise"
                  :lightingColor "#2a1f3155"
                  :surfaceScale "5"      
                  :result "strong-lighting"}
             (helix.core/create-element
              "feDistantLight"
              #js {:azimuth "45"
                   :elevation "10"}))
            (helix.core/create-element
             "feGaussianBlur"
             #js {:in "strong-lighting" 
                  :stdDeviation "1.75"
                  :result "blurred-lighting"})

        ;Subtle effect
            (helix.core/create-element
             "feTurbulence"
             #js {:type "fractalNoise"
                  :baseFrequency "0.02"  
                  :numOctaves "1"      
                  :result "subtle-noise"})
            (helix.core/create-element
             "feDiffuseLighting"
             #js {:in "subtle-noise"
                  :lightingColor "#c5f3e855"
                  :surfaceScale "2"    
                  :result "subtle-lighting"}
             (helix.core/create-element
              "feDistantLight"
              #js {:azimuth "45"
                   :elevation "0"}))

        ;Create gradient for blending
            (helix.core/create-element
             "feComponentTransfer"
             #js {:result "blend-mask"}
             (helix.core/create-element
              "feFuncA"
              #js {:type "linear"
                   :slope "-1"
                   :intercept "1"}))

            (helix.core/create-element
             "feComposite"
             #js {:in "blurred-lighting"
                  :in2 "blend-mask"
                  :operator "arithmetic"
                  :k1 "0"
                  :k2 "1"
                  :k3 "0"
                  :k4 "0"
                  :result "masked-strong"})

            (helix.core/create-element
             "feComposite"
             #js {:in "subtle-lighting"
                  :in2 "blend-mask"
                  :operator "arithmetic"
                  :k1 "1"
                  :k2 "-1"
                  :k3 "0"
                  :k4 "1"
                  :result "masked-subtle"})

        ;Combine the masked versions
            (helix.core/create-element
             "feComposite"
             #js {:in "masked-strong"
                  :in2 "masked-subtle"
                  :operator "arithmetic"
                  :k1 ".1"
                  :k2 "1"
                  :k3 "0"
                  :k4 "0"
                  :result "final"})))
         (d/rect
          {:width "100%"
           :height "100%"
           :fill "#ffffff01"
           :filter "url(#deep-space)"})))

(defn space-lighting [angle]
  (let [lighting-x (str (+ 50 (* SPACE_RADIUS 1.8 (Math/cos angle))) "%")
        lighting-y (str (- 50 (* SPACE_RADIUS 0.8 (Math/sin angle))) "%")]
    (d/svg {:id "space-lighting"
            :width "100%"
            :height "100%"}
           (d/radialGradient
            {:id "inner-to-outer"
             :cx lighting-x
             :cy lighting-y
             :r "1000%"
             :fx "50%"
             :fy "66%"
             :spreadMethod "pad"}
            (d/stop {:offset "0%"
                     :style {:stop-color "#3f2bea00" :stop-opacity 0.8}})
            (d/stop {:offset "20%"
                     :style {:stop-color "#3f2bea12" :stop-opacity 0.6}})
            (d/stop {:offset "50%"
                     :style {:stop-color "#4f237eff" :stop-opacity 0.3}})
            (d/stop {:offset "80%"
                     :style {:stop-color "#3a1a45ff" :stop-opacity 0.1}})
            (d/stop {:offset "100%"
                     :style {:stop-color "#1a116919" :stop-opacity 0}}))
           (d/rect
            {:width "100%"
             :height "100%"
             :fill "url(#inner-to-outer)"}))))

(defn stars []
  (d/svg  {:id "p-matrix"
           :width "100%"
           :height "100%"}
          (d/defs
            (helix.core/create-element
             "filter"
             #js {:id "matrix-filter"
                  :x "0"
                  :y "0"
                  :width "100%"
                  :height "100%"}

             (helix.core/create-element
              "feTurbulence"
              #js {:baseFrequency ".2"
                   #_#_:seed (rand-int 100)
                   :result "strong-noise"})

                   

             (helix.core/create-element
              "feColorMatrix"
              #js {:type "matrix"
                   :in "strong-noise"
                   :values "0 0 0 9 -4    
                           0 0 0 9 -4    
                           0 0 0 9 -4   
                           0 0 0 0 1"
                   :result "matrix-result"})))
          (d/rect
           {:width "100%"
            :height "100%"
            :filter "url(#matrix-filter)"})))

(defn interpolate-color
  "Returns an rbg value in the form of a string based on two points and a varying midpoint."
  [color1 color2 value opacity-value]                               ;[[135 206 235] [25 25 112] .6]
   (let [start-r (nth color1 0)
         start-g (nth color1 1)
         start-b (nth color1 2)
         end-r (nth color2 0)
         end-g (nth color2 1)
         end-b (nth color2 2)
         r (+ start-r (* value (- end-r start-r)))      ;(+ 135 ( * .5 (- 25 135)))
         g (+ start-g (* value (- end-g start-g)))      ;         (-115*.6) = -69
         b (+ start-b (* value (- end-b start-b)))]     ;(+ 135 -69)        = 46
     (str "rgba(" (int r) "," (int g) "," (int b) ", " opacity-value ")"))) ; returns "rgb(46, n2, n3)"

;If I wanted to do a binary sliding gradient
(defn binary-color-interpolation 
  ([slider-value start-color end-color]
   (interpolate-color start-color end-color slider-value DEFAULT_OPACITY))
  ([slider-value start-color end-color opacity-fn]
  (interpolate-color start-color end-color slider-value opacity-fn)))

(defn triple-point-interpolation
  ([slider-value color1 color2 color3]
   (triple-point-interpolation slider-value color1 color2 color3 DEFAULT_OPACITY))
  ([slider-value color1 color2 color3 opacity-fn]
   (if (< slider-value 0.5)
     (let [adjusted-progress (* slider-value 2)]
       (interpolate-color color1 color2 adjusted-progress opacity-fn))
     (let [adjusted-progress (* (- slider-value 0.5) 2)]
       (interpolate-color color2 color3 adjusted-progress opacity-fn)))))

(defn test-o-color []
  (nth sky-colors (mod (dec 3) num-sky-colors)))

(defn n-color-interpolation [slider-value]
  (let [step (/ 1.0 num-sky-colors)
        index (float (/ slider-value step))
        progress (mod slider-value step)
        color1 (nth sky-colors index)
        color2 (nth sky-colors (mod (inc index) num-sky-colors))]
    (cond
      (< progress 0.0) (interpolate-color color1 color2 0 DEFAULT_OPACITY)
      (< progress 1.0) (interpolate-color color1 color2 progress DEFAULT_OPACITY)
      :else (interpolate-color color2 color1 slider-value DEFAULT_OPACITY))))

  (defn solar-lunar-body
    "The Sun/Moon, pulling double duty."
    [angle current-color]
    (let [sun-moon-x (+ 50 (* SPACE_RADIUS 1.65 (Math/cos angle)))
          sun-moon-y (- 50 (* SPACE_RADIUS 1 (Math/sin angle)))]
      (d/svg {:id "sun"}
             (d/radialGradient
              {:id "sun-gradient"
               :cx "50%" :cy "50%" :r "50%" :fx "50%" :fy "50%"}
              (d/stop {:offset "0%" :style {:stop-color current-color :stop-opacity 1}})
              (d/stop {:offset "20%" :style {:stop-color "#2d2bf4f5" :stop-opacity 0.29}})
              (d/stop {:offset "85%" :style {:stop-color "#2f2bea20" :stop-opacity 0.05}})
              (d/stop {:offset "100%" :style {:stop-color "#1f2b1ae0" :stop-opacity 0}}))
             (d/circle
              {:cx sun-moon-x
               :cy sun-moon-y
               :r 100
               :fill "url(#sun-gradient)"}))))

  (defn solar-lunar-slider
    "Slider to control the solar-lunar-body."
    [slider-value set-slider-value]
    (d/input
     {:id "solar-lunar-slider"
      :type "range" :min 0 :max 1 :step 0.01 :value slider-value
      :on-change #(set-slider-value (js/parseFloat (.. % -target -value)))}))

  (defnc scene [{:keys [turns]}]
    (let [[slider-value set-slider-value] (react/useState 0.0)
          angle (* Math/PI (- 1 slider-value))
          sky-color (if (< slider-value 0.5)
                      (triple-point-interpolation slider-value
                                                  navy
                                                  sky-blue
                                                  light-blue
                                                  (peak-opacity slider-value))
                      (triple-point-interpolation slider-value
                                                  light-blue
                                                  sky-blue
                                                  dark-purple
                                                  (peak-opacity slider-value)))
          #_#_sky-color (n-color-interpolation slider-value)
          celestial-color (triple-point-interpolation slider-value
                                                      white
                                                      yellow
                                                      white)]
      (hooks/use-effect
       [turns]
       (when (number? turns)
         (set-slider-value (if (= turns 9)
                             1.0
                             (mod (/ turns 9) 1.0)))))
      (d/div {:id "scene"
              :class "fixed inset-0 w-full h-full"
              :style {:background sky-color}} 
             (sky-background)
             (space-lighting angle)
             (stars)
             (rings)
     ;The Sun/Moon        
             (solar-lunar-body angle celestial-color)
     ;Slider
             (solar-lunar-slider slider-value set-slider-value))))