(ns data-optim.core
  (:gen-class)
  (:require [quil.core :as q]))


(defn eval-here [expression]
  (binding [*ns* (find-ns 'data-optim.core)]
    (eval expression)))


#_(def target-data
  (map #(vector % (+ (* % %) % 1))
       (range -1.0 1.1 0.1)))


(def target-data
    (map #(vector % (nth 
                      (map first (iterate (fn [[a b]] [b (+' a b)])
                                          [0.0 1.0])) %))
         (range 0.0 11.0 1.0)))


(def current-best
  (atom (map #(vector
                %
                ((eval-here (list 'fn '[x] 'x))
                  %))
             (range -5.0 15.0 1))))

;; And we have to define pd (protected division):


(defn pd
  "Protected division; returns 0 if the denominator is zero."
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

(def function-table (zipmap '(+ - * pd)
                            '(2 2 2 2)))

#_(def function-table (zipmap '(+ - *)
                              '(2 2 2)))

(defn random-function 
  []
  (rand-nth (keys function-table)))

(defn random-terminal
  []
  (rand-nth (list 'x (- (rand 10) 5))))

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2))) ; might want to try (rand-int (count function-table))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))


;; We can now evaluate the error of an individual by creating a function
;; built around the individual, calling it on all of the x values, and 
;; adding up all of the differences between the results and the 
;; corresponding y values.

(defn error 
  [individual]
  (let [value-function (eval-here (list 'fn '[x] individual))]
    (reduce + (map (fn [[x y]] 
                     (Math/abs 
                       (- (value-function x) y)))
                   target-data))))


;; To help write mutation and crossover functions we'll write a utility
;; function that returns a random subtree from an expression and another that
;; replaces a random subtree of an expression.

(defn codesize [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn random-subtree 
  [i]
  (if (zero? (rand-int (codesize i)))
    i
    (random-subtree 
      (rand-nth
        (apply concat
               (map #(repeat (codesize %) %)
                    (rest i)))))))

;(random-subtree '(+ (* x (+ y z)) w))

(defn replace-random-subtree
  [i replacement]
  (if (zero? (rand-int (codesize i)))
    replacement
    (let [position-to-change 
          (rand-nth 
            (apply concat
                   (map #(repeat (codesize %1) %2)
                        (rest i)
                        (iterate inc 1))))]
      (map #(if %1 (replace-random-subtree %2 replacement) %2)
           (for [n (iterate inc 0)] (= n position-to-change))
           i))))

;(replace-random-subtree '(0 (1) (2 2) (3 3 3) (4 4 4 4) (5 5 5 5 5) (6 6 6 6 6 6 6)) 'x)

;(replace-random-subtree '(+ (* x (+ y z)) w) 3)

(defn mutate
  [i]
  (replace-random-subtree i (random-code 2)))

;(mutate '(+ (* x (+ y z)) w))

(defn crossover
  [i j]
  (replace-random-subtree i (random-subtree j)))

;; We'll also want a way to sort a population by error that doesn't require 
;; lots of error re-computation:

(defn sort-by-error
  [population]
  (vec (map second
            (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
                  (map #(vector (error %) %) population)))))

;; Finally, we'll define a function to select an individual from a sorted 
;; population using tournaments of a given size.

(defn select
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

;; Now we can evolve a solution by starting with a random population and 
;; repeatedly sorting, checking for a solution, and producing a new 
;; population.

(defn evolve
  [popsize]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 4)))]
    (let [best (first population)
          best-error (error best)]
      (reset! current-best (map #(vector % ((eval-here (list 'fn '[x] best)) %))
                                (range -10.0 15.0 0.1)))
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ popsize 2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      (if (< best-error 10.0) ;; good enough to count as success
        (println "Success:" best)
        (recur 
          (inc generation)
          (sort-by-error      
            (concat
              (repeatedly (* 1/2 popsize) #(mutate (select population 7)))
              (repeatedly (* 1/4 popsize) #(crossover (select population 7)
                                                      (select population 7)))
              (repeatedly (* 1/4 popsize) #(select population 7)))))))))


(defn setup []
  (q/frame-rate 10)
  (q/background 250 250 250))


(defn draw-axes [xtot ytot xrange yrange xstep ystep x0 y0]
  ;draw the axes
  (q/background 250 250 250)
  (q/stroke 225 225 225)
  (doseq [xticks (range 0 900 xstep)
          yticks (range (mod (- 610 y0) ystep) 600 ystep)]
    ; draw the lines intersecting the y axis
    (q/line 0 yticks 900 yticks)
    ; draw the lines intersecting the x axis
    (q/line xticks 0 xticks 600))
  (q/stroke 0 0 0)
  ; draw the x-axis (where y = 0)
  (q/line 0 y0 900 y0)
  ; draw the y-axis (where x = 0)
  (q/line x0 0 x0 600))


(defn draw-target-data [xstep ystep x0 y0]  
  ;draw the target data
  (loop [td target-data
         wh 6]
    (let [tx (*  xstep (first (first td)))
          ty (* -1 ystep (second (first td)))]
      (q/fill 243 0 0)
      (q/stroke 243 0 0)  
      (q/with-translation [x0 y0]
        (q/ellipse tx ty wh wh))
      (when-not (empty? (rest (rest td)))
        (recur (rest td) wh)))))


(defn draw-current-data [xstep ystep x0 y0]
  ;draw the current data
  (loop [cd @current-best]
    (let [cx (* xstep (first (first cd)))
          cy (* -1 ystep (second (first cd)))
          cx2 (* xstep (first (second cd)))
          cy2 (* -1 ystep (second (second cd)))]
      (q/stroke 0 128 255)
      (q/with-translation [x0 y0]
        (q/line cx cy cx2 cy2))
      (when-not (empty? (rest (rest cd)))                             
        (recur (rest cd))))))


(defn draw-all []
  ;draw the axes
  (let [xtot (map first target-data)
        ytot (map second target-data)
        xrange (vector (apply min xtot) (apply max xtot))
        yrange (vector (apply min ytot) (apply max ytot))
        ; create steps that span the width of the screen (uses the extra step in calculating x0)
        xstep (/ 900 (+ (- (second xrange) (first xrange)) 2))
        ; create steps that span the height of the screen
        ystep (/ 600 (+ (- (second yrange) (first yrange)) 1))
        ; finds where x0 should be
        x0 (- 900 (* (second xrange) xstep) xstep)
        ; finds where y0 should be
        y0 (* (second yrange) ystep)]
    (draw-axes xtot ytot xrange yrange xstep ystep x0 y0)
    (draw-target-data xstep ystep x0 y0)
    (draw-current-data xstep ystep x0 y0)))


(defn -main []
  ;run sketch
  (q/defsketch Optimize
    :title "Data Optimization"
    :size [900 600]
    :setup setup
    :draw draw-all)
  (evolve 1000))
