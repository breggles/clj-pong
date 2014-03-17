(ns pong.core
  (:require [clj-time.core :as t])
  (:use seesaw.core
        seesaw.graphics
        seesaw.color
        seesaw.keymap))

; helpers

(defn x-coord [pos-vec]
  (first pos-vec))

(defn y-coord [pos-vec]
  (second pos-vec))

(defn calc-frm-rate [last-frm-time now]
  (let [time-diff (t/in-millis 
                    (t/interval last-frm-time now))]
    (/ 1000 time-diff)))

; paint

(defn paint-world [canv g world]
  (doseq [obj world]
    (when-let [paint (:paint obj)]
      (paint canv g obj))))

(defn paint-ball [canv g obj]
  (let [pos (:position obj)] 
    (draw g 
      (rect (x-coord pos) (y-coord pos) 10 10)
      (style :foreground :black))))

(defn paint-fps-indicator [canv g last-frm-time-atom]
  (let [now (t/now)
        frm-rate (calc-frm-rate @last-frm-time-atom now)] 
    (draw g 
      (string-shape 400 450 (str "FPS:" (int frm-rate)))
      (style :foreground :black))))

; move

(defn move-objs [world t-since-last-frm]
  (loop [obj (first world)
         rest-world (rest world) 
         new-world []]
    (if obj
      (let [new-obj (if-let [move (:move obj)]
                      (move obj world t-since-last-frm)
                      obj)]
        (recur 
          (first rest-world)
          (rest rest-world)
          (conj new-world new-obj)))
      new-world)))

(defn move-ball [ball world t-since-last-frm] 
  (let [other-solid-objs (remove #(and (= % ball) (:solid? %)) 
                                 world)
        distance (map (partial * t-since-last-frm) (:velocity ball))
        new-pos (vec (map + (:position ball) distance))
        collision? (<= (y-coord new-pos) 0)
        new-ball (if collision?
                   (let [vel (:velocity ball)] 
                     (assoc-in ball [:velocity 1] (* (y-coord vel) -1)))
                   ball)]
    (assoc new-ball :position new-pos)))

; run

(let [running (atom true)
      world-atom (atom [{:id :ball
                         :position [100 100]
                         :velocity [0.1 -0.1] ; pixels/millisecond
                         :solid? true
                         :paint paint-ball
                         :move move-ball}
                        {:id :top-wall
                         :position [0 -1]
                         :size [1 500]
                         :solid? true}])
      last-frm-time-atom (atom (t/now))
      canv (canvas 
             :paint (fn [cnv g] 
                      (paint-world cnv g @world-atom)
                      ;(paint-fps-indicator cnv g last-frm-time-atom)
                      )
             :focusable? true)
      frm (frame
            :title "Pong"
            :width 500
            :height 500 
            :resizable? false
            :content canv
            :on-close :dispose)]
  (listen frm :window-closing (fn [_] (reset! running false)))
  (-> frm show!)
  (while @running
    (let [frm-time (t/now)
          t-since-last-frm (t/in-millis
                             (t/interval @last-frm-time-atom frm-time))
          world @world-atom
          new-world (move-objs world t-since-last-frm)]
      (reset! world-atom new-world)
      (reset! last-frm-time-atom frm-time))
    (repaint! canv)
    (Thread/sleep 10)))
