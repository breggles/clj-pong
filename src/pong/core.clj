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
  (reduce #(conj %1 (if-let [move (:move %2)]
                      (move %2 t-since-last-frm)
                      %2))
          []
          world))

(defn move-ball [ball t-since-last-frm] 
  (let [distance (map (partial * t-since-last-frm) (:velocity ball))
        new-pos (vec (map + (:position ball) distance))]
    (assoc ball :position new-pos)))

(defn bottom-right [obj]
  (map #(dec (+ %1 %2)) (:position obj) (:size obj)))

(defn bounding-box [obj]
  (vec (map int 
            (flatten [(:position obj) (bottom-right obj)]))))

(defn horz-coll? [bnd-box1 bnd-box2]
  (when (and (or (= (get bnd-box1 1) (get bnd-box2 3))
                 (= (get bnd-box1 3) (get bnd-box2 1)))
             (or (<= (get bnd-box2 0) (get bnd-box1 0) (get bnd-box2 2))
                 (<= (get bnd-box2 0) (get bnd-box1 2) (get bnd-box2 2))))
    :horz-coll))

(defn vert-coll? [bnd-box1 bnd-box2]
  (when (and (or (= (get bnd-box1 0) (get bnd-box2 2))
                 (= (get bnd-box1 2) (get bnd-box2 0)))
             (or (<= (get bnd-box2 1) (get bnd-box1 1) (get bnd-box2 3))
                 (<= (get bnd-box2 1) (get bnd-box1 3) (get bnd-box2 3))) 
             )
    :vert-coll))

(defn detect-obj-colls [idx-obj idx-others]
  (let [obj-bound (bounding-box (:obj idx-obj))] 
    (for [idx-other idx-others
          collision? [horz-coll? vert-coll?]
          :let [other-bound (bounding-box (:obj idx-other))
                collision (collision? obj-bound other-bound)]
          :when collision] 
      {:idx-objs [idx-obj idx-other] :type collision})))

(defn index-solid [idx obj]
  (when (:solid? obj) 
    {:idx idx :obj obj}))

(defn detect-collisions [world t-since-prev-frm]
  (let [provisional-world (move-objs world t-since-prev-frm)
        indexed-solids (keep-indexed index-solid provisional-world)]
    (loop [idx-obj (first indexed-solids)
           rest-idx-solids (rest indexed-solids)
           collisions []] 
      (if idx-obj
        (recur 
          (first rest-idx-solids)
          (rest rest-idx-solids)
          (concat collisions 
                  (detect-obj-colls idx-obj rest-idx-solids)))
        collisions))))

(defn collide-obj [idx-obj idx-other world]
  (let [idx (:idx idx-obj)
        obj (nth world idx)
        other (nth world (:idx idx-other))] 
    (assoc world
           idx 
           (if-let [collide (:collide obj)]
             (collide obj other)
             obj))))  

(defn apply-collision [world collision]
  (let [idx-objs (:idx-objs collision)
        frst (first idx-objs)
        scnd (second idx-objs)]
    (->> world
         (collide-obj frst scnd)
         (collide-obj scnd frst))))

(defn apply-collisions [world collisions]
  (reduce apply-collision world collisions))

; run

(let [running (atom true)
      world-atom (atom [{:id :ball
                         :position [100 100]
                         :size [10 10]
                         :velocity [0.1 -0.1] ; pixels/millisecond 
                         :solid? true
                         :paint paint-ball
                         :move move-ball
                         :collide (fn [a b] (alert "asdf"))
                         }
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
          collisions (detect-collisions world t-since-last-frm)
          new-world (-> world 
                        (apply-collisions collisions)
                        (move-objs t-since-last-frm))]
      (reset! world-atom new-world)
      (reset! last-frm-time-atom frm-time))
    (repaint! canv)
    (Thread/sleep 10)))
