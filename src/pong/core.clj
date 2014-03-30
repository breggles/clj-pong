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

(defn get-by-id-indexed [world id]
  (first 
    (keep-indexed #(when (= id (:id %2)) [%1 %2]) world)))

; paint

(defn paint-world [canv g world]
  (doseq [obj world]
    (when-let [paint (:paint obj)]
      (paint canv g obj))))

(defn paint-rect 
  ([canv g obj]
   (paint-rect canv g obj :black))
  ([canv g obj colour]
    (let [pos (:position obj)
          size (:size obj)] 
      (draw g 
        (rect (x-coord pos) (y-coord pos) (x-coord size) (y-coord size))
        (style :foreground colour)))))

(defn paint-wall [canv g obj]
  (paint-rect canv g obj :red))

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

(defn move-obj [ball t-since-last-frm] 
  (let [distance (map (partial * t-since-last-frm) (:velocity ball))
        new-pos (vec (map + (:position ball) distance))]
    (assoc ball :position new-pos)))

(defn bottom-right [obj]
  (map + (:position obj) (:size obj)))

(defn bounding-box [obj]
  (vec (map #(Math/round (double %)) 
            (flatten [(:position obj) (bottom-right obj)]))))

(defn horz-coll? [bnd-box1 bnd-box2]
  (when (and (or (= (get bnd-box1 1) (get bnd-box2 3))
                 (= (get bnd-box1 3) (get bnd-box2 1)))
             (or (<= (get bnd-box2 0) (get bnd-box1 0) (get bnd-box2 2))
                 (<= (get bnd-box2 0) (get bnd-box1 2) (get bnd-box2 2))
                 (<= (get bnd-box1 0) (get bnd-box2 0) (get bnd-box1 2))))
    :horz-coll))

(defn vert-coll? [bnd-box1 bnd-box2]
  (when (and (or (= (get bnd-box1 0) (get bnd-box2 2))
                 (= (get bnd-box1 2) (get bnd-box2 0)))
             (or (<= (get bnd-box2 1) (get bnd-box1 1) (get bnd-box2 3))
                 (<= (get bnd-box2 1) (get bnd-box1 3) (get bnd-box2 3))
                 (<= (get bnd-box1 1) (get bnd-box2 1) (get bnd-box1 3))))
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

(defn collide-paddle [paddle other coll-type]
  (if (some #{(:id other)} [:top-wall :bottom-wall])
    (assoc-in paddle [:velocity 1] 0)
    paddle))

(defn collide-ball [ball other coll-type] 
  (let [vel (:velocity ball)] 
    (assoc ball :velocity (case coll-type
                           :horz-coll [(x-coord vel) 
                                       (* -1 (y-coord vel))]
                           :vert-coll [(* -1 (x-coord vel))
                                       (y-coord vel)]))))

(defn collide-obj [idx-obj idx-other coll-type world]
  (let [idx (:idx idx-obj)
        obj (nth world idx)
        other (nth world (:idx idx-other))] 
    (assoc world
           idx 
           (if-let [collide (:collide obj)]
             (collide obj other coll-type)
             obj))))  

(defn apply-collision [world collision]
  (let [idx-objs (:idx-objs collision)
        frst (first idx-objs)
        scnd (second idx-objs)
        coll-type (:type collision)]
    (->> world
         (collide-obj frst scnd coll-type)
         (collide-obj scnd frst coll-type))))

(defn apply-collisions [world collisions]
  (reduce apply-collision world collisions))

; run

(defn update-world [world t-since-last-frm]
  (let [collisions (detect-collisions world t-since-last-frm)]
    (-> world 
        (apply-collisions collisions)
        (move-objs t-since-last-frm))))

(defn update-paddle-vel [world id vert-velocity]
  (let [idx-paddle (get-by-id-indexed world id)
        idx (first idx-paddle)]
    (assoc-in world [idx :velocity 1] vert-velocity)))

(defn key-pressed [world keyChar] 
  (case keyChar 
    \k (update-paddle-vel world :right-paddle  0.1) 
    \i (update-paddle-vel world :right-paddle -0.1) 
    \s (update-paddle-vel world :left-paddle   0.1)  
    \w (update-paddle-vel world :left-paddle  -0.1)
    world))

(defn key-released [world keyChar] 
  (case keyChar 
    \k (update-paddle-vel world :right-paddle 0) 
    \i (update-paddle-vel world :right-paddle 0) 
    \s (update-paddle-vel world :left-paddle  0)  
    \w (update-paddle-vel world :left-paddle  0)
    world))

(defn key-listen! [e world-atom handler]
  (let [keyChar (.getKeyChar e)] 
    (swap! world-atom handler keyChar)))

(defn init-canvas [world-atom run?-atom] 
  (let [canv (canvas 
               :paint (fn [cnv g] 
                        (paint-world cnv g @world-atom)
                        ;(paint-fps-indicator cnv g last-frm-time-atom)
                        )
               :focusable? true)
         frm (frame
               :title "Pong"
               :width 500
               :height 500 
               ;:resizable? false
               :content canv
               :on-close :dispose)]
    (listen frm :window-closing (fn [_] (reset! run?-atom false)))
    (listen canv :key-pressed (fn [e] (key-listen! e world-atom key-pressed)))
    (listen canv :key-released (fn [e] (key-listen! e world-atom key-released)))
    (.requestFocusInWindow canv)
    (show! frm)
    ;(.setExtendedState frm javax.swing.JFrame/MAXIMIZED_BOTH)
    ; wait for maximization
    (Thread/sleep 200)
    canv))

(defn init-world [world-atom canv]
  (let [canv-width (width canv)
        canv-height (height canv)]
    [{:id :ball
      :position [100 100]
      :size [10 10]
      :velocity [0.1 -0.1] ; pixels/millisecond 
      :solid? true
      :paint paint-rect
      :move move-obj
      :collide collide-ball}
     {:id :top-wall
      :position [0 0]
      :size [canv-width 0]
      :paint paint-wall
      :solid? true}
     {:id :right-wall
      :position [canv-width 0]
      :size [0 canv-height]
      :paint paint-wall
      :solid? true}
     {:id :bottom-wall
      :position [0 (- canv-height 2)]
      :size [canv-width 0]
      :paint paint-wall
      :solid? true}
     {:id :left-wall
      :position [0 0]
      :size [0 canv-height]
      :paint paint-wall
      :solid? true}
     {:id :right-paddle
      :position [(- canv-width 50) (- (/ canv-height 2) 35)]
      :size [10 70]
      :velocity [0 0]
      :solid? true
      :paint paint-rect
      :move move-obj
      :collide collide-paddle}  
     {:id :left-paddle
      :position [50 (- (/ canv-height 2) 35)]
      :size [10 70]
      :velocity [0 0]
      :solid? true
      :paint paint-rect
      :move move-obj
      :collide collide-paddle}]))

(let [run?-atom (atom true)
      last-frm-time-atom (atom (t/now))
      world-atom (atom nil)
      canv (init-canvas world-atom run?-atom)]
  (swap! world-atom init-world canv)
  (reset! last-frm-time-atom (t/now)) 
  (while @run?-atom
    (let [frm-time (t/now)
          t-since-last-frm (t/in-millis
                             (t/interval @last-frm-time-atom frm-time))
          world @world-atom]
      (swap! world-atom update-world t-since-last-frm)
      (reset! last-frm-time-atom frm-time))
    (repaint! canv)
    (Thread/sleep 1)))
