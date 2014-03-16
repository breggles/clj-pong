(ns pong.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color
        seesaw.keymap))

; paint

(defn paint-world [canv g world-atom]
  (doseq [obj @world-atom]
    (when-let [paint (:paint obj)]
      (paint canv g obj))))

(defn paint-ball [canv g obj]
  (let [pos (:position obj)] 
    (draw g 
      (rect (first pos) (second pos) 10 10)
      (style :foreground java.awt.Color/BLACK))))

; move

(defn move-objs [world]
  (loop [obj (first world) rest-world (rest world) new-world []]
    (if obj
      (if-let [move (:move obj)]
        (let [new-obj (move obj)]
          (recur (first rest-world) (rest rest-world) (conj new-world new-obj)))
        (recur (first rest-world) (rest rest-world) (conj new-world obj)))
      new-world)))

(defn move-ball [obj] 
  (let [new-pos (vec (map + (:position obj) (:velocity obj)))] 
    (assoc obj :position new-pos)))

; run

(let [running (atom true)
      world-atom (atom [{:desc "ball"
                         :position [0 0]
                         :velocity [1 1]
                         :paint paint-ball
                         :move move-ball}])
      canv (canvas :paint #(paint-world %1 %2 world-atom))
      frm (frame
            :title "Pong"
            :width 500
            :height 500 
            :content canv
            :on-close :dispose)]
  (listen frm :window-closing (fn [_] (reset! running false)))
  (-> frm show!)
  (while @running
    (let [world @world-atom
          new-world (move-objs world)]
      ;(println new-world)
      (reset! world-atom new-world))
    (repaint! canv)
    (Thread/sleep 10)))
