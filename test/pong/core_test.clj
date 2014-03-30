(ns pong.core-test
  (:require [clojure.test :refer :all]
            [pong.core :refer :all]))

(deftest tests
  (testing "moving obj returns new obj with correct position"
    (is (= {:position [1 0]
            :velocity [1 0]} 
           (move-obj {:position [0 0]
                       :velocity [1 0]}
                      1))))
  (testing "moving objects returns new world with objects at new positions"
    (is (= [{:position [1 0]
             :velocity [1 0]
             :move move-obj
             :paint paint-rect}]
           (move-objs [{:position [0 0]
                        :velocity [1 0]
                        :move move-obj
                        :paint paint-rect}]
                      1))))
  (testing "can calculate bottom right corner correctly"
    (is (= [9 10] (bottom-right {:position [1 3] :size [8 7]}))))
  (testing "can calc bounding box"
    (is (= [1 3 9 10] (bounding-box {:position [1.1 3.4] :size [8 7]}))))
  (testing "solids are indexed correctly"
    (do (is (= {:idx 3 :obj {:solid? true}} (index-solid 3 {:solid? true})))
        (is (= nil (index-solid 3 {:solid? false})))))
  (testing "horizontal collisions are recognized correctly"
    (do (is (= :horz-coll (horz-coll? [0 0 4 4] [1 4 4 8])))
        (is (= nil (horz-coll? [0 0 4 4] [1 5 4 8])))
        (is (= nil (horz-coll? [0 0 4 4] [6 4 4 8])))))
  (testing "vertical collisions are recognized correctly"
    (do (is (= :vert-coll (vert-coll? [0 0 4 4] [4 4 4 8])))
        (is (= nil (vert-coll? [0 0 4 4] [5 4 4 8])))
        (is (= nil (vert-coll? [0 0 4 4] [4 5 4 8])))))
  (testing "collision between two objects is detected"
    (is (= [{:idx-objs [{:idx 0 
                         :obj {:position [0 0] :size [5 5]}}
                        {:idx 1 
                         :obj {:position [5 0] :size [5 5]}}]
            :type :vert-coll}]
           (detect-obj-colls {:idx 0 
                              :obj {:position [0 0] :size [5 5]}}
                             [{:idx 1
                               :obj {:position [5 0] :size [5 5]}}
                              {:idx 2
                               :obj {:position [6 0] :size [5 5]}}]))))
  (testing "collisions in world are detected"
    (is (= [{:idx-objs [{:idx 0 
                         :obj {:position [0 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}
                        {:idx 1 
                         :obj {:position [5 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}]
            :type :vert-coll}]
           (detect-collisions [{:position [0 0] 
                                      :size [5 5]
                                      :velocity [0 0]
                                      :solid? true}
                               {:position [5 0] 
                                      :size [5 5]
                                      :velocity [0 0]
                                      :solid? true}
                               {:position [6 0] 
                                      :size [5 5]
                                      :velocity [0 0]
                                      :solid? true}]
                              1))))
  (testing "obj's collide is called when objects collide, if it exists"
    (is (= ["called"
            {:position [4 0] 
             :size [5 5]
             :velocity [0 0]
             :solid? true}]
           (collide-obj {:idx 0 
                         :obj {:position [0 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}
                         {:idx 1 
                         :obj {:position [4 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}
                        :vert-coll
                        [{:position [0 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true
                               :collide (fn [o oth ct] "called")}
                         {:position [4 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}]))))
  (testing "apply-collision calls collide-obj twice"
    (is (= 2 (with-redefs [collide-obj (fn [_ _ _ w] (inc w))] 
               (apply-collision 0 {})))))
  (testing "bounding box bottom wall"
    (is (= [0 500 500 500] 
           (bounding-box {:id :bottom-wall
                          :position [0 500]
                          :size [500 0]
                          :solid? true}))))
  (testing "bounding box paddle"
    (is (= [450 430 460 500] 
           (bounding-box {:id :right-paddle
                          :position [450 430]
                          :size [10 70]
                          :velocity [0 0]
                          :solid? true}))))
  (testing "horzizontal collide"
    (is (= :horz-coll (horz-coll? [0 500 500 500] [450 430 460 500]))))
  (testing "paddle can collide horizontally"
    (is (= :horz-coll (horz-coll? 
                        (bounding-box {:id :bottom-wall
                                       :position [0 500]
                                       :size [500 0]
                                       :solid? true})
                        (bounding-box {:id :right-paddle
                                       :position [450 430]
                                       :size [10 70]
                                       :velocity [0 0]
                                       :solid? true})))))
  (testing "paddle can collide with wall"
    (is (= :horz-coll 
           (:type 
             (first
               (detect-obj-colls {:idx 3 
                                  :obj {:id :bottom-wall
                                        :position [0 500]
                                        :size [500 0]
                                        :solid? true}}
                                 [{:idx 5
                                   :obj {:id :right-paddle
                                         :position [450 430]
                                         :size [10 70]
                                         :velocity [0 0]
                                         :solid? true}}]))))))
  (testing "if paddle collides with top or bottom wall vert velocity is set to 0"
    (is (= 0 (get-in (collide-paddle {:velocity [1 2]}
                                     {:id :top-wall}
                                     :horz-coll)
                     [:velocity 1])))))

