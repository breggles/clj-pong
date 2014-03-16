(ns pong.core-test
  (:require [clojure.test :refer :all]
            [pong.core :refer :all]))

(deftest tests
  (testing "moving ball returns new ball with correct position"
    (is (= {:position [1 0]
            :velocity [1 0]} 
           (move-ball {:position [0 0]
                       :velocity [1 0]}
                      []))))
  (testing "moving objects returns new world with objects at new positions"
    (is (= [{:position [1 0]
             :velocity [1 0]
             :move move-ball
             :paint paint-ball}]
           (move-objs [{:position [0 0]
                        :velocity [1 0]
                        :move move-ball
                        :paint paint-ball}])))))
