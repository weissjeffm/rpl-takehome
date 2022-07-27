(ns redplanetlabs.core-test
  (:require [clojure.test :refer :all]
            [redplanetlabs.core :as sut]))

(sut/defstackfn example [!a !b !c] ; example uses input: 1 2 4. Stack starts empty.
  !a ; 1
  !b ; 1 2
  (invoke> + 2) ; 3
  !v1+ ; 3
  !c ; 3 4
  !c ; 3 4 4
  <pop> ; 3 4
  2 ; 3 4 2
  (invoke> * 2) ; 3 8
  !v2+ ; 3 8
  (invoke> = 2) ; false
  (if> ; stack empty
      !v1
    !v2
    (invoke> - 2)
    else>
    "false!!" ;; "false!!"
    (invoke> println 1) ; nil
    <pop> ; stack empty
    !v1 ; 3
    !v2 ; 3 8
    (invoke> * 2) ; 24
    )
  )

(sut/defstackfn nested-if [!y]
  !y !y
  (invoke> pos? 1)
  (if> !y
    (invoke> even? 1)
    (if> 3
      (invoke> + 2)
      else>
      11
      (invoke> * 2))
    else> -99))

(sut/defstackfn shadow [!x !y] !x !y !x+ !x)

(deftest example-test
  (is (= (example 1 2 4) '(24))))

(deftest nested-if-test
  (are [y exp] (= (nested-if y) exp)
    4 '(7)
    5 '(55)
    -4  '(-99 -4)))

(deftest shadow-test
  (is (= (shadow 1 2) '(2 2 1))))
