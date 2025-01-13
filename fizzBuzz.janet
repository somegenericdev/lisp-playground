(defn isDivisibleByThree [input] (= 0 (% input 3)))
(defn isDivisibleByFive [input] (= 0 (% input 5)))

(defn range [start end]  (def arr (array/new 0)) (loop [i :range [start (+ end 1)]] (array/push arr i) ) arr  )

(defn fizzBuzzOnce [input] (def isDivByFive (isDivisibleByFive input)) (def isDivByThree (isDivisibleByThree input)) (if (and isDivByFive isDivByThree) (print "FizzBuzz") )  (if (and isDivByFive (not isDivByThree)) (print  "Buzz")) (if (and isDivByThree (not isDivByFive)) (print  "Fizz")))

(defn fizzBuzz [] (each number (range 1 100) (fizzBuzzOnce number)) )

(fizzBuzz)