(defn fizzbuzz [n]
  (cond
    (and (zero? (mod n 3)) (zero? (mod n 5))) "FizzBuzz"
    (zero? (mod n 3)) "Fizz"
    (zero? (mod n 5)) "Buzz"
    :else n))

(->>
  (range 1 101)
  (map fizzbuzz)
  (run! println))
