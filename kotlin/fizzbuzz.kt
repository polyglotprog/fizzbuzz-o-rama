fun main() {
  for (i in 1..100) {
    when {
      (i.mod(3) == 0 && i.mod(5) == 0) -> println("FizzBuzz")
      (i.mod(3) == 0) -> println("Fizz")
      (i.mod(5) == 0) -> println("Buzz")
      else -> println(i)
    }
  }
}
