defmodule FizzBuzz do
  def fizzbuzz(n) when rem(n, 3) == 0 and rem(n, 5) == 0, do: "FizzBuzz"
  def fizzbuzz(n) when rem(n, 3) == 0, do: "Fizz"
  def fizzbuzz(n) when rem(n, 5) == 0, do: "Buzz"
  def fizzbuzz(n), do: n
end

1..100
|> Enum.map(&FizzBuzz.fizzbuzz/1)
|> Enum.each(&IO.puts/1)
