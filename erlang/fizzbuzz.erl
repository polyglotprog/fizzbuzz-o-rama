-module(fizzbuzz).
-export([run/0]).

fizzbuzz(N) when (N rem 3 == 0) and (N rem 5 == 0) -> "FizzBuzz";
fizzbuzz(N) when (N rem 3 == 0) -> "Fizz";
fizzbuzz(N) when (N rem 5 == 0) -> "Buzz";
fizzbuzz(N) -> integer_to_list(N).

run() ->
  Sequence = lists:seq(1, 100),
  FizzBuzz = lists:map(fun fizzbuzz/1, Sequence),
  lists:foreach(fun(S) -> io:format("~s~n", [S]) end, FizzBuzz).
