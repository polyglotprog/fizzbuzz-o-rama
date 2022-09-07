![FizzBuzz-O-Rama](/fizzbuzz-o-rama.png)

# FizzBuzz-O-Rama

Solutions to [FizzBuzz] in 21 different [languages](#languages).

## Contents

- [The Problem](#the-problem)
- [Languages](#languages)
- [Solutions](#solutions)

## The Problem

For numbers 1 to 100:

- If the number is *divisible by 3*, output `Fizz`
- If the number is *divisible by 5*, output `Buzz`
- If the number is *divisble by 3 and 5*, output `FizzBuzz`
- Otherwise, output the number

## Languages

- [X] [Bash](#bash)
- [X] [C](#c)
- [X] [C++](#c-1)
- [X] [Clojure](#clojure)
- [X] [Crystal](#crystal)
- [X] [Elixir](#elixir)
- [X] [Erlang](#erlang)
- [X] [Go](#go)
- [X] [Groovy](#groovy)
- [X] [Java](#java)
- [X] [JavaScript](#javascript)
- [ ] Kotlin
- [ ] Nim
- [ ] Perl
- [ ] PHP
- [ ] Python
- [ ] Ruby
- [ ] Rust
- [ ] Scala
- [ ] Smalltalk
- [ ] SQL

## Solutions

### Bash

To run:

```shell
# Make executable
chmod +x fizzbuzz.sh
# Run
./fizzbuzz.sh
```

Solution:

```bash
#!/usr/bin/env bash

for i in $(seq 100); do
  (( $i % 3 != 0 )) && (( $i % 5 != 0 )) && echo -n $i
  (( $i % 3 == 0 )) && echo -n 'Fizz'
  (( $i % 5 == 0 )) && echo -n 'Buzz'
  echo
done
```

### C

To run:

```shell
# Compile with GCC
gcc fizzbuzz.c -o fizzbuzz
# Run
./fizzbuzz
```

Solution:

```c
#include <stdio.h>

int main() {
  for (int i = 1; i <= 100; i++) {
    if (i % 3 == 0 && i % 5 == 0) {
      printf("FizzBuzz\n");
    } else if (i % 3 == 0) {
      printf("Fizz\n");
    } else if (i % 5 == 0) {
      printf("Buzz\n");
    } else {
      printf("%d\n", i);
    }
  }
  return 0;
}
```

### C++

To run:

```shell
# Compile with G++
g++ fizzbuzz.cpp -o fizzbuzz
# Run
./fizzbuzz
```

Solution:

```c++
#include <iostream>
using namespace std;

int main() {
  for (int i = 1; i <= 100; i++) {
    if (i % 3 == 0 && i % 5 == 0) {
      cout << "FizzBuzz" << endl;
    } else if (i % 3 == 0) {
      cout << "Fizz" << endl;
    } else if (i % 5 == 0) {
      cout << "Buzz" << endl;
    } else {
      cout << i << endl;
    }
  }
  return 0;
}
```

### Clojure

To run:

```shell
clojure fizzbuzz.clj
```

Solution:

```clojure
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
```

### Crystal

To run:

```shell
crystal fizzbuzz.cr
```

Solution:

```crystal
(1..100).each do |i|
  if (i % 3).zero? && (i % 5).zero?
    puts "FizzBuzz"
  elsif (i % 3).zero?
    puts "Fizz"
  elsif (i % 5).zero?
    puts "Buzz"
  else
    puts i
  end
end
```

### Elixir

To run:

```shell
elixir fizzbuzz.ex
```

Solution:

```elixir
defmodule FizzBuzz do
  def fizzbuzz(n) when rem(n, 3) == 0 and rem(n, 5) == 0, do: "FizzBuzz"
  def fizzbuzz(n) when rem(n, 3) == 0, do: "Fizz"
  def fizzbuzz(n) when rem(n, 5) == 0, do: "Buzz"
  def fizzbuzz(n), do: n
end

1..100
|> Enum.map(&FizzBuzz.fizzbuzz/1)
|> Enum.each(&IO.puts/1)
```

### Erlang

To run, start the Erlang shell with `erl` and then run the following commands:

```
1> c(fizzbuzz).
{ok,fizzbuzz}
2> fizzbuzz:run().
```

Solution:

```erlang
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
```

### Go

To run:

```shell
go run fizzbuzz.go
```

Solution:

```go
package main

import "fmt"

func main() {
  for i := 1; i <= 100; i++ {
    if i % 3 == 0 && i % 5 == 0 {
      fmt.Println("FizzBuzz")
    } else if i % 3 == 0 {
      fmt.Println("Fizz")
    } else if i % 5 == 0 {
      fmt.Println("Buzz")
    } else {
      fmt.Println(i)
    }
  }
}
```

### Groovy

To run:

```shell
groovy fizzbuzz.groovy
```

Solution:

```groovy
for (i in 1..100) {
  if (i % 3 == 0 && i % 5 == 0) {
    println("FizzBuzz")
  } else if (i % 3 == 0) {
    println("Fizz")
  } else if (i % 5 == 0) {
    println("Buzz")
  } else {
    println(i)
  }
}
```

### Java

To run:

```shell
# Java >= 11
java FizzBuzz.java

# Java < 11
javac FizzBuzz.java
java FizzBuzz
```

Solution:

```java
public class FizzBuzz {
  public static void main(String[] args) {
    for (int i = 1; i <= 100; i++) {
      if (i % 3 == 0 &&  i % 5 == 0) {
        System.out.println("FizzBuzz");
      } else if (i % 3 == 0) {
        System.out.println("Fizz");
      } else if (i % 5 == 0) {
        System.out.println("Buzz");
      } else {
        System.out.println(i);
      }
    }
  }
}
```

### JavaScript

To run:

```shell
node fizzbuzz.js
```

Alternatively, you can run it in your browser:

1. Open the developer tools.
2. Go to the console.
3. Paste the below code.
4. Hit enter.

Solution:

```javascript
function fizzbuzz(n) {
  if (n % 3 === 0 && n % 5 === 0) {
    return 'FizzBuzz'
  } else if (n % 3 === 0) {
    return 'Fizz'
  } else if (n % 5 === 0) {
    return 'Buzz'
  } else {
    return n
  }
}

[...Array(100).keys()]
  .map(i => i + 1)
  .map(fizzbuzz)
  .forEach(i => console.log(i))
```

<!-- Links -->
[FizzBuzz]: https://en.wikipedia.org/wiki/Fizz_buzz
