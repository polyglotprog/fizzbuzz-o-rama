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
- [ ] Crystal
- [ ] Elixir
- [ ] Erlang
- [ ] Go
- [ ] Groovy
- [ ] Java
- [ ] JavaScript
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

<!-- Links -->
[FizzBuzz]: https://en.wikipedia.org/wiki/Fizz_buzz
