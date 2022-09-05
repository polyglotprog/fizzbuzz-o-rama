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
- [ ] C
- [ ] C++
- [ ] Clojure
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

<!-- Links -->
[FizzBuzz]: https://en.wikipedia.org/wiki/Fizz_buzz
