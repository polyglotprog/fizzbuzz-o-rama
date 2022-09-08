![FizzBuzz-O-Rama](/fizzbuzz-o-rama.png)

# FizzBuzz-O-Rama

Solutions to [FizzBuzz] in 21 different [languages](#languages). Don't
[jump to Conclusions](#conclusions).

## Contents

- [The Problem](#the-problem)
- [Languages](#languages)
- [Solutions](#solutions)
- [Conclusions](#conclusions)

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
- [X] [Kotlin](#kotlin)
- [X] [Nim](#nim)
- [X] [Perl](#perl)
- [X] [PHP](#php)
- [X] [Python](#python)
- [X] [Ruby](#ruby)
- [X] [Rust](#rust)
- [X] [Scala](#scala)
- [X] [Smalltalk](#smalltalk)
- [X] [SQL](#sql)

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

### Kotlin

To run:

```shell
kotlinc fizzbuzz.kt
kotlin FizzbuzzKt
# You can also run it with Java!
java FizzbuzzKt
```

Solution:

```kotlin
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
```

### Nim

To run:

```shell
nim compile fizzbuzz.nim
./fizzbuzz
```

Solution:

```nim
for i in 1..100:
  if i mod 3 == 0 and i mod 5 == 0:
    echo "FizzBuzz"
  elif i mod 3 == 0:
    echo "Fizz"
  elif i mod 5 == 0:
    echo "Buzz"
  else:
    echo i
```

### Perl

To run:

```shell
# Run with Perl
perl fizzbuzz.pl
# Or run as script
chmod +x fizzbuzz.pl
./fizzbuzz.pl
```

Solution:

```perl
#!/usr/bin/env perl

for my $i (1..100) {
  print $i unless ($i % 3 == 0 or $i % 5 == 0);
  print "Fizz" if ($i % 3 == 0);
  print "Buzz" if ($i % 5 == 0);
  print "\n"
}
```

### PHP

To run:

```shell
# Run with PHP
php fizzbuzz.php
# Or run as script
chmod +x fizzbuzz.php
./fizzbuzz.php
```

Solution:

```php
#!/usr/bin/env php

<?php

foreach (range(1, 100) as $i) {
  if ($i % 3 == 0 && $i % 5 == 0) {
    echo "FizzBuzz\n";
  } elseif ($i % 3 == 0) {
    echo "Fizz\n";
  } elseif ($i % 5 == 0) {
    echo "Buzz\n";
  } else {
    echo "$i\n";
  }
}

?>
```

### Python

To run:

```shell
# Run with Python
python fizzbuzz.py
# Or run as script
chmod +x fizzbuzz.py
./fizzbuzz.py
```

Solution:

```python
#!/usr/bin/env python

for i in range(1, 101):
  if i % 3 == 0 and i % 5 == 0:
    print("FizzBuzz")
  elif i % 3 == 0:
    print("Fizz")
  elif i % 5 == 0:
    print("Buzz")
  else:
    print(i)
```

### Ruby

To run:

```shell
# Run with Ruby
ruby fizzbuzz.rb
# Or run as script
chmod +x fizzbuzz.rb
./fizzbuzz.rb
```

Solution:

```ruby
#!/usr/bin/env ruby

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

### Rust

To run:

```shell
rustc fizzbuzz.rs
./fizzbuzz
```

Solution:

```rust
fn main() {
  for i in 1..101 {
    if i % 3 == 0 && i % 5 == 0 {
      println!("FizzBuzz");
    } else if i % 3 == 0 {
      println!("Fizz");
    } else if i % 5 == 0 {
      println!("Buzz");
    } else {
      println!("{}", i);
    }
  }
}
```

### Scala

To run:

```shell
scalac FizzBuzz.scala
scala FizzBuzz
```

Solution:

```scala
object FizzBuzz {
  def main(args: Array[String]) {
    for (i <- 1 to 100) {
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
  }
}
```

### Smalltalk

To run:

1. Fire up your favorite [Smalltalk] environment (such as [Pharo]).
2. Make sure the Transcript is open.
3. Paste the below code in the Playground/Workspace.
4. Run it!

Solution:

```smalltalk
1 to: 100 do: [ :i |
  | fizz buzz |
  fizz := i % 3 = 0.
  buzz := i % 5 = 0.
  (fizz not and: buzz not) ifTrue: [ Transcript show: i; cr ].
  (fizz and: buzz not)     ifTrue: [ Transcript show: 'Fizz'; cr ].
  (buzz and: fizz not)     ifTrue: [ Transcript show: 'Buzz'; cr ].
  (fizz and: buzz)         ifTrue: [ Transcript show: 'FizzBuzz'; cr ].
].
```

### SQL

To run: Create a new database or connect to an existing one. Run the below SQL.

Solution:

```sql
CREATE TABLE fizzbuzz(value VARCHAR(8));

INSERT INTO fizzbuzz VALUES
(1), (2), ('Fizz'), (4), ('Buzz'), ('Fizz'), (7), (8), ('Fizz'), ('Buzz'),
(11), ('Fizz'), (13), (14), ('FizzBuzz'), (16), (17), ('Fizz'), (19), ('Buzz'),
('Fizz'), (22), (23), ('Fizz'), ('Buzz'), (26), ('Fizz'), (28), (29),
('FizzBuzz'), (31), (32), ('Fizz'), (34), ('Buzz'), ('Fizz'), (37), (38),
('Fizz'), ('Buzz'), (41), ('Fizz'), (43), (44), ('FizzBuzz'), (46), (47),
('Fizz'), (49), ('Buzz'), ('Fizz'), (52), (53), ('Fizz'), ('Buzz'), (56),
('Fizz'), (58), (59), ('FizzBuzz'), (61), (62), ('Fizz'), (64), ('Buzz'),
('Fizz'), (67), (68), ('Fizz'), ('Buzz'), (71), ('Fizz'), (73), (74),
('FizzBuzz'), (76), (77), ('Fizz'), (79), ('Buzz'), ('Fizz'), (82), (83),
('Fizz'), ('Buzz'), (86), ('Fizz'), (88), (89), ('FizzBuzz'), (91), (92),
('Fizz'), (94), ('Buzz'), ('Fizz'), (97), (98), ('Fizz'), ('Buzz');

SELECT * FROM fizzbuzz;
```

## Conclusions

- The *Most Concise Award* &#x1F3C6; goes to&hellip; **Nim**.
- The *Most Verbose Award* &#x1F3C6; goes to&hellip; **Java**.
- The *Most Lines of Code Award* &#x1F3C6; goes to&hellip; **C++** with 16 LOC
  &ndash; it really is more than C!
- The *Least Lines of Code Award* &#x1F3C6; is a tie between&hellip; **Bash**
  and **Perl** with 6 LOC each. You should be grateful neither of them was
  [written in one line].
- The *Most Object-Oriented Award* &#x1F3C6; goes to&hellip; **Smalltalk**
  &ndash; because in Smalltalk, [*everything is an object*].
- The *Most Functional Award* &#x1F3C6; is a three-way tie between&hellip;
  **Clojure**, **Elixir**, and **Erlang**. 'Nuff said.
- The *Most Unique Syntax Award* &#x1F3C6; is a tie between&hellip;
  ~~**Prolog**~~ &ndash; er &ndash; **Erlang** and **Smalltalk**. Erlang gets
  its "weird" (according to some) syntax from Prolog.[^1]
- The *Best Java Award* &#x1F3C6; goes to&hellip; **Kotlin**. Kotlin puts the
  `fun` into JVM development.
- The *Most Pythonic Award* &#x1F3C6; goes to&hellip; **Nim**. Sorry Python!
- The *Most Rubyesque Award* &#x1F3C6; is a tie between&hellip; **Ruby** and
  **Crystal**. In this case, the code is exactly the same.
- The *Most Erlangish Award* &#x1F3C6;  goes to&hellip; **Erlang**. This
  functional language is [in a *class* of its own].
- The *Most Lispy Award* &#x1F3C6; goes to&hellip; **Clojure**, the only [Lisp]
  &#x1F3C6;&#x1F3C6;&#x1F3C6; in the running.[^2]
- The **JavaScript** solution could have been written in Angular, React, Vue,
  Svelte, jQuery, or [\[INSERT YOUR FAVORITE JAVASCRIPT FRAMEWORK HERE\]].
  After carefully considering all of the options, I finally settled on [Vanilla
  JS] &#x1F3C6;. However, instead of a simple, boring `for`-loop like so many
  of the other solutions, a *modern*, *functional style* using [spread syntax]
  seemed like the best approach ¯\\\_(ツ)\_/¯.
- There are some problems **SQL** is just no good at. That's why there is
  [NoSQL] &#x1F3C6;.
- Looking for **C#**? It wasn't included because we have **C** and **C++**, and
  [they are already sharp enough] &#x2622;.

And...

- My *Personal Favorites* are [Clojure]&#x1F3C5;, [Elixir]&#x1F3C5;,
  [Nim]&#x1F3C5;, and [Smalltalk]&#x1F3C5; ([Pharo]&#x1F3C5;). ***These are
  some of the most interesting &ndash; and powerful &ndash; languages I have
  found.*** Check them out. They are well worth your time.


<!------------------------------------------------------------------------------
  Footnotes
------------------------------------------------------------------------------->
[^1]: https://www.erlang.org/faq/academic.html
[^2]: https://clojure.org/about/rationale#_lisp_is_a_good_thing


<!------------------------------------------------------------------------------
  Links
------------------------------------------------------------------------------->
[FizzBuzz]: https://en.wikipedia.org/wiki/Fizz_buzz

[Clojure]: https://clojure.org/
[Elixir]: https://elixir-lang.org/
[Lisp]: https://en.wikipedia.org/wiki/Lisp_(programming_language)
[Nim]: https://nim-lang.org/
[NoSQL]: https://en.wikipedia.org/wiki/NoSQL
[Pharo]: https://pharo.org/
[Smalltalk]: https://en.wikipedia.org/wiki/Smalltalk
[Vanilla JS]: http://vanilla-js.com/

[written in one line]: https://jacoby.github.io//2019/03/25/fizzbuzz-oneliner-in-perl.html
[*everything is an object*]: https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Book%3A_Squeak_by_Example_(Black_Ducasse_Nierstrasz_and_Pollet)/05%3A_The_Smalltalk_Object_Model/5.02%3A_Everything_is_an_Object
[in a *class* of its own]: https://www.quora.com/What-does-Alan-Kay-think-about-Joe-Armstrong-claiming-that-Erlang-might-be-the-only-object-oriented-language-and-also-his-thesis-supervisor-s-claim-that-Erlang-is-extremely-object-oriented?share=1
[\[INSERT YOUR FAVORITE JAVASCRIPT FRAMEWORK HERE\]]: https://www.quora.com/What-is-JavaScript-fatigue
[spread syntax]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax
[they are already sharp enough]: https://www.goodreads.com/quotes/226222-c-makes-it-easy-to-shoot-yourself-in-the-foot
