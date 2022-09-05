#!/usr/bin/env bash

for i in $(seq 100); do
  (( $i % 3 != 0 )) && (( $i % 5 != 0 )) && echo -n $i
  (( $i % 3 == 0 )) && echo -n 'Fizz'
  (( $i % 5 == 0 )) && echo -n 'Buzz'
  echo
done
