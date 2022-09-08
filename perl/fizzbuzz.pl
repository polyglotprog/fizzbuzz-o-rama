#!/usr/bin/env perl

for my $i (1..100) {
  print $i unless ($i % 3 == 0 or $i % 5 == 0);
  print "Fizz" if ($i % 3 == 0);
  print "Buzz" if ($i % 5 == 0);
  print "\n"
}
