1 to: 100 do: [ :i |
  | fizz buzz |
  fizz := i % 3 = 0.
  buzz := i % 5 = 0.
  (fizz not and: buzz not) ifTrue: [ Transcript show: i; cr ].
  (fizz and: buzz not)     ifTrue: [ Transcript show: 'Fizz'; cr ].
  (buzz and: fizz not)     ifTrue: [ Transcript show: 'Buzz'; cr ].
  (fizz and: buzz)         ifTrue: [ Transcript show: 'FizzBuzz'; cr ].
].
