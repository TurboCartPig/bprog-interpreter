# BProg interpreter

## Approach taken to parsing

The approach I took to parsing is based on the concept of monadic parser combinators. This allows me to compose more and more complex parsers out of simpler parsers. All in all I'm pretty happy with this from the parsing side. Not so sure about it from the evaluation side though, that part is kind of a mess atm.

I considered, and still consider, writing the parsers in a way that produces proper parse trees. I think this could be done by introducing a stack while parsing and parse onto and off of that stack to produce expression trees. If time permits I might explore this further.

## Differences between this and the spec

Most of these differences are minor and silly, they should not really matter, but are nice to know about before attempting to run programs through this interpreter.

 * Booleans are `true` and `false`
 * swap is swp
 * Strings are quoted with no space surrounding the string. E.g. `"hello"` instead of `" hello "`. In fact the second one would not parse correctly.
 * unbound symbols do not evaluate to themselves. This seems like a silly artifact of how lone symbols are also part of strings according to the spec.
 
## Issues with the solution
 
These are not purposeful differences to the spec, these are bugs
 
 * Symbols are not implemented.
 

## Readings and other sources of inspiration

 * Programming in Haskell by Graham Hutton (This is where I got monadic parsers from).
 * Parsec and Megaparsec for inspiration for how to do proper parsing.