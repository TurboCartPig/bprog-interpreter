# Bprog interpreter

## Approach taken to parsing

The approach I took to parsing is based on the concept of monadic parser combinators. This allows me to compose more and more complex parsers out of simpler parsers. All in all I'm pretty happy with this from the parsing side. Not so sure about it from the evaluation side though, that part is kind of a mess atm.

I considered, and still consider, writing the parsers in a way that produces proper parse trees. I think this could be done by introducing a stack while parsing and parse onto and off of that stack to produce expression trees. If time permits I might explore this further.

## Differences between this and the spec

Most of these differences are minor and silly, they should not really matter, but are nice to know about before attempting to run programs through this interpreter.

* Booleans are `true` and `false`
* swap is swp
* Strings are quoted with no space surrounding the string. E.g. `"hello"` instead of `" hello "`. In fact the second one would not parse correctly.
* unbound symbols do not evaluate to themselves. This seems like a silly artifact of how lone symbols are also part of strings according to the spec.
* Control flow operations are implemented as postfix operators. This means that I can evaluate them only in the context of the stack and symbol table, and without regard for the program instructions. The downside of this is that operators can not be used in branches without `{}`s, as these will be immediately evaluated.
* Not all tests are implemented, only those that succeed. This is to make effective use of CI.
 
## Issues with the solution
 
These are not purposeful differences to the spec, these are bugs.
 
* Symbols are not implemented.
* IO is not implemented

## Thoughts on the assignment and this solution

The approach I took to this assignment was that of minimum time investment for maximum return value. Overall I have spent about 35 hours on the project, and considering that, I am pretty happy about how it turned out. I will invest a bit more time into the final project to make up for the shortcomings here, and I hope that do.

I'm also reasonably happy about how the parsing turned out, considering it's all of my making, and no libraries used. The evaluation part I never quite came to grips with though, and it's pretty messy as is.

## Readings and other sources of inspiration

* Programming in Haskell by Graham Hutton
* Parsec and Megaparsec for inspiration for how to do proper parsing.
