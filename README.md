# Memoization and Dynamic Programming in Haskell

Typically, when one thinks of memoization and dynamic programming, they think
of hiding some sort of mutable data structure underneath a function call where
the function call checks if that particular cell has been populated yet, doing
some work to populate it if not.

In Haskell, we can lazily initialize the entire table, and then index into it
immediately without doing all of the work, which allows us to write dynamic
programs in a much more declarative way. The machinery to do this is contained
in Memo, some examples are contained in Examples, and a cute little test suite
is contained in Test.

This work was inspired by [Fun with Type Functions](https://www.cs.tufts.edu/comp/150FP/archive/simon-peyton-jones/typefun.pdf).
