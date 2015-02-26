# brainfuck-hs

Originally written as a solution to the CodeWars kata at http://www.codewars.com/kata/526156943dfe7ce06200063e/

The tests are adopted from the provided test-cases there.

```sh
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal test
echo -en '\6' | cabal run -- fib.bf
```
