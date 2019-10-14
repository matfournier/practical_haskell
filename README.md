# Practical Haskell

- get the book [here](https://www.apress.com/gp/book/9781484244791)
- repo (doesn't include solutions) is [here](https://github.com/Apress/practical-haskell)

This has my solutions to some of the exercises in the book

## Errata / Notes


### Ch6 

Pg 215 gives a `kMeans` implementation using state:

- line `t <- fmap threshold get` can be replaced with `t <- gets threshold` 
- likely importing `import qualified Control.Monad.State as S` so everything is `S.modify, S.get, S.State` etc. 

Pg 221 in the type `Settings` you don't need the `user :: Person` as it looks like cruft 

Pg 223 Exercise 6-6 you need to implement `Functor` and `Applicative` in order to implement `Monad`.  That's what the type error is going to
tell you in this question.

## Ch7 

Pg 240 Exercise 7-3 should include InfoClientGender Male 

Pg 241 Apriori Algorithm - I found [this page](https://www.hackerearth.com/blog/developers/beginners-tutorial-apriori-algorithm-data-mining-r-implementation/) for what the algorithm was doing 

Pg 250 The referenced paper: [Adventures in three
monads](http://web.mit.edu/~ezyang/Public/threemonads.pdf) by Edward Z. Yang.
Note to self: has a good overview of MonadFail typeclass 
