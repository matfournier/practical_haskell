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

Pg 254 Exercise 7-5 the signature for sequence is `monad m => [m a] -> m [a]`

Pg 262 This is a [good introduction to monad transformers that is more clear
than the book](https://two-wrongs.com/a-gentle-introduction-to-monad-transformers)

Pg 263 Exercise 7.6 was very clear once I read a few exampls [here in the monad
transformers section ](http://dev.stephendiehl.com/fun/basics.html)

Pg 264 Exercise 7.7 *This exercise is terrible* and I have no idea what the author
wants. I spent 6+ hours on this.  Eventually came across [this SO post from his
firstbook](https://stackoverflow.com/questions/24195617/use-list-monad-inside-monad-transformer-type-classes)
although the solution crashes with an exception (!!!) around mzero in my
`ch7Transformers.hs` file.  I wrote a version using ReaderT / WriterT explicitly
and it works although I find the lifting of the list monad using `ask >>= msum .
map return` to be super confusing.  I have no idea what the author wants and if
anyone knows please open an issue and explain it [or track me down on twitter in
this discussion when I was asking the world about this
question](https://twitter.com/fried_brice/status/1185815297250320385
)

## Ch 8 

Skipping. I've worked through a bunch of [parallel and concurrent programming in
haskell](https://simonmar.github.io/pages/pcph.html) before so I'm moving onto
the next chapter. 

## Ch 9 

- Accessing files pg 332 need to bring in package `conduit-extra`
- winnersFile example on pg 333 make sure to concat `<> BS.pack \n` 

## Ch 10 

Note: skipping most of the exercises in this chapter as they are tedious AF.  Parsing is super
interesting and there are more fun ways to play with them: see
[here](https://blog.ploeh.dk/2019/10/28/a-basic-haskell-solution-to-the-robot-journeys-coding-exercise/)
and [particularly this
one](https://github.com/Cmdv/Haskell-Interview-Tests/tree/master/src/SpiralWorld.hs)

- pg 359 you don't need the `let c = ` part, you can just run `parseOnly aClient b` instead 

- Ex 10-1 skipped, Ex 10-2 skipped
