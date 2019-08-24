# Jaipur

Game modelling in Racket for the fast-paced two-player card game [Jaipur](https://boardgamegeek.com/boardgame/54043/jaipur).

This is primarily an exercise in (a) coding up Euro-style games, and (b) improving my Racket skills and idioms.
This extensively uses optics (i.e. lenses) to access and manipulate the game state data structure.
The approach is very similar to my [Purescript version](https://github.com/alphajuliet/jaipur) but the lens code is much simpler. Racket is like that.

The code is split up into the following modules by source file, starting from the bottom of the dependency stack.

## state.rkt

We encode game state as a deep hash: see the definition of `initial-state`. This gives maximum readability, and we define composable lenses to access all the pieces.
Borrowing from Purescript, we can access a player's hand as `(view (>>> _hand (_player 'A)) state)`, where `>>>` is left-to-right lens composition (i.e. the thrush operator). 
Similarly, we can use the verbs `at` and `over` to set and update values.

Hands of cards are stored as hashes of numeric values, keyed by card type. This allows rudimentary arithmetic operations on hands that simplifies some of the manipulations. 
These operatios come from my [hash-ext](https://github.com/alphajuliet/hash-ext) library. You'll need to install this package with `raco`.

Tokens are stored in the state as a hash of lists. This file also defines a readable version of the state using `ppst`.

## actions.rkt

The actions in Jaipur are:

* `init-game`: initialise the gamne with dealt cards to the market and players' hands. Optionally, force the random number seed to make it deterministic.
* `take-card`: take one card from the market, or all the camels.
* `sell-cards`: sell all of a given resource, subject to minimum sell rules.
* `exchange-cards`: exchange cards from the market with a player's hand.

These are implemented as actions with relevant parameters applied to a state, and thereby generating a new state, i.e. we have immutable state.

We also encode the game rules for each of the actions, scoring, and define an `end-of-game?` test. All the rest is supporting code.

## game.rkt

At this level, we define a list of the available actions for a given state (not yet complete). 

We also have defined a function that plays a random game of Jaipur for two players. 
No strategy, just random actions from those permitted. The game will terminate eventually when either the deck is empty or three token piles are empty. 
To run it with a given random seed, call it like this:

```
(random-game (init-game #:seed 42))
```
If you want to see all the states and actions go by, turn on the printing...
```
(random-game (init-game #:seed 42) #:print? #t)
```

All the game states and actions are collected for now into `*game*`, so you can
go back and see what happened. 

## learn.rkt

This is where we'll learn how to play the game using reinforcement learning, specifically the Q-learning 
algorithm. It's a work in progress.

