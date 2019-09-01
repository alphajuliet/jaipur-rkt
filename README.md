# Jaipur

Game modelling in Racket for the fast-paced two-player card game
[Jaipur](https://boardgamegeek.com/boardgame/54043/jaipur).

This is primarily an exercise in (a) coding up Euro-style games, and (b)
improving my Racket skills and idioms. This extensively uses optics
(i.e. lenses) to access and manipulate the game state data structure.
The approach is very similar to my [Purescript
version](https://github.com/alphajuliet/jaipur) but the lens code is
much simpler. Racket is like that.

The code is split up into the following modules by source file, starting
from the bottom of the dependency stack.

## state.rkt
 
We encode game state as a deep hash: see the definition of
`initial-state`. This gives maximum readability, and we define
composable lenses to access all the pieces. Borrowing from Purescript,
and using the convention of a leading underscore for a lens, we can
access a player's hand as `(view (>>> _hand (_player 'A)) state)`, where
`>>>` is left-to-right lens composition (i.e. the thrush operator).
Similarly, we can use the verbs `at` and `over` to set and update
values.

Hands of cards are stored as hashes of numeric values, keyed by card
type. This allows rudimentary arithmetic operations on hands that
simplifies some of the manipulations. These operatios come from my
[hash-ext](https://github.com/alphajuliet/hash-ext) library. You'll need
to install this package with `raco`.

Tokens are stored in the state as a hash of lists. This file also
defines a readable version of the state using `ppst`.

## actions.rkt
 
The actions in Jaipur are:

* `init-game`: initialise the gamne with dealt cards to the market and
players' hands. Optionally, force the random number seed to make it
deterministic. 
* `take-card`: take one card from the market, or all the
camels. 
* `sell-cards`: sell all of a given resource, subject to minimum
sell rules. 
* `exchange-cards`: exchange cards from the market with a
player's hand.

These are implemented as actions with relevant parameters applied to a
state, and thereby generating a new state, i.e. we have immutable state.

We also encode the game rules for each of the actions, scoring, and
define an `end-of-game?` test. All the rest is supporting code.

## game.rkt
 
At this level, the core function is `available-actions`. It lists all
the legal actions that are possible for a given player and current
state, i.e. `Player -> State -> [Action]`.

A *policy* is an algorithm for selecting an action  given the current
state for a given player. Its type signature is `Player -> State ->
Action`. The action is then applied to a game state by the function
`apply-action`, which has signature `Action -> State -> State`.

The default policy is `policy-random`, so you can play a game with a
random starting state and random (but legal) actions from both players
with the following call:

``` 
(play-game policy-random (init-game #:seed 42)) 
```

A slightly better policy is to pick the action that gives the highest
immediate reward; this is captured in the `argmax-points` function.

All the game states and actions are collected for now into `*game*`, so
you can go back and see what happened.


## learn.rkt
 
This contains experiments in a more sophisticated policy using classic
reinforcement learning, specifically the Q-learning algorithm and the
Bellman equation. It's a work in progress but the core is there to learn
from playing a series of games with random seeds, where player A uses
the Q-learning approach and B uses a random policy for selecting
actions.

There are two phases. In the learning phase we use the `q-learn`
function to play a game and update a Q-table with the new values using
the Bellman equation. The Q-table here is implemented as a hash of
hashes of integers because of its sparse nature. The sparseness is a
product of the state and action encodings that form the rows and columns
respectively of the table.

The encodings here (`encode-state` and `encode-action`) are awkward
because both the state spaces and actions spaces are large. The primary
approach for state is to encode the presence of a particular resource
(there are eight, including the camels) in the market, the player's own
hand, and the tokens remaining. That is all that is visible to a player
at the start of their turn. The actions are then encoded in terms of the
three main action options: `take-card`, `sell-cards`, and `exchange-cards`,
and the arguments each takes.

*Aside*: In a deep Q-learning approach you would get the system to learn
an efficient intermediate embedding of states and actions that would
optimise the learning. That's something to explore in the future, and
necessarily in another language. Racket unfortunately doesn't currently
give easy access to deep learning libraries, and I'm not very equipped
or inclined to knock up my own. I might port all this over to Clojure,
and then I at least have access to Java-based libraries like [DL4J](http://deeplearning4j.org/).

In the execution phase of Q-learning, the Q-table is referenced by a
policy called `Q-argmax` that selects the best action. Jaipur has enough
luck from card draws to obscure the results of the policies. You need to run enough games to generate statistically-significant evidence of improvement over a purely random policy.


