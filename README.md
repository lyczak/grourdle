# Grourdle!

**Gr**oup Wo**rdle**!
(Made with ❤️, Erlang, and WebSockets)

This project is a proof-of-concept implementation of a variant on the game
[Wordle](https://en.wikipedia.org/wiki/Wordle).
However, in Grourdle, instead of playing alone, a group
of people play together in real-time!

Everyone shares the same game-board and
each round everybody can submit one vote for what they want the next guess to be.
After all the guesses have been submitted, the game will choose whichever guess
seems to be "most representative" of the all of them (if you're curious,
this is achieved by calculating the minimum adjacent edit distance between all
the guesses). After the guess is chosen, an updated game board is sent out to all
the players who can vote on the next guess until they win or run out of guesses.

If you're wondering why this is written in Erlang and not something more sensible,
its because this was done as a final project for our
[Principles of Programming Languages](https://compsci.lafayette.edu/cs301/) class.

## Setup

1. Install make and the Erlang runtime.
2. `make run` from the root directory.
3. Navigate to https://localhost:1312
4. Open a few tabs and join the same game to play against some friends (or yourself).
