-module(guess_arbiter).

-export([choose_guess/1]).



%% @doc Pick/generate a Wordle guess from a list of several guesses.
choose_guess([Guess | NextGuess]) ->
  ChosenGuess = lists:nth(rand:uniform(length([Guess | NextGuess])), [Guess | NextGuess]),
  io:fwrite("~s",[ChosenGuess]).




