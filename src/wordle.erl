-module(wordle).
-import(string,[equal/2]).
-export([check_guess/2]).

%% @doc Check a Wordle guess against a provided word.
%% @returns A tuple containing the Guess and a list of
%% colors based on where the letters appear in the provided Word.
check_guess(Guess, Word) ->
  check_guess_recursion(Guess, Word, Word, []).

check_guess_recursion([], [], FullWord, Result) ->
  Result;

check_guess_recursion(Guess, Word, FullWord, Result) ->
  [Ghead | Gtail] = Guess,
  [Whead | Wtail] = Word,
  case Ghead =:= Whead of
    true -> Result = Result ++ green;
    false -> Result = Result
  end,
  case lists:member(Ghead, FullWord) =:= true of
    true -> Result = Result ++ yellow;
    false -> Result = Result ++ grey
  end,
  check_guess_recursion(Gtail, Wtail, FullWord, Result).