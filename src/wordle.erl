-module(wordle).

-import(string,[sub_string/3]).

-export([check_guess/2]).

-export([same_check/3]).

-export([while/1,while/2, run/0]).


%% @doc Check a Wordle guess against a provided word.
%% @returns A tuple containing the Guess and a list of
%% colors based on where the letters appear in the provided Word.
check_guess(Guess, Word) ->
  %%Board = board(),
  A1 = sub_string(Word,1,1),
  A2 = sub_string(Word,2,2),
  A3 = sub_string(Word,3,3),
  A4 = sub_string(Word,4,4),
  A5 = sub_string(Word,5,5),
  AnswerCharacters = [A1,A2,A3,A4,A5],
  AnswerCharacters,

  if
    length(Guess) == 6  ->  %% 1st and 7th is for "" for user input
      %%io:format("~s",[Guess]),
      C1 = sub_string(Guess,1,1),
      C2 = sub_string(Guess,2,2),
      C3 = sub_string(Guess,3,3),
      C4 = sub_string(Guess,4,4),
      C5 = sub_string(Guess,5,5),
      GuessCharacters = [C1, C2, C3, C4, C5],
      GuessCharacters,

      %%Board = {Guess, [same_check(C1, A1, AnswerCharacters), same_check(C2, A2, AnswerCharacters), same_check(C3, A3, AnswerCharacters), same_check(C4, A4, AnswerCharacters), same_check(C5, A5, AnswerCharacters)]},
      Board = [same_check(C1, A1, AnswerCharacters), same_check(C2, A2, AnswerCharacters), same_check(C3, A3, AnswerCharacters), same_check(C4, A4, AnswerCharacters), same_check(C5, A5, AnswerCharacters)],

      Board;


true -> Error = "Guess is not 5 letters",
      io:fwrite("~s ~n", [Error])

  end.


same_check(C1, C2, Characters)  ->
  YellowCheck = lists:member(C1, Characters),
  if
    C1 == C2 -> green;
    true -> if
              YellowCheck -> yellow;
              true -> grey
            end
  end.




while(L) -> while(L,0).
while([], Acc) -> Acc;

while([_|T], Acc) ->
  Guess = io:get_line("Enter a Guess: "),
  io:fwrite(lists:flatten(io_lib:format("~p ~n",[check_guess(Guess, "bread")]))), %%prints the resulting board from check guess
while(T,Acc+1).

run() ->
  NumberOfGuesses = [1,2,3,4,5],
  while(NumberOfGuesses).