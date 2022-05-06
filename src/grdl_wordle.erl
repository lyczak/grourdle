-module(grdl_wordle).
-import(string,[equal/2]).
-export([get_answer/0, check_guess/2]).
%% @doc Reads from the possible_answers.json file which contains all of the possible correct answers for wordle.
%% Reads the json file answers and stores them in a list.
%% @return P, list of possible answers.
make_answers_list() ->
  PrivDir = case code:priv_dir(grourdle) of
    {error, bad_name} ->
      "priv";
    P ->
      P
  end,
  {ok, File} = file:read_file(filename:join([PrivDir, "possible_answers.json"])),
  jsx:decode(File).

%% @doc Reads the list of possible answers and converts the randomly chosen answer from binary to a list (Erlang string).
%% @return binary_to_list(list:nth(RO, List))., function that returns the randomly selected binary answer to a string as an Erlang list.
get_answer() ->
  List = make_answers_list(),
  R0 = rand:uniform(length(List)),
  binary_to_list(lists:nth(R0, List)).


%% @doc Check a Wordle guess against a provided word.
%%checks for exact matches, then checks remaining letters for yellows, default grey
%%3> wordle:check_guess("crate","cheek").
%%    [green,grey,grey,grey,yellow]
%%4> wordle:check_guess("check","cheek").
%%    [green,green,green,grey,green]
%% @param G or Guess, The specified Guess to be compared to the Answer.
%% @param W or Word, the chosen Answer for the current game of Grourdle for all of the guesses to be compared to.
%% @returns A tuple containing the Guess and a list of
%% colors based on where the letters appear in the provided Word.
check_guess(G, W) when is_binary(G) ->
  check_guess(binary_to_list(G), W);

check_guess(G, W) when is_binary(W) ->
  check_guess(G, binary_to_list(W));

check_guess(Guess, Word) ->
  Res = check_for_green(Guess, 1, Word, [grey,grey,grey,grey,grey]),
  New_guess = element(1, Res),
  New_word = element(2, Res),
  New_result = element(3, Res),
  Final_res = check_for_yellow(New_guess, 1, New_word, New_result),
  element(3, Final_res).

%% @doc Function to calculate the comparison case for a green character (which means the character is correct for the correct Answer.
%% @param Guess, The specified guess for this comparison.
%% @param Index, the index that corresponds to the character in both the guess and the correct answer.
%% @param Word, the correct answer for the comparison to be made.
%% @param Result, which is ether set to green or not changed, with the index of the character and the Guess and the Correct Word.
%% @return Either a tuple containing {Guess, Word, green} or the original resulting character color. Returns the check
%% for the green case.
check_for_green(Guess, Index, Word, Result) ->
  case Index > length(Word) of
    true -> {Guess, Word, Result};
    false ->
      G_letter = getnth(Index, Guess),
      W_letter = getnth(Index, Word),
      case G_letter =:= W_letter of
        true -> New_result = setnth(Index, Result, green),
          check_for_green(setnth(Index, Guess, "-"), Index, setnth(Index, Word, "_"), New_result);
        false -> check_for_green(Guess, Index + 1, Word, Result)
      end
  end.
%% @doc Function to calculate the comparison case for a yellow character (which means the character is not in the correct position
%% but is within the correct word.
%% @param Guess, The specified guess for this comparison.
%% @param Index, the index that corresponds to the character in both the guess and the correct answer.
%% @param Word, the correct answer for the comparison to be made.
%% @param Result, which is ether set to yellow or not changed, with the index of the character and the Guess and the Correct Word.
%% @return Either a tuple containing {Guess, Word, yellow} or the original resulting character color. Returns the check
%% for the yellow case.
check_for_yellow(Guess, Index, Word, Result) ->
  case Index > length(Word) of
    true -> {Guess, Word, Result};
    false ->
      G_letter = getnth(Index, Guess),
      case (lists:member(G_letter, Word) andalso getnth(Index, Result) =:= grey) of
        true ->
          New_result = setnth(Index, Result, yellow),
          W_ind = index_of(G_letter, Word),
          check_for_yellow(setnth(Index, Guess, "-"), Index, setnth(W_ind, Word, "_"), New_result);
        false -> check_for_yellow(Guess, Index + 1, Word, Result)
      end
  end.

%% @doc Functions that are already within Erlang but re-written here for organizational purposes for work flow.
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

getnth(1, [N|_]) -> N;
getnth(I, [_|Rest]) -> getnth(I-1, Rest).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).