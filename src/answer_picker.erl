-module(answer_picker).

%% API
-export([pick_answer/0, make_list/0, binary_list_to_string_list/1]).

%% @doc takes a list of binary values and turns the list into all strings
%% @param a list of binary values, BinaryList
%% @returns a list of string values
binary_list_to_string_list(BinaryList) when length(BinaryList) == 0 -> [];
binary_list_to_string_list(BinaryList) ->
  [binary:bin_to_list(hd(BinaryList)) | binary_list_to_string_list(tl(BinaryList))].

%% @doc pulls from the .json file and turns all binary values into strings
%% @param takes no parameter
%% @returns a list of strings from the .json file
make_list () ->
  {ok, File} = file:read_file("../../possible_answers.json"),
  binary_list_to_string_list(jsx:decode(File)).

%% @doc returns a random string read from the .json file to act as
%% the answer to the game.
%% @param takes no parameter
%% @returns a single string read from .json answers
pick_answer() ->
  List = make_list(),
  R0 = rand:uniform(length(List)),
  lists:nth(R0, List).

