-module(answer_picker).

%% API
-export([pick_answer/0]).

binary_list_to_string_list(BinaryList) when length(BinaryList) == 0 -> [];
binary_list_to_string_list(BinaryList) ->
  [binary:bin_to_list(hd(BinaryList)) | binary_list_to_string_list(tl(BinaryList))].

make_list () ->
  {ok, File} = file:read_file("../../possible_answers.json"),
  binary_list_to_string_list(jsx:decode(File)).

pick_answer() ->
  List = make_list(),
  R0 = rand:uniform(length(List)),
  lists:nth(R0, List).

