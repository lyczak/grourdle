-module(answer_picker_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pick_answer_test()  ->
  ?assert(is_list([1,2,3,4]), is_list(answer_picker:pick_answer())).

make_list_test()  ->
  ?assert(is_list([1,2,3,4]), is_list(answer_picker:make_list())).

binary_list_to_string_list_test()  ->
  ?assert(is_list([1,2,3,4]), is_list(answer_picker:binary_list_to_string_list([1,1,0,1,0]))).


-endif.