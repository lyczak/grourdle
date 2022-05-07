-module(answer_picker_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pick_answer_test()  ->
  CorrectAnswer = answer_picker:pick_answer(),
  ?assertMatch([1,2,3,4], CorrectAnswer).



-endif.