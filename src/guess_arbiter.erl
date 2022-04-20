-module(guess_arbiter).

%%-export([choose_guess_helper/2]).

-export([choose_guess/1]).


%%choose_guess_helper([Guess | NextGuess], G) ->
 %% _.
%%SHORTCUT RUN: "control" g
%% @doc Pick/generate a Wordle guess from a list of several guesses.
choose_guess([Guess | NextGuess]) ->
  %%io:format("~s", [First]),
  %%io:format("~s", [Second]),  %%gotta figure out what this part does - i think it prints as string?
  %%io:format("~s", [Third]),
  G = digraph:new(),
  io:format("~s", [Guess]),

  [G:add_vertex(G, Guess)|choose_guess(NextGuess)].
    %%Data = {First, Second, Third},
  %%digraph:add_vertex(G, First),
  %%digraph:add_vertex(G, Second),
  %%digraph:add_vertex(G, Third),
  %%Data = {digraph:vertex(G, First), digraph:vertex(G, Second), digraph:vertex(G, Third)}.
  %%digraph:vertices(G).
  %%digraph_utils:preorder(G).
  %%digraph:add_edge(G, First, Third, 4).
%%digraph_utils:postorder().
%%digraph:vertices/1(G).
%%digraph:add_edge(G, G:get(First), G:get(Third), 16). %% NOTTTEEEE: for this to work u mgiht have to make the vertexes first and then add the edge first wihout addig the vertexs



















