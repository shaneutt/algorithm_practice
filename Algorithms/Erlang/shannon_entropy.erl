%% @doc Shannon Entropy Algorithm
%%
%% Erlang implementation of the Shannon Entropy Algorithm:
%% https://en.wiktionary.org/wiki/Shannon_entropy
%%
%% @end

-module(shannon_entropy).
-export([calculate/1]).

%% -----------------------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------------------

-spec calculate(Input :: integer() | string() | binary()) -> Float :: float().

calculate(Input) when is_integer(Input) ->
  BinaryInput = integer_to_binary(Input),
  calculate(BinaryInput);
calculate(Input) when is_list(Input) ->
  BinaryInput = iolist_to_binary(Input),
  calculate(BinaryInput);
calculate(Input) ->
  CounterMap   = build_counter(Input, []),
  InputLength  = size(Input),
  EntropyParts = [ entropy_part(Count, InputLength) || {_Char, Count} <- CounterMap ],
  -1 * lists:sum(EntropyParts).

%% -----------------------------------------------------------------------
%% Private Functions
%% -----------------------------------------------------------------------

build_counter(<<>>, CounterMap) -> CounterMap;
build_counter(<<Character:8, Remainder/binary>>, CounterMap) ->
  NewCount = case lists:keyfind(Character, 1, CounterMap) of
    false              -> 1;
    {Character, Count} -> Count + 1
  end,
  NewCounterMap = lists:keystore(Character, 1, CounterMap, {Character, NewCount}),
  build_counter(Remainder, NewCounterMap).

entropy_part(Number, InputLength) ->
  Float = float(Number),
  Div   = Float / InputLength,
  Div * math:log2(Div).

%% -----------------------------------------------------------------------
%% Unit Tests
%% -----------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

calculate_test_() ->
  [
    ?_assertEqual(
      calculate("122333444455555666666777777788888888"),
      2.7942086837942446
    ),
    ?_assertEqual(
      calculate(563881467447538846567288767728553786),
      2.7942086837942446
    ),
    ?_assertEqual(
      calculate("https://www.reddit.com/r/dailyprogrammer"),
      4.056198332810094
    ),
    ?_assertEqual(
      calculate("int main(int argc, char *argv[])"),
      3.8667292966721747
    )
  ].

-endif.

