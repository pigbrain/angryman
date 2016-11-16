
-module(am_util).
-author("pigbrain").

%% API
-export([number_to_binary/1,
    number_to_list/1,
    print_list/1]).

number_to_binary(N) when is_integer(N) -> integer_to_binary(N);
number_to_binary(N) when is_float(N) -> float_to_binary(N).

number_to_list(N) when is_integer(N) -> integer_to_list(N);
number_to_list(N) when is_float(N) -> float_to_list(N, [{decimals, 5}]).

print_list([]) ->
    void;
print_list([H|T]) ->
    io:format("list element[~p]~n", H),
    print_list(T).