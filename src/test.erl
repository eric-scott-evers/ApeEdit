-module(test).
-export ([ 
    start/0
]).

start() ->
    rotate([1,2,3]).

reverse_rotate(L) -> 
    [H|T] = lists:reverse(L),
    Q = lists:reverse(T),
    [H|Q].

reverse_rotate(0, L) -> L;
reverse_rotate(N, L) ->
    LL = reverse_rotate(L),
    reverse_rotate(N-1, LL).

% ----------------------------------------------

rotate([H|T]) ->
    lists:append(T,H).

rotate(0, L) -> L;
rotate(N, L) when N>0 ->
    LL = rotate(L),
    rotate(N-1, LL).