-module(hello).
-export([hello_world/0]).

hello_world() ->
    X = io:fwrite("hello, world\n"),
    Test = true, 
    Y = X.

