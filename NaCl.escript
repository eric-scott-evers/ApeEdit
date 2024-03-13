#!/usr/bin/env escript
%%! -noinput -pa ../cecho/_build/default/lib/cecho/ebin +A 50

%% Read the LICENSE file
-include_lib("cecho/include/cecho.hrl").
main(Arg_List) ->
    Length = length(Arg_List),
    if
        Length == 1 ->
            [Filename] = Arg_List, 
            io:format("filename: " ++ Filename ++ "~n", []),
            timer:sleep(1000),
            plank3:edit(Filename);
        true ->
            plank3:edit()
    end,
    ok.



