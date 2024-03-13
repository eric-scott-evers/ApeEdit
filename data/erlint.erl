-module(erlint).
-export([lint/1]).

lint(File) ->
    {ok,_,Bin} = compile:file(File,[debug_info,binary]),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Bin,[abstract_code]),
    erl_lint:module(AC,File).

