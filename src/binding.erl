
-module(binding).

-author('Eric Scott Evers').

-include("cecho.hrl").

-export ([
          start/0
         ]).

start() -> 
    ets:insert(db1, [{{bind, 25}, {send, {yank_kill_buf}    }}]),   % control y
    ets:insert(db1, [{{bind,  8}, {send, {help_info}        }}]),   % control h
    ets:insert(db1, [{{bind,  7}, {send, [{kill_line}]      }}]),   % control g, kill line
    ets:insert(db1, [{{bind,  5}, {send, {execute_line}     }}]),   % control e

    ets:insert(db1, [{{bind, 11}, {send, [{kill_line}]             }}]),   % control k
    ets:insert(db1, [{{bind, 20}, {send, [{tether}]                }}]),   % control t
    ets:insert(db1, [{{bind, 12}, {send, [{toggle_update_screen}]  }}]),   % _Ctrl_l
    % save buffer to file on path
    ets:insert(db1, [{{bind, 19}, {send, [{save_file}]             }}]),   % control s
    
    {ok, done}.

