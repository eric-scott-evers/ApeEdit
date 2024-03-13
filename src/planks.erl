%%==============================================================================
%% Copyright (c) 2023, Eric Evers
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%==============================================================================

-module(planks).

-author('Eric Scott Evers').

-include("cecho.hrl").

-export ([
          edit/0, char_queue/0, build_event/0, event_queue/0, border_patrol/1
         ]).

%%
%% A Simple text editor. 'q' to quit.
%%
%%  Data flow:                                                   
%%   
%%    +-> execute_event 
%%   (|)  
%%    +-- execute_event_transaction <----+
%%                                      (|)
%%    +------------------------> event_queue <---+  
%%    :                                          : 
%%    +-- build_event <-+                        +-- border_patrol 
%%                      :
%%    +-> char_queue ---+ 
%%    :             
%%    +-- listen_char   
%%  

edit() ->
    application:start(cecho),
    % application:start(mnesia),
    cecho:cbreak(),
    cecho:noecho(),
    % cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:keypad(?ceSTDSCR, true),
    % ---------------------------------
    % create small database 
    ets:new(db1, [set, named_table, public] ), 
    ets:insert(db1, [{line, ""} ]), 
    ets:insert(db1, [{bbox, {1,1,1,1} } ]), 
    ets:insert(db1, [{buf_label, "buf:"} ]),
    ets:insert(db1, [{buf, "default"} ]), 
    ets:insert(db1, [{file, "default.txt"} ]),
    ets:insert(db1, [{file_label, "file:"} ]),
    ets:insert(db1, [{output, "" } ]),
    ets:insert(db1, [{it, nul}]),
    Spaces = string:copies( " ", 80),
    ets:insert(db1, [{{line, 1}, Spaces} ]),   
    ets:insert(db1, [{{line, 2}, Spaces} ]),
    ets:insert(db1, [{{line, 3}, Spaces} ]),
    ets:insert(db1, [{{line, 4}, Spaces} ]),
    ets:insert(db1, [{{line, 5}, Spaces} ]),

    ets:insert(db1, [{{line, 6}, Spaces} ]),
    ets:insert(db1, [{{line, 7}, Spaces} ]),
    ets:insert(db1, [{{line, 8}, Spaces} ]),
    ets:insert(db1, [{{line, 9}, Spaces} ]),
    ets:insert(db1, [{{line, 10}, Spaces} ]),

    eval_str_init("ok."),          % create first environment val
    
    Event_Queue = spawn(?MODULE, event_queue, []), 
    register(event_queue, Event_Queue),

    Build_Event = spawn(?MODULE, build_event, []),
    register(build_event, Build_Event),

    Char_Queue = spawn(?MODULE, char_queue, []),
    register(char_queue, Char_Queue),

    Border_Patrol = spawn(?MODULE, border_patrol, [1]),
    register(border_patrol, Border_Patrol),

    {_MY,MX} = cecho:getmaxyx(),
    Top_border = string:copies( "-", MX-21),
    cecho:mvaddstr(0,1, io_lib:format( "~p", ["Create something." ] )),
    cecho:mvaddstr(0,19, io_lib:format( "~p", [ Top_border ] )),
    cecho:move(0, MX-1), cecho:addch(32),
    cecho:move(1,1),
    cecho:refresh(),
    listen_char().

listen_char() ->   
    Key = cecho:getch(),
    char_queue ! Key,
    listen_char().

char_queue() ->    
    receive
        _Alt = 27   -> build_event ! {alt, $k};
        Char        -> build_event ! Char
        after 1000 -> ok
    end, 
    char_queue().

event_queue() ->    
    receive
        {Action}          ->   execute_event({Action});
        {Action, Char}    ->   execute_event({Action, Char});
        [{{loc_store},{mv, Y, X}, C},{loc_restore}] ->  
                            execute_event_transaction([
                                {loc_store},  
                                {{mv, Y, X}, C},
                                {loc_restore} ,{refresh} ]);
        [H|T]               -> execute_event_transaction([H|T])
                        
    after 1000 -> ok
    end, 
    event_queue().

execute_event_transaction([]) -> ok;
execute_event_transaction(T) when is_tuple(T) -> execute_event(T); 
execute_event_transaction([H|T]) ->
    execute_event(H),
    execute_event_transaction(T).

execute_event(Event) -> 
    case Event of
        { space }               -> space_key();
        { backspace }           -> backspace_key();
        { move_left_all }       -> {CY,_CX} = cecho:getyx(), cecho:mvaddch(CY,0,32);
        { get_line }            -> [{line, Line}] = ets:lookup( db1, line), ets:insert( db1, {it, Line});
        { line_into_db }        -> [{line, Line}] = ets:lookup( db1, line), 
                                    {CY,_CX} = cecho:getyx(), 
                                    ets:insert(db1, {{line,CY}, Line});
        { get, Tag }           ->  [{Tag, Value}] = ets:lookup( db1, Tag), ets:insert( db1, {it, Value});
        { eval_str }            -> [{it, It}] = ets:lookup( db1, it), Out = eval_str(It), ets:insert( db1, {it, Out});
        { put_into_cmd }        -> [{it, It}] = ets:lookup( db1, it), cecho:mvaddstr( 20, 20, io_lib:format( "~p", [It] ));
        { show_in_cmd_bar, Col} -> [{it, It}] = ets:lookup( db1, it), 
                                     {MY,_MX} = cecho:getmaxyx(),
                                     cecho:mvaddstr(MY-2, Col, io_lib:format( "~p", [It] ));
        { put_into_remark }     -> [{it, It}] = ets:lookup( db1, it), 
                                    {CY,_CX} = cecho:getyx(),               
                                    cecho:mvaddstr( CY+1, 1, io_lib:format( "% ~p", [It] ));  
        { clear_line }          -> ets:insert( db1, {line, ""});
        { delta, DY, DX}        -> mv(DY,DX);   
        { loc_store}            -> ets:insert( db1, { loc, cecho:getyx() } ); 
        { loc_restore}          -> [{loc,{JY,JX}}] = ets:lookup( db1, loc), cecho:move(JY,JX);
        {{ mv, Y, X}, C}        -> cecho:mvaddch(Y,X,C); 
        { mvaddstr, Row, Col, Data } -> 
                                   cecho:mvaddstr(Row, Col, io_lib:format( "~p", [Data] ));
        { refresh}              -> cecho:refresh();
        { kill_line}      -> kill_line();   
        { show, Char }          -> show(Char);
        {_Action, Char}         -> show(Char)
    end.
% 
% A process to draw the boarder: 
%   
border_patrol(A) -> 
    receive
        _ -> ok
    after 20 -> ok
    end,
    {MY, MX} = cecho:getmaxyx(),
    % draw command line border 
    Col = A,
    if 
        Col==20 ->
            % draw cursor coordinates (row x col) in command bar 
            % {CY, CX} = cecho:getyx(),
            %event_queue ! [ {{loc_store}, {mv, MY-2, 20}, $( }, {loc_restore} ], 
            %event_queue ! [ {{loc_store}, {mv, MY-2, 21}, 32 }, {loc_restore} ], 
            %event_queue ! [ {{loc_store}, {mv, MY-2, 22}, 32 }, {loc_restore} ],  
            %event_queue ! [ {{loc_store}, {mv, MY-2, 23}, 32 }, {loc_restore} ],
            %event_queue ! [ {{loc_store}, {mv, MY-2, 24}, $x }, {loc_restore} ], 
            %event_queue ! [ {{loc_store}, {mv, MY-2, 25}, 32 }, {loc_restore} ],
            %event_queue ! [ {{loc_store}, {mv, MY-2, 26}, 32 }, {loc_restore} ], 
            %event_queue ! [ {{loc_store}, {mv, MY-2, 27}, 32 }, {loc_restore} ],
            %event_queue ! [ {{loc_store}, {mv, MY-2, 28}, $) }, {loc_restore} ],  
            % numbers do not create quotes 
            % event_queue ! [ {loc_store}, {mvaddstr, MY-2, 22, CY }, {loc_restore} ], 
            % event_queue ! [ {loc_store}, {mvaddstr, MY-2, 25, CX }, {loc_restore} ],
            event_queue ! [ {refresh} ];
        Col==40 ->
            % draw buffer name in command bar
            event_queue ! [ {loc_store}, {get, buf_label}, { show_in_cmd_bar, 40}, {loc_restore},{refresh} ],
            event_queue ! [ {loc_store}, {get, buf}, { show_in_cmd_bar, 45}, {loc_restore},{refresh} ],
            event_queue ! [ {loc_store}, {{mv, MY-2, 40}, 32}, {loc_restore},{refresh} ];
        Col==60 ->
            % draw file name in command bar 
            %event_queue ! [{loc_store}, {get, file_label}, {show_in_cmd_bar, 60}, {loc_restore},{refresh} ],
            %event_queue ! [{loc_store}, {get, file}, {show_in_cmd_bar, 66}, {loc_restore},{refresh} ],
            %event_queue ! [{loc_store}, {{mv, MY-2, 60}, 32}, {loc_restore},{refresh} ];
            event_queue ! [{refresh}];
        Col==80 ->
            % make room for char code 
            %event_queue ! [ {loc_store}, {mvaddstr, MY-2, 80, "     "}, {loc_restore}],
            %event_queue ! [ {loc_store}, {{mv, MY-2, 80}, 32}, {loc_restore} ],
            event_queue ! [ {refresh}];
        true -> 
            % draw line segments of command bar
            event_queue ! [ {loc_store}, {mvaddstr, MY-3, 1, string:copies( " ", MX-3)}, {loc_restore},{refresh}],
            event_queue ! [ {{loc_store}, {mv, MY-3,    1}, 32}, {loc_restore} ],
            event_queue ! [ {{loc_store}, {mv, MY-3, MX-1}, 32}, {loc_restore} ],
            % --- 
            event_queue ! [ {loc_store}, {mvaddstr, MY-1, 1, string:copies( " ", MX-3)}, {loc_restore},{refresh} ],
            event_queue ! [ {loc_store}, {{mv, MY-1,    1}, 32}, {loc_restore} ],
            event_queue ! [ {loc_store}, {{mv, MY-1, MX-1}, 32}, {loc_restore} ],
            % ---
            Bar_Len = 19, 
            X1 =  min(MX, A), 
            X2 = min(MX, A+Bar_Len), 
            Len =  max(0, min(A + Bar_Len-1, MX-2) - A),
            event_queue ! [ {loc_store}, {mvaddstr, MY-2, X1, string:copies( "=", Len)}, {loc_restore},{refresh} ],
            event_queue ! [ {loc_store}, {{mv, MY-2, MX-1}, $=}, {loc_restore}  ],
            event_queue ! [ {loc_store}, {{mv, MY-2, X1}, $=}, {loc_restore} ],
            event_queue ! [ {loc_store}, {{mv, MY-2, X2}, $=}, {loc_restore} ],
            event_queue ! [ {refresh}]
    end,
    AA = (A+20) rem MX,  
    AAA = 20*round(AA/20),
    border_patrol( AAA ).     

%% build_event( K) when K == ?ceKEY_F(1) ->
%%    halt();
%% build_event( ?ceKEY_ESC) ->
%%     application:stop(cecho);
build_event( ) -> 
    receive
        C -> C 
    end, 
    {_, _CX} = cecho:getyx(),  
    {JY,JX}  = cecho:getyx(),  % remember jumpback location  
    case C of
	  ?ceKEY_UP        -> event_queue ! [{delta, -1,  0}];    
	  ?ceKEY_DOWN      -> event_queue ! [{delta,  1,  0}];  
	  ?ceKEY_RIGHT     -> event_queue ! [{delta,  0,  1}];  
	  ?ceKEY_LEFT      -> event_queue ! [{delta,  0, -1}];   
      ?ceKEY_ENTER     -> event_queue ! [{line_into_db}, {delta, 1, 0}, {move_left_all}, {clear_line}, {refresh} ];   
      _BACKSPACE = 263 -> event_queue ! {backspace};    % backup_key();
      _Space = 32      -> event_queue ! {space};
      ?ceKEY_DEL       -> event_queue ! [{delta, 0, -1}, {action, 32},{delta, 0, -1}]; 
      ?ceCtrl_s        -> ask_filename(), cecho:move(JY, JX);
      ?ceCtrl_r        -> cecho:addstr("read");
      _Ctrl_k = 11     -> show($K); 
      _Ctrl_l = 12     -> redraw();
      _Ctrl_e = 5      -> execute_line();
      _Ctrl_y = 25     -> yank_kill_buf();
      _Ctrl_g = 7      -> event_queue ! {kill_line};     % grab line 
      {alt, _Key}        -> event_queue ! {kill_line};
      % _Alt = 27         -> event_queue ! {kill_line};   
	  A                 -> event_queue ! [{show, A}]
    end,
    cecho:curs_set(?ceCURS_NORMAL),
    build_event().  

backspace_key() -> 
    {_CY, CX} = cecho:getyx(),
    backupspace_key( CX).

backupspace_key( CX) when (CX < 2) ->
    % do nothing case 
    ok;

backupspace_key(_CX) ->
    {CY, CX} = cecho:getyx(),
    % ----------------------------------
    {_MY, MX}  = cecho:getmaxyx(),
    show_row_and_col(CY, CX+1),
    [{{line, CY}, Line}]      = ets:lookup(db1, {line, CY}),
    % -------------------------------------------------
    Rotated_Line          = rotate(CX-2, Line), 
    [_H|T]                = Rotated_Line,
    Updated_Line_1        = reverse_rotate(CX-2,T), 
    if
        length(Updated_Line_1) < 80 -> Updated_Line = lists:append(Updated_Line_1," ");
        true -> Updated_Line = Updated_Line_1
    end,
    % mark   
    % save Updated_Line        

    ets:insert(db1, {line,       Updated_Line}),
    ets:insert(db1, {{line, CY}, Updated_Line}),
    % print Updated_Line 
    cecho:mvaddstr(CY, 1, io_lib:format("~s     ", [lists:sublist(Updated_Line,MX-3)] )),
    cecho:move(CY, CX-1),
    ok.

show_row_and_col(CY, CX) ->
    {MY, _MX}   = cecho:getmaxyx(),
    cecho:mvaddstr(MY-2, 10, io_lib:format("(~w x ~w)==", [CY, CX] )),
    cecho:move(CY, CX).

space_key() ->
        {CY, CX} = cecho:getyx(),
        % ------------------------------------------- update line 
        { _MY, MX}   = cecho:getmaxyx(),
        { JY, JX}   = cecho:getyx(),    % remember jumpback location
        show_row_and_col(CY, CX+1),

        [{{line, CY}, Line}]      = ets:lookup(db1, {line, CY}),

        Rotated_Line          = rotate(CX-1, Line), 
        Updated_Line          = reverse_rotate( CX-1, [32|Rotated_Line] ), 

        % -------------------------------------------- save Updated_Line        
        ets:insert(db1, {line,       Updated_Line}),
        ets:insert(db1, {{line, CY}, Updated_Line}),
        % print Updated_Line 
        cecho:mvaddstr(CY, 1, io_lib:format("~s", [lists:sublist(Updated_Line,MX-3)] )),
        if
            length(Updated_Line) > (MX-2) ->  
                cecho:mvaddstr(CY, MX-1, io_lib:format("%", [] )),  
                cecho:move(JY,JX);
            true -> ok
        end,
        cecho:move(CY, CX+1),
        cecho:refresh(),
        ok.

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
    lists:append(T,[H]).

rotate(0, L) -> L;
rotate(N, L) when N>0 ->
    LL = rotate(L),
    rotate(N-1, LL).

% -----------------------------------------------

yank_kill_buf() ->
    %
    % build Updated_Line
    %
    {CY, _CX} = cecho:getyx(),
    [{kill_buf, Kill_Buf}] = ets:lookup(db1, kill_buf),
    [{{line, CY}, Line}]   = ets:lookup(db1, {line, CY}),
    Updated_Line           = lists:append(Line, Kill_Buf),
    %   
    % save Updated_Line 
    %
    ets:insert(db1, {line,       Updated_Line}),
    ets:insert(db1, {{line, CY}, Updated_Line}),
    % print Updated_Line 
    cecho:mvaddstr(CY, 1, io_lib:format("~s", [Updated_Line] )),
    ok.

% kill line  with d 
kill_line() -> 
    {CY, _CX} = cecho:getyx(),
    % get line of text at line number CY 
    [{{line, CY}, Line}] = ets:lookup(db1, {line, CY}),
    % put it into kill buffer 
    ets:insert(db1, {kill_buf, Line}),
    ets:insert(db1, {line, ""}),
    ets:insert(db1, {{line, CY}, ""}),
    % erase line 
    cecho:mvaddstr(CY, 1, io_lib:format("                                                 ", [] )),
    cecho:move(CY+1,1),
    cecho:refresh(),
    ok.

execute_line() -> event_queue !
     [  {delta,  1,  0}, {move_left_all}, {delta, 0, 1},
        {loc_store},
        {get_line}, {eval_str}, {put_into_remark}, {clear_line},
        {loc_restore}, {delta, 2, 0}]. 

redraw() ->
    ok.

read_filename(Filename) ->
    C = _Char = cecho:getch(),
    case C of  
      ?ceKEY_ENTER -> Filename;  
      A -> cecho:addch(A), 
            cecho:refresh(), 
            read_filename(Filename++[A])
    end.

ask_filename() ->
  {MY, _MX} = cecho:getmaxyx(),
  cecho:mvaddstr(MY-1, 1, io_lib:format(" filename: ", [] )),    
  Filename = read_filename([]),
  cecho:mvaddstr(MY-2, 50, io_lib:format(" saved filename:~s ",[Filename] )),
  file:write_file(Filename, "test data"),
  ok.

mv(OffsetY, OffsetX) ->
    {CY, CX} = cecho:getyx(),
    {MY, MX} = cecho:getmaxyx(),
    FinalY = min(MY-3, max(1, CY + OffsetY)), % lowest line no is 1
    FinalX = min(MX-3, max(1, CX + OffsetX)), % lowest col no is 1
    cecho:move(FinalY, FinalX),
    cecho:refresh().

show(Char) ->  
    {CY, CX} = cecho:getyx(),
    {MY, _MX} = cecho:getmaxyx(),
    cecho:addch(Char),
    % ets:insert( db1, { loc, cecho:getyx() } ), 
    %% build and save line 
    [{line,Line}] = ets:lookup(db1, line),
    Updated_Line = lists:append(Line,[Char]),      %  a way to append 
    ets:insert(db1, {line, Updated_Line} ),        %  Update this line
    ets:insert(db1, {{line, CY}, Updated_Line}),   %  Update numbered line 
    % show character ascii code 
    D0 = round(math:floor(Char/100)),  % digit one 
    C1 = Char-D0*100,
    D1 = round(math:floor(C1/10)),  % digit one 
    D2 = Char - D1*10 - D0*100, 
    cecho:mvaddch(MY-2, 80, 48+D0),           
    cecho:mvaddch(MY-2, 81, 48+D1),
    cecho:mvaddch(MY-2, 82, 48+D2), 
    % cecho:mvaddstr(MY-2, 80, io_lib:format(" ~p ", [Char] )),
    % cecho:refresh(),
    % [{loc,{JY,JX}}] = ets:lookup( db1, loc), 
    % cecho:move(JY,JX), 
    % cecho:mvaddstr(MY-4, round(MX/2), io_lib:format(" ~p ", [Line_2] )),
    % cecho:mvaddstr(MY-4, round(MX/2), io_lib:format(" ~p ", [Output] )),
    % P = Char,
    cecho:move(CY, CX+1),
    cecho:refresh().    % P. 

eval_str(Expression) ->
   [{env, Env}]             = ets:lookup( db1, env),
   {ok, Tokens, _}          = erl_scan:string(Expression),
   {ok, Parsed}             = erl_parse:parse_exprs(Tokens),
   {value, Result, New_Env} = erl_eval:exprs(Parsed, Env),
   ets:insert( db1, {env, New_Env}), 
   Result. 

eval_str_init(Expression) ->
    {ok, Tokens, _}       = erl_scan:string(Expression),
    {ok, Parsed}          = erl_parse:parse_exprs(Tokens),
    {value, Result, Env}  = erl_eval:exprs(Parsed, []),
    ets:insert( db1, {env, Env}), 
    Result. 

