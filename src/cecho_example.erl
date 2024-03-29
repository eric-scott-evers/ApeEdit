%%==============================================================================
%% Copyright (c) 2017, Mazen Harake
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

-module(cecho_example).

-author('mazen.harake@gmail.com').

-include("cecho.hrl").

-export ([countdown/0,
          simple/0,
          colors/0,
          pos/2,
          input/0, input_counter/1,
          cursmove/0,
          helloworld/0
         ]).

%%
%% Simple countdown which shows how to print, move and get coordinates
%%
countdown() ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:move(1, 1),
    Flag = cecho:has_colors(),
    cecho:addstr(io_lib:format("Has color: ~p",[Flag])),
    print_colors(Flag),
    cecho:move(10, 10),
    cecho:addstr("Countdown: "),
    cecho:refresh(),
    count_it_down(10),
    cecho:curs_set(?ceCURS_NORMAL),
    timer:sleep(2000),
    application:stop(cecho).

count_it_down(S) when S =< 0 ->
    cecho:move(10, 22),
    cecho:addstr("BOOOOM!"),
    cecho:refresh();
count_it_down(S) ->
    cecho:move(10+S, 22),
    {X, Y} = cecho:getyx(),
    {MX, MY} = cecho:getmaxyx(),
    cecho:addstr(io_lib:format("~p",[S])),
    cecho:move(22,22),
    cecho:addstr(io_lib:format("~p:~p (~p:~p)",[X,Y,MX,MY])),
    cecho:refresh(),
    timer:sleep(1000),
    count_it_down(S-1).

print_colors(false) -> ok;
print_colors(true) ->
    cecho:start_color(),
    cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    cecho:move(2,1),
    cecho:addstr("Colored!"),
    cecho:refresh(),
    cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok.

%%
%% Simple example to show usage
%%
simple() ->
    application:start(cecho),
    ok = cecho:nocbreak(),
    ok = cecho:cbreak(),
    ok = cecho:echo(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:move(7, 10),
    ok = cecho:addch(45),
    ok = cecho:addch(45),
    ok = cecho:move(7, 12),
    ok = cecho:addstr(" Information ----"),
    ok = cecho:move(8, 10),
    {Row, Col} = cecho:getyx(),
    {MRow, MCol} = cecho:getmaxyx(),
    ok = cecho:addstr(io_lib:format("Row:~p Col:~p MaxRow:~p MaxCol:~p",
				    [Row,Col,MRow,MCol])),
    case cecho:has_colors() of
	true ->
	    cecho:start_color(),
	    ok = cecho:init_pair(1, ?ceCOLOR_BLUE, ?ceCOLOR_WHITE),
	    ok = cecho:init_pair(2, ?ceCOLOR_GREEN, ?ceCOLOR_YELLOW), 
	    cecho:move(9, 10),
	    cecho:attron(?ceCOLOR_PAIR(1) bor ?ceA_BOLD bor ?ceA_UNDERLINE),
	    cecho:addstr(" Has Colors! "),
	    cecho:attron(?ceCOLOR_PAIR(2)),
	    cecho:addstr(" Yes!! "),
	    cecho:attroff(?ceCOLOR_PAIR(1) bor ?ceCOLOR_PAIR(2) bor ?ceA_BOLD bor ?ceA_UNDERLINE);
	false ->
	    cecho:move(9, 10),
	    cecho:addstr(" No colors :( ")
    end,		     
    cecho:addch($!),
    ok = cecho:refresh(),
    timer:sleep(5000),
    cecho:curs_set(?ceCURS_NORMAL),
    application:stop(cecho).

%%
%% Fun example
%%
colors() ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:start_color(),
    ok = cecho:init_pair(1, ?ceCOLOR_BLACK, ?ceCOLOR_RED),
    ok = cecho:init_pair(2, ?ceCOLOR_BLACK, ?ceCOLOR_GREEN),
    ok = cecho:init_pair(3, ?ceCOLOR_BLACK, ?ceCOLOR_YELLOW),
    ok = cecho:init_pair(4, ?ceCOLOR_BLACK, ?ceCOLOR_BLUE),
    ok = cecho:init_pair(5, ?ceCOLOR_BLACK, ?ceCOLOR_MAGENTA),
    ok = cecho:init_pair(6, ?ceCOLOR_BLACK, ?ceCOLOR_CYAN),
    ok = cecho:init_pair(7, ?ceCOLOR_BLACK, ?ceCOLOR_WHITE),
    ok = cecho:init_pair(8, ?ceCOLOR_BLACK, ?ceCOLOR_BLACK),
    rand_seed(),
    {MaxRow, MaxCol} = cecho:getmaxyx(),
    cecho:move(10,10),
    cecho:addstr(io_lib:format("Max Row: ~p, Max Col: ~p",[MaxRow, MaxCol])),
    cecho:move(0, 0),
    cecho:addch($@),
    cecho:move(MaxRow-1, 0),
    cecho:addch($@),
    cecho:move(0, MaxCol-1),
    cecho:addch($@),
    cecho:move(MaxRow-1, MaxCol-1),
    cecho:addch($@),
    cecho:refresh(),
    timer:sleep(2000),
    do_colors(MaxRow, MaxCol, 2000),
    application:stop(cecho).

do_colors(_,_,0) -> ok;
do_colors(MR,MC,N) ->
    ch_colors(MR,MC,1000),
    cecho:refresh(),
    timer:sleep(100),
    do_colors(MR, MC, N-1).

ch_colors(_,_,0) -> ok;
ch_colors(MR, MC, N) ->
    R = rand_uniform(MR)-1,
    C = rand_uniform(MC)-1,
    CN = rand_uniform(8),
    cecho:attron(?ceCOLOR_PAIR(CN)),
    cecho:move(R, C),
    cecho:addch($ ),
    cecho:move(R, C),
    ch_colors(MR, MC, N-1).

-ifdef(random_module_available).
rand_seed() ->
  random:seed(os:timestamp()).
rand_uniform(N) ->
  random:uniform(N).
-else.
rand_seed() ->
  ok.
rand_uniform(N) ->
  rand:uniform(N).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Simply puts an @ character somewhere. If you go out of bounds you crash
%% 
pos(Y, X) ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:move(Y, X),
    cecho:addstr("*"),
    cecho:refresh(),
    timer:sleep(2000),
    application:stop(cecho).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Prints a number continuously as another io thread is waiting for keyinput
%%
input() ->
    application:start(cecho),
    ok = cecho:cbreak(),
    ok = cecho:noecho(),
    ok = cecho:curs_set(?ceCURS_INVISIBLE),
    ok = cecho:keypad(?ceSTDSCR, true),
    spawn_link(?MODULE, input_counter, [0]),
    cecho:mvaddstr(9, 10, "Enter:    "),
    cecho:refresh(),
    input_reader().

input_reader() ->
    P = cecho:getch(),
    case P of
	$q ->
	    application:stop(cecho);
	?ceKEY_F(1) -> 
	    halt();
	_ ->
	    cecho:mvaddstr(9, 17, io_lib:format("~p  ",[P])),
	    cecho:refresh(),
	    input_reader()
    end.

input_counter(N) ->
    cecho:mvaddstr(10, 10, io_lib:format("# ~p",[N])),
    cecho:refresh(),
    timer:sleep(100),
    input_counter(N+1).

%%
%% cursmove - move the '@' around the screen with the arrow keys. 'q' to quit.
%%
cursmove() ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:keypad(?ceSTDSCR, true),
    cecho:mvaddch(10, 10, $*),
    cecho:move(10,10),
    cecho:refresh(),
    moveloop(cecho:getch()).
moveloop(K) when K == ?ceKEY_F(1) ->
    halt();
moveloop(?ceKEY_ESC) ->
    application:stop(cecho);
moveloop(C) ->
    case C of
	?ceKEY_UP -> mv(-1, 0);
	?ceKEY_DOWN -> mv(1, 0);
	?ceKEY_RIGHT -> mv(0, 1);
	?ceKEY_LEFT -> mv(0, -1);
	_ -> ok
    end,
    cecho:refresh(),
    moveloop(cecho:getch()).

mv(OffsetY, OffsetX) ->
    {CY, CX} = cecho:getyx(),
    FinalY = CY+(OffsetY),
    FinalX = CX+(OffsetX),
    %% cecho:mvaddch(FinalY,FinalX, $*),
    P = 10*OffsetX + OffsetY,
    cecho:mvaddstr(9, 17, io_lib:format("~p  ",[P])),
    cecho:move(FinalY, FinalX).

%%
%% helloworld - bounce "Hello World!" on the end of the screen
%%
helloworld() ->
    %% Start application
    application:start(cecho),
    %% Set attributes
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),

    %% Write initial string...
    cecho:mvaddstr(0, 0, "Hello World 42!"),

    cecho:refresh(),

    %% Start the process that will "move" the string
    Mover = spawn(fun() -> mvhello() end),
    ctrl(Mover).

ctrl(Mover) ->
    %% get key-input
    C = cecho:getch(),
    case C of
	$q -> 
	    %% If we get a 'q' then exit the mover and stop cecho
	    exit(Mover, normal),
	    application:stop(cecho),
	    erlang:halt();
	_ ->
	    %% ignore anything else
	    ctrl(Mover)
    end.

%% start the mover
mvhello() -> mvhello(0, 0, 1, 1).

%% take previous pos and direction and print out new string
mvhello(PrevY, PrevX, DirY, DirX) ->
   
    %% "erase" previous position
    cecho:mvaddstr(PrevY, PrevX, "              "),
   
    %% calculate new position and direction
    {NewY, NewX, NewDirY, NewDirX} =
	calc_new_pos(PrevY, PrevX, DirY, DirX),
   
    %% "move" the text to new position
    cecho:mvaddstr(NewY, NewX, "Hello World 42!"),
   
    %% update the screen to show the change
    cecho:refresh(),
   
    %% do it again!
    timer:sleep(50),
    mvhello(NewY, NewX, NewDirY, NewDirX).

calc_new_pos(Py, Px, Dy, Dx) ->
    %% get max coords of the screen
    {My, Mx} = cecho:getmaxyx(),
    %% calc new vertical position and new direction
    {NewPy, NewDy} =
	if (Py+(Dy) >= My) orelse (Py+(Dy) < 0) ->
		{Py+(Dy*-1), Dy*-1};
	   true ->
		{Py+(Dy), Dy}
	end,
    %% calc new horizontal position and new direction
    %% take string length into account
    {NewPx, NewDx} =
	if (Px+(Dx)+12 >= Mx) orelse (Px+(Dx) < 0) ->
		{Px+(Dx*-1), Dx*-1};
	   true ->
		{Px+(Dx), Dx}
	end,
    {NewPy, NewPx, NewDy, NewDx}.
