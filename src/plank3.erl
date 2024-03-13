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

-module(plank3).

-author('Eric Scott Evers').

-include("cecho.hrl").

-export ([
          edit/0, edit/1, char_queue/0, build_event/0, event_queue/1, char_queue_for_tether/0,
          tether/0, track_cursor/0, loop_ant/2, track_screen_size/0, track_buffer/0,
          update_screen/0, update_border/0, update_state/0, update_node/0, yank_kill_buf/0,
          help_info/0, execute_line/0, helper/0, eval_str/1, track_file/0,
          minibuffer_reader/0, track_path/0, ignore_data_queue/0, menu_event_queue/1,
          set_timeout_for_key_binding/2, update_menu_bar/0, clean_up_screen/0, update_color/0,
          store_nodes/0
         ]).

%%==========================================================================
%%
%%  A Simple text editor written in erlang.  
%%
%%  Architecture:
%%
%%  Data flow:
%%
%%    terminal -> cecho.c -> cecho.erl -> listen_char...
%%    
%%    ...listen_char -> char_queue_for_tether -> char_queue -> build_event --> +
%%                                                                             |
%%                                               + <---------------------------+                                                  
%%                                               | 
%%      +--- event_queue <------------------ <---+
%%      |                                       /|\
%%      +--> execute_event_transaction --+       |
%%                                       |       |
%%      +--  execute_event  <------------+       |
%%      |                                        |
%%     \|/                                       |                                                  
%%   cecho.erl                                   + <-- track_cursor location
%%      |                                        |
%%     \|/                                       + <-- track_buffer name
%%   cecho.c                                     |
%%      |                                        + <-- track_filename
%%     \|/                                       |
%%   terminal                                    + <-- update_border
%%                                               |
%%                                               + <-- update_screen
%%                                               |
%%                                               + <-- track_path
%%                                               |
%%                                               + <-- track_node
%%                                               |
%%                                               + <-- menu_event_queue
%%         
%% ======================================================================= 

edit() -> 
    edit("default.txt").

    % edit("default.txt").

% test(Filename) ->
%     application:start(cecho),
%     % application:start(mnesia),
%     cecho:cbreak(),
%     cecho:noecho(),
%     % cecho:curs_set(?ceCURS_INVISIBLE),
%     cecho:keypad(?ceSTDSCR, true),

%     build_node_name(),

%     init_database(),
    
%     start_queue_services(),
%     register(listen_char, self()),

%     {_MY,MX} = cecho:getmaxyx(),
%     Top_border = string:copies( "-", MX-40),
%     % cecho:mvaddstr( 0,1, ["  file    | Settings | Create something."] ),
%     cecho:mvaddstr( 0,40, [ Top_border ] ),
%     cecho:move(0, MX-1), cecho:addch(32),
%     cecho:move(1,2),  % (1,1) actual
%     cecho:refresh(),
%     start_worker_services(),
%     set_up_default_values(Filename),
%     set_color(none),
%     listen_char().  


edit(Filename) ->
    application:start(cecho),
    % application:start(mnesia),
    cecho:cbreak(),
    cecho:noecho(),
    % cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:keypad(?ceSTDSCR, true),

    build_node_name(),    % create the node name 

    init_database(),
    % set_up_default_values(Filename),
    start_queue_services(),
    register(listen_char, self()),

    {_MY,MX} = cecho:getmaxyx(),
    Top_border = string:copies( "-", MX-40),
    % cecho:mvaddstr( 0,1, ["  file    | Settings | Create something."] ),
    cecho:mvaddstr( 0,40, [ Top_border ] ),
    cecho:move(0, MX-1), cecho:addch(32),
    cecho:move(1,2),  % (1,1) actual
    cecho:refresh(),
    start_worker_services(),
    set_up_default_values(Filename),
    set_color(none),
    listen_char().  

    % Create_Line = spawn(?MODULE, create_line, []),
    % register(create_line, Create_Line),
    % Border_Patrol = spawn(?MODULE, border_patrol, [1]),

set_up_default_values(Filename) ->
    {ok, Current_Dir} = file:get_cwd(),
    ets:insert(db1, {path, Current_Dir ++ "/"} ),
    % -----
    ets:insert(db1, {filename, Filename} ),
    % load_file(), 
    ok.

start_queue_services() ->    
    % ----- menu_event_queue is separate from the other queues 

    Ignore_Data_Queue = spawn(?MODULE, ignore_data_queue, []),
    register(ignore_data_queue, Ignore_Data_Queue),

    Menu_Event_Queue = spawn(?MODULE, menu_event_queue, [ignore_data_queue]),
    register(menu_event_queue, Menu_Event_Queue),
    
    % main queue data path: 
    % 
    % ----- char_q -> char_q_for_tether -> build_event -> event_q
    
    Event_Queue = spawn(?MODULE, event_queue, [[{loc_store},{loc_restore}]]),
    register(event_queue, Event_Queue),

    Build_Event = spawn(?MODULE, build_event, []),
    register(build_event, Build_Event),
 
    Char_Queue = spawn(?MODULE, char_queue, []),
    register(char_queue, Char_Queue),

    Char_Queue_For_Tether = spawn(?MODULE, char_queue_for_tether, []),
    register(char_queue_for_tether, Char_Queue_For_Tether),
    ok. 

start_worker_services() ->
    spawn(?MODULE, track_cursor,         []),         % automated screen proc - need to be started last  

    spawn(?MODULE, track_screen_size, []),  % automated screen processes
    spawn(?MODULE, track_buffer,         []),         % automated screen processes
    spawn(?MODULE, track_file,           []),          
    Update_Screen = spawn(?MODULE, update_screen, []),    % automated screen processes
    register(update_screen, Update_Screen),
    spawn(?MODULE, update_border,        []),         % automated screen processes
    spawn(?MODULE, update_state,         []),         % display keyboard state
    spawn(?MODULE, update_node,          []),         % display keyboard state  
    spawn(?MODULE, minibuffer_reader,    []),   
    spawn(?MODULE, track_path,           []), 

    spawn(?MODULE, clean_up_screen,      []),     % clear_screen_bottom() 
    spawn(?MODULE, update_color,         []),  

    spawn(?MODULE, update_menu_bar,      []), 
    timer:sleep(2000),                            % sleep 1 sec 
    % spawn(?MODULE, store_nodes,          []),     % look for nodes and store them in ets  
    ok.

% causes cursor to leave the minibuffer somehow. 
store_nodes() ->
    timer:sleep(1000),                                      % sleep 1 sec first
    Found_Nodes       = lists:sort(find_nodes()),           % find other nodes and     
    ets:insert(db1,   [ {found_nodes, Found_Nodes}]),       %    store them in database   
    Length = length(Found_Nodes),
    case Length of 
        0 -> ok;
        _Other ->
            Event_Share_Node  = hd(lists:reverse(Found_Nodes)),     % get last node in list
            ets:insert(db1, {event_share_node, Event_Share_Node}),  % save last node in ets            
            store_nodes()
    end. 

build_node_name() ->
            % ---------- build node name -----------
            % Digits    = "0123456789",
            Upper       =  "BCDEFGHIJKLMNOPQRSTUVWXYZ",   % exclude A, b/c A is default value 
            Lower       = "abcdefghijklmnopqrstuvwxyz",
            All_Chars   = Upper ++ Lower,
            Range       = length(All_Chars),     
            Index       = ceil(rand:uniform() * Range),
            Rand_Char   = lists:nth(Index, All_Chars),

            Default_Char         = $A,
            Default_Char_Name    = list_to_atom("book"++[Default_Char]++"@127.0.0.1"),

            Long_Char_Name       = list_to_atom("book"++[Rand_Char]++"@127.0.0.1"),

            net_kernel:start([Long_Char_Name, longnames]),
            erlang:set_cookie(node(), cookie),

            Nodes          = lists:sort( find_nodes()),
            Length         = length(Nodes),
             
            % If this program is the first instance of the editor on this computer then 
            %   is the base case is bookA@127.0.0.1 
            %   otherwise create random node name for new editor instance 
            case Length of
                0      -> Long_Name = Default_Char_Name;       %  base case 
                _other -> Long_Name = list_to_atom("book"++[Rand_Char]++"@127.0.0.1")
            end,

            % assign Node_Name to self
            net_kernel:stop(),   % needed to change the long name 
            net_kernel:start([Long_Name, longnames]),
            erlang:set_cookie(node(), cookie),
            ok. 

% -------------------------

init_database() ->
    % create small database
    ets:new(db1,    [set, named_table, public] ),

    {MY, MX} = cecho:getmaxyx(),
    ets:insert(db1, [{screen_size, {MY, MX}}]),
    ets:insert(db1, [{line, ""} ]),                 % not used anymore
  
    ets:insert(db1, [{buf_label,   "buf:"} ]),
    ets:insert(db1, [{buffer,   "default"} ]),      % buffer name

    ets:insert(db1, [{file_label, "file:"} ]),
    ets:insert(db1, [{path,       "empty"} ]),
    ets:insert(db1, [{filename, "default.txt"} ]),
   
    ets:insert(db1, [{export_events, false} ]),     % export events for debug
    ets:insert(db1, [{exclude_events, [req_yx]}]), 
    ets:insert(db1, [{bump, 1}]),                   % line number for export events

    ets:insert(db1, [{output, "" } ]),              % generic working veriable
    ets:insert(db1, [{it, nul}]),                   % generic working variable/pronoun

    % auto file save

    ets:insert(db1, [{{auto_file_save, value}, true}]),
    ets:insert(db1, [{{auto_file_save, values}, {true, false}}]), 

    % program state variables  

    ets:insert(db1, [{scroll, 0}]),                 % scrool of buffer
    ets:insert(db1, [{nice, 100}]),                 % set the nice level of background processes  

    % menu setings  

    ets:insert(db1, {{color,value}, none}),         % preset for color menu

    % ets:insert(db1, [{refresh_speed, 3000}]),
    ets:insert(db1, [{{refresh, value}, 3000}]),
    ets:insert(db1, [{refresh_line,  1}]),

    % menu data 
    ets:insert(db1, [{{refresh, choice}, 1000 }]),
    ets:insert(db1, [{{refresh, choices}, 
        ["3000 ms",
         "2000 ms",
         "1000 ms",
          "500 ms"]} ]),
    ets:insert(db1, [{{refresh, useful_choices},
        % item 1 is default value 
        % item 2 is first menu choice   
        [3000,                
         3000,
         2000,
         1000,
          500,
        60000,
        600000 ]} ]),

    ets:insert(db1, [{{app, choice}, "about" }]),
    ets:insert(db1, [{{app, choices}, 
        ["about",
         "help",
         "settings",
         "quit"
        ]} ]),

        ets:insert(db1, [{{app, useful_choices},
        % item 1 is default value 
        % item 2 is first menu choice   
        ["default",
         "about",                
         "help",
         "settings",
         "quit"
        ]} ]),

        ets:insert(db1, [{{colors, useful_choices},
        % item 1 is default value 
        % item 2 is first menu choice   
        ["about",                
         "help",
         "settings",
         "quit"
        ]} ]),

        ets:insert(db1, [{{settings, useful_choices},
        % item 1 is default value 
        % item 2 is first menu choice   
        ["about",                
         "help",
         "settings",
         "quit"
        ]} ]),

        ets:insert(db1, [{{color, useful_choices},
        % item 1 is default value 
        % item 2 is first menu choice   
        [none,                
         none,
         red,
         cyan,
         green
        ]} ]),

    % experiment with plugin 
    ets:insert(db1, [ {refresh_screen, {{values, {true, false}}, {value, true}} } ]),
 
    % key bindings stored in ets, in a {key, function} pair, note: experiment does work 

    % ets:insert(db1, [{{bind, 65}, [{ show, 42 }]  }]),         % A 

    % ets:insert(db1, [{{bind, 25}, {send, {yank_kill_buf}    }}]),   % control y
    % ets:insert(db1, [{{bind,  8}, {send, {help_info}        }}]),   % control h
    % ets:insert(db1, [{{bind,  7}, {send, [{kill_line}]      }}]),   % control g, kill line
    % ets:insert(db1, [{{bind,  5}, {send, {execute_line}     }}]),   % control e
    
    % ets:insert(db1, [{{bind,  12}, {send, {toggle_refresh_screen}}}]),  % constrol l 

    ets:insert(db1, [{{bind, 10}, {send,          [{enter}] }}]),   % enter key, default value  
    ets:insert(db1, [{{bind, default, 10}, {send, [{enter}] }}]),   % enter key, default value 

    % enter key, default value:   used by timeout_for_key_binding 
    binding:start(), 

    % ----------
    %   helper gives help with syntax  
    ets:insert(db1, [{{helper, 1}, {"ets:lookup", " arg1 = DB, returns: [{Input, Output}] "}} ]),
    % ----------

    % create lines of text with spaces  
    setup_lines(MY*2),     

    % setup menus
    setup_menus(),                        

    % eval line of erlang in editor at cursor 
    eval_str_init("ok."),                            % create first environment val
    ets:insert(db1, [ {eval_result, ""}]),           % default eval_result

    % bounding box / not used yet 
    ets:insert(db1, [{bbox,    {1,1,1,1} } ]),      % default bounding box

    % default minibuffer value
    Spaces = string:copies( " ", MX-3),
    ets:insert(db1, {minibuffer,        Spaces}), 
    ets:insert(db1, {minibuffer_target, default}),  % default value
    ets:insert(db1, {default,          "default"}),  % default value

    % more program state variables

    ets:insert(db1, {state, none}),                     % state if control-x or other key-combo is active 
    ets:insert(db1, [ {tether, false}]),                % if connected to another editor
    Found_Nodes = lists:sort(find_nodes()),             % find other nodes and     
    ets:insert(db1, [ {found_nodes, Found_Nodes}]),     %    store them in database 
    ets:insert(db1, [ {event_share_node, undefined}]),    % event share tartget node 

    ets:insert(db1, [ {{unit, 3}, {0}} ]), 

    % save screen size so we can update if screen has changed size
    
    ets:insert(db1, {screen_size, {MY, MX}}),

    % set mark    
    ets:insert(db1, {mark_loc, {1, 1}}),  

    % set color on a character 
    ets:insert(db1, {{char_color, {5, 5}}, red   } ),
    ets:insert(db1, {{char_color, {5, 6}}, green } ),
    ets:insert(db1, {{char_color, {5, 7}}, cyan  } ),
    setup_colors(),

    % setup kill buffer 
    ets:insert(db1, {kill_buf, ""}),                    % simple kill-line

    ets:insert(db1, {{kill_buf, {line, 1}}, ""}  ),     % multi line kill-buf
    ets:insert(db1, {kill_buf_line_count, 1}),          % multi line kill-buf 
    ok.

% end setup 

setup_colors() ->
    cecho:start_color(),
    cecho:init_pair(1, ?ceCOLOR_RED,     ?ceCOLOR_BLACK),
    cecho:init_pair(2, ?ceCOLOR_GREEN,   ?ceCOLOR_BLACK),
    cecho:init_pair(3, ?ceCOLOR_YELLOW,  ?ceCOLOR_BLACK),
    cecho:init_pair(4, ?ceCOLOR_CYAN,    ?ceCOLOR_BLACK),
    cecho:init_pair(5, ?ceCOLOR_MAGENTA, ?ceCOLOR_BLACK),
    cecho:init_pair(4, ?ceCOLOR_CYAN,    ?ceCOLOR_BLACK),
    cecho:init_pair(7, ?ceCOLOR_WHITE,   ?ceCOLOR_BLACK),
    cecho:refresh().

set_color(none) ->
    cecho:start_color(),
    cecho:init_pair(7, ?ceCOLOR_WHITE, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(7)),
    % cecho:move(2,1),
    % cecho:addstr("Colored!"),
    cecho:refresh(),
    % cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok;

set_color(red) ->
    cecho:start_color(),
    cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    % cecho:move(2,1),
    % cecho:addstr("Colored!"),
    cecho:refresh(),
    % cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok;

set_color(green) ->
    cecho:start_color(),
    cecho:init_pair(2, ?ceCOLOR_GREEN, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(2)),
    % cecho:move(2,1),
    % cecho:addstr("Colored!"),
     cecho:refresh(),
    % cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok;

set_color(cyan) ->
    cecho:start_color(),
    cecho:init_pair(4, ?ceCOLOR_CYAN, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(4)),
    % cecho:move(2,1),
    % cecho:addstr("o"),
     cecho:refresh(),
    % cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok;

set_color(white) ->
    cecho:start_color(),
    cecho:init_pair(7, ?ceCOLOR_WHITE, ?ceCOLOR_BLACK),
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(7)),
    % cecho:move(2,1),
    % cecho:addstr("o"),
     cecho:refresh(),
    % cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1)),
    ok.

update_color() ->
    % [{{color, value}, Color}] = ets:lookup(db1, {color, value}),
    % set_color(Color),
    timer:sleep(1000),
    update_color().
    
% -----------------------------------------------------------------------
% menu event queue collects menu events 
%   the menu controler requests the menu events 
%   if nothing happens in 3 seconds then terminate
% 
%   menu_event_queue, timeout_for_key_binding, set_temporary_key_binding
%   all work together.  
%   The enter key only sellects menu items when a menu is showing. 
% 
%   When a menu does a refresh it calls
%      temporary_key_binding() calls
%      set_timeout_for_key_binding()  
 

% enter key is ascii #10 
% temporary_key_binding(Key, Value) ->

temporary_key_binding(Key) ->
    % ets:insert(db1, {{bind, Key}, Value} ),   
    ets:insert(db1, {{bind, 10}, {send_to, ignore_data_queue, [{enter}]}  } ), 
    spawn(?MODULE, set_timeout_for_key_binding, [Key, _Timeout=10000] ),  
    % set_timeout_for_key_binding(Key, _Timeout = 3000),
    ok.

    % timeout function for temporary key-binding, revert to default key binding after 3 sec 

set_timeout_for_key_binding(Key, Timeout) ->
    receive
        % _ -> ignore
    after Timeout ->
        [{_, Value}] = ets:lookup(db1, {bind, default, Key} ),
        ets:insert(db1, {{bind, Key}, Value} )
    end,
    ok.

% send info here we want to forget
ignore_data_queue() ->
    receive
        _Any -> ok
    end,
    ignore_data_queue().

menu_event_queue(Target) ->
    % a) The active menu handler sets the address of the menu_mng, the menu manager 
    % b) if a menu_event happens it is sent to the active menu handler
    % c) if the address has not been sent then the default target is ignore_data    
    receive 
        {menu_event, Event}           ->  ignore_data_queue ! Event;  % temp address for testing, normally Target = Target
        {set_menu_mng_address, SASE}  ->  menu_event_queue(SASE)                 
    end,
    menu_event_queue(Target).

% ------------------------------------------------------
% find a unit of text

find_unit() ->
    N        = 1,
    [{{line, N}, Line}] = ets:lookup(db1, {line, N}),
    Y        = N,
    X        = string:str(Line, "3"),
    Len      = length("2"),
    _Time    = {{_,_,_}, {Hours, Mins, Secs}} = erlang:localtime(),
    ID_su    = [{{unit, 3}, {Y, X}, {Len, {Hours, Mins, Secs}}}],  
    ets:insert(db1, ID_su),
    event_queue ! {show, 48+X}.  

setup_menus() ->
        {_MY, MX} = cecho:getmaxyx(),
        AMenu =  "| App       | File      | Settings  | Refresh   | Color     | Test      |",

        %         |-----6-----|-----6-----|-----6-----|-----6-----|-----6-----|-----6-----|  
        Menu   = "|-----=-----|-----=-----|-----=-----|-----=-----|-----=-----|-----=-----|",  
         
        Spaces = string:copies( " ", MX-3),
        ets:insert(db1, [{{line, 0}, Menu ++ Spaces} ]),
        done.

setup_lines(0) -> ok;
setup_lines(N) ->
    {_MY, MX} = cecho:getmaxyx(),
    Spaces = string:copies( " ", MX-3),
    ets:insert(db1, [{{line, N}, Spaces} ]),
     ets:insert(db1, [{{char, N, 1}, "/nul"}]),   % space in char 1 of unicode layer
    setup_unicode_line(N, 80), 
    setup_lines(N-1).

setup_unicode_line(N, 1) -> ok;
setup_unicode_line(N, X) ->
    ets:insert(db1, [{{char, N, 1}, "/nul"}]), 
    setup_unicode_line(N, X-1).

%create_line() ->
 %       receive
  %          {create_line_after, Line_Num} -> Line_Num
   %     end,
    %    create_line().

% update position of cursor in screen  
% not used

track_screen_size() ->
    {Y, _X} = cecho:getyx(),
    {MY, _MX} = cecho:getmaxyx(),
    %  is cursor is out of window then move cursor up
    if
        Y > (MY-4) ->
            event_queue ! {delta, -1, 0};
        true -> ok
    end,
    timer:sleep(5000),
    ok.
    %track_screen_size().

% Clean up the bottom of the screen is the terminal is resized. 

clean_up_screen() ->
    {MY, MX} = cecho:getmaxyx(), 
    [{_, Old_Size}] = ets:lookup(db1, screen_size),
    ets:insert(db1, {screen_size, {MY,MX}}),
    {Old_MY, Old_MX} = Old_Size, 
    DY = MY-Old_MY,
    DX = MX-Old_MX,
    DSize = DY + DX,
    Row = 2,
    if
        DSize =/= 0 -> 
            Spaces = string:copies(" ", MX-3),
            event_queue ! [
                { loc_store},                      % save cursor position
                { mvaddstr, MY-Row, 0, Spaces },   % erase area 
                { mvaddstr, MY-1,   0, Spaces },   % erase area 
                { loc_restore}                     % restore cursor position
            ];
        true -> do_nothing
    end,

    timer:sleep( 3 * 1000),   % 3 seconds   
    clean_up_screen().

% update position of cursor in border bar
% note: changing the screen size breaks input hmmmm...

track_cursor() ->
    {Y, X} = cecho:getyx(),
    {MY, _MX} = cecho:getmaxyx(),
    [{scroll, Scroll}] = ets:lookup(db1, scroll),
    Row = 3,
    CY = Y + Scroll, % max(Y, 1),  % one is minimum value for row and col
    CX = X, % max(X, 1),
    % if we resize the window then we need to keep the cursor inside the window also

    % {{_,_,_},{_,_,Seconds1}} = erlang:localtime(),
    event_queue ! [
        { loc_store},                            % save cursor position
        { mvaddstr, MY-Row, 5, "          " },   % erase space to start
        { {mv, MY-Row, 5}, $r},                  % r for row
         
        { mvaddstr, MY-Row, 6, integer_to_list(CY) },
        { {mv, MY-Row, 9},  $*},          %  star in the middle    
        { {mv, MY-Row, 11}, $c},         % c for colunmn    
        { mvaddstr, MY-Row, 12, integer_to_list(CX) },
        % { {mv, MY-Row, 17}, 32},  
        % { {mv, MY-Row, 4}, 32 },
        { loc_restore}                % restore cursor position
    ],
    timer:sleep(150),      
    track_cursor().

track_buffer() ->
    {MY, _MX} = cecho:getmaxyx(),
    Row = 3,
    [{buffer, Buffer}] = ets:lookup(db1, buffer),
    Len    = length(Buffer),
    Pad    = 9 - Len,
    Spaces = string:copies(" ", Pad),
    Longer_Buffer = Buffer ++ Spaces,
    event_queue ! [
        { loc_store},                          % save cursor position
        { mvaddstr, MY-Row, 50, "buffer:  " },
        { mvaddstr, MY-Row, 58, Longer_Buffer },  
        { loc_restore}                         % restore cursor position
    ],
    timer:sleep(1000),      
    track_buffer().

% minibuffer_reader is a utility process
%   reads the minibuffer
%   stores it into the mini_buffer target  

minibuffer_reader() ->
    [{_,Target}    ] = ets:lookup(db1, minibuffer_target),
    [{_,Minibuffer}] = ets:lookup(db1, minibuffer),
    ets:insert(db1,{ Target, Minibuffer }),
    timer:sleep(1000),
    minibuffer_reader().

% update file-name on screen 

track_file() ->
        {MY, _MX} = cecho:getmaxyx(),
        Row = 3,
        [{_, Filename}] = ets:lookup(db1, filename),
        % Len    = length(Mini_Buffer),
        % Pad    = 9 - Len,
        Spaces = string:copies(" ", 5),
        Filename_Plus = string:trim( Filename) ++ Spaces,
        event_queue ! [
            { loc_store},                          % save cursor position
            { mvaddstr, MY-Row, 24, "file: " },
            { mvaddstr, MY-Row, 30, Filename_Plus },  
            { loc_restore}                         % restore cursor position
        ],
        timer:sleep(1000),      
        track_file().

% update file path on screen 

track_path() ->
        {MY, _MX} = cecho:getmaxyx(),
        Row = 1,
        [{_, Path}] = ets:lookup(db1, path),
        % Len    = length(Mini_Buffer),
        % Pad    = 9 - Len,
        Spaces = string:copies(" ", 5),
        Path_Plus = string:trim( Path) ++ Spaces,
        event_queue ! [
            { loc_store},                          % save cursor position
            { mvaddstr, MY-Row, 24, "path: " },
            { mvaddstr, MY-Row, 30, Path_Plus },  
            { loc_restore}                         % restore cursor position
        ],
        timer:sleep(1000),      
        track_path().

% update screen border  

update_border() ->
        {MY, MX} = cecho:getmaxyx(),
        % Start  = 40,
        Row      = 4,
        % End    = MX-3,
        % Length = End - Start,
        % Bar    = string:copies("=", 15),
        Long_Bar = string:copies("=", MX-1),
        event_queue ! [
            { loc_store},                          % save cursor position
            { mvaddstr, MY-Row, 1, Long_Bar },
            %{ mvaddstr, MY-1, 52, Long_Bar },
            %{ {mv, MY-Row, Start}, 32 },
            %{ {mv, MY-Row, End+1}, 32 },
            %{ {mv, MY-Row, 10}, 32 },
            %{ {mv, MY-Row, MX-1}, 32 },
            { loc_restore}                % restore cursor position
        ],
        timer:sleep(1000),      
        update_border().

% state of editor could be control-x is first half of cntr-x cntr-c combo
%   or maybe Cap-lock

update_state() ->
        {MY, _MX}        = cecho:getmaxyx(),
        [{state, State}] = ets:lookup(db1, state),
        State_String     = "state:" ++ atom_to_list(State) ++ " ",
        event_queue ! [
            { loc_store},  
            { mvaddstr, MY-1, 11, ["            "] },                      
            { mvaddstr, MY-1, 11, [State_String] },
            { loc_restore}                  
        ],
        timer:sleep(500),      
        update_state().

% find node name of this editor and display node name at bottom of the screen

update_node() ->
            {MY, MX}       = cecho:getmaxyx(),
            Node            = node(),
            Node_List       = atom_to_list(Node),
            [H|_T]           = string:split(Node_List, "@"),
            Row = 3,
            Node_String     = "node:" ++ H ++ " ",
            % Node_String     = "node:" ++ atom_to_list()
            % string:str
            Len      = 70 + length(Node_String),
            Safe_Len = min(MX, Len), 
            string:left(Node_String, Safe_Len),
            if 
                Safe_Len < 70 -> 
                    Good_Node_String  = "",
                    Good_Space_String  = "";
                true -> 
                    Good_Node_Len     = Safe_Len - 70,
                    Good_Node_String  = string:left(Node_String, Good_Node_Len),
                    Good_Space_String = string:copies(" ", Good_Node_Len)
            end,
            event_queue ! [
                { loc_store},  
                { mvaddstr, MY-Row, 70, [Good_Space_String] },                      
                { mvaddstr, MY-Row, 70, [Good_Node_String] },
                { loc_restore}                  
            ],
            timer:sleep(500),      
            update_node().

helper() ->
        receive
            Any   -> event_queue ! Any
        after 1000 -> ok
        end,
        helper().

simple_update_line(Line_Num) ->
            {_MY,  MX} = cecho:getmaxyx(),
            {_CY, _CX} = cecho:getyx(),
            [{scroll, Scroll}]               = ets:lookup(db1, scroll),

            % calculate: sline = scroll + line 
            SLine_Num = Line_Num + Scroll,
            [{{line, SLine_Num}, Line}]       = ets:lookup(db1, {line, SLine_Num}),
        
            % trim line to fit screen
            Show_Line   = string:sub_string(Line, 1, MX-3), 
            Line_Len    = length(Line),

            % if line is shorter than screen width the pad with spaces
            Limit = MX-3,
            if  Line_Len < Limit ->
                    Extra = string:copies(" ", MX-3-Line_Len);
                true ->
                    Extra = ""
            end,
            draw_a_line(Show_Line ++ Extra, Line_Num, 1),
            ok.

% update menu bar

update_menu_bar() ->
        {_MY,  MX} = cecho:getmaxyx(),
        % memu bar is on line 0, is not changed by scrolling
        Line_Num = 0, 
        [{{line, _}, Line_Data}] = ets:lookup(db1, {line, Line_Num}),
        Line_Data_To_Fit         = string:left(Line_Data, MX-3),
        draw_a_line(Line_Data_To_Fit, Line_Num, 1),
        timer:sleep(3000),
        update_menu_bar().

char_color_line(5) ->
    {MY,  MX} = cecho:getmaxyx(),
    % {CY, _CX} = cecho:getyx(),
    % ets:insert(db1, [{update_line_no, Line_Num}]),
    [{scroll, Scroll}]                  = ets:lookup(db1, scroll),
    % [{{refresh, value}, Refresh_Value}] = ets:lookup(db1, {refresh, value}),
        % calculate: sline = scroll + line 
    Line_Num  = 5, 
    SLine_Num = Line_Num + Scroll,
    Line_Exists = ets:member(db1, {line, SLine_Num}),
    if
        Line_Exists -> draw_char_line(SLine_Num, Line_Num);
        true -> ok
    end,
    update_screen(6).

draw_char_line(Sline_Num, Line_Num) ->
    [{_, Text_Line}]  = ets:lookup(db1, {line, Sline_Num}),
    [{_, Char_Color}] = ets:lookup(db1, {char_color, {5,5}} ),
    if 
        Char_Color == red -> 
            % cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(2)); 
            % cecho:refresh(); 
            cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(2) ),
            % set_color(red),
            cecho:refresh();
        true -> ok
    end,
    event_queue ! [{ loc_store}, {{ mv, 5, 5}, 42 }, {refresh}, { loc_restore}],
    % event_queue ! [{{ mv, 5, 5}, 42}],
    Limit = 7,   % length(SLine)
    paint_chars(5, Limit), 
    ok.

% Paint colored chars on a line 

paint_chars(Limit, Limit) -> ok;
paint_chars(N, Limit) ->
    % set_color(cyan),
    % cecho:attron(?ceCLOR_CYAN),
    event_queue ! [{ loc_store}, {{ mv, 5, N}, 42 }, {refresh}, { loc_restore}],
    cecho:attron(?ceA_BOLD bor ?ceCOLOR_PAIR(4)),   
    cecho:refresh(),
    paint_chars(N+1, Limit).

% update screen one line at a time
%   update screen is nice about sharing system resources 

update_screen() -> update_screen(0).

% Note: line zero is the menu bar is handled by update_menu_bar 
% update_screen(0) ->  update_screen(1);
% update_screen(5) ->  char_color_line(5);
update_screen(0) -> 
    {MY, _MX} = cecho:getmaxyx(),
    update_screen( MY-5);

update_screen(Line_Num) ->
        {MY,  MX} = cecho:getmaxyx(),
        {CY, _CX} = cecho:getyx(),
        ets:insert(db1, [{update_line_no, Line_Num}]),
        [{scroll, Scroll}]                  = ets:lookup(db1, scroll),
        [{{refresh, value}, Refresh_Value}] = ets:lookup(db1, {refresh, value}),
        % calc: line + scroll
        SLine_Num = Line_Num + Scroll,
        Line_Exists = ets:member(db1, {line, SLine_Num}),
        if 
            not Line_Exists -> 
                line_flag(Line_Num),  
                % You have gone past end of file, so backup scroll 
                % is a wierd bad idea:   ets:insert(db1, {scroll, Scroll-1}),
                % Next_Line_Num = (Line_Num + 1) rem (MY-4), 
                % update_screen(Next_Line_Num);
                Line = "",
                Line_Color = blue;
            true -> 
                [{{line, SLine_Num}, Line}]       = ets:lookup(db1, {line, SLine_Num}),
                Color_Flag = ets:member(db1, {line_color, SLine_Num}),
                if
                    Color_Flag == true -> 
                        [{{_, _}, Line_Color}]    = ets:lookup(db1, {line_color, SLine_Num});
                        % set_color(Line_Color); 
                    true -> Line_Color = none 
                end
        end,
        
        % ---------- helper section
        %   shows syntax help info for particlar paterns 

        [{_Input, {Pattern, Help_Text }}] = ets:lookup(db1, {helper, 1}),
        Hit_Loc = string:str(Line, Pattern),
        if
            Hit_Loc == 0    -> ok;
            SLine_Num == CY -> event_queue ! [{help, Pattern ++ " " ++ Help_Text}];
            true -> ok
        end,
        % ---------- end helper  

        Show_Line   = string:sub_string(Line, 1, MX-3), % trim line to fit screen
        Line_Len    = length(Line),
        % if line is longer than screen with percent mark
        if
            Line_Len == 0 ->
                Long_Line_Flag = $0;
            Line_Len > MX ->
                Long_Line_Flag = $%;
            true ->
                Long_Line_Flag = $.
        end,
        % if line is shorter than screen width the pad with spaces, called crystalize 
        Limit = MX-3,
        if  
            Line_Len < Limit ->
                Extra = string:copies(" ", MX-3-Line_Len);
            true ->
                Extra = ""
        end,
        draw_a_line_w_color(Show_Line ++ Extra, Line_Num, 1, Line_Color),
        event_queue ! [
            { label, update},                      % label can be filtered out of event watching 
                                                    % b/c sceen updates would swamp event watching 
            { loc_store},                          % save cursor position
            % { mvaddstr, Line_Num, 1, Show_Line },
            % { {mv, Line_Num, 0}, 32 },
            { {mv, Line_Num,   MX-2}, $. },     % show refresh dot on current line
            { {mv, Line_Num-1, MX-2}, 32 },     % hide refresh dot
            { {mv, Line_Num,   MX-1}, Long_Line_Flag },
            { loc_restore}                         % restore cursor position
        ],
        % set_color(none),
        %  
        % unicode is stored in {{char, Line_Num, Col_num}, char_string} format 
        Find_it = ets:match(db1, {{char, Line_Num, '$1'}, '$2'} ),
        Measure = length(Find_it),
        if 
            Measure > 0 -> 
                do_each_unicode(Find_it, Line_Num ); 
                %% Search_for_unicode = ets:match(db1, {{char, Line_Num, '$1'}, '$2'} ),
                % Search_for_unicode = Find_it,
                %[X, Uni_char] = hd(Search_for_unicode),
                %if
                %    Uni_char == "~nul" -> ok;
                %    true ->
                %        event_queue ! 
                %        [   { loc_store}, 
                %             { delta, -20, -20},
                %             {{mv, Line_Num, X}, 32}, 
                %                { refresh },
                %                { addstr, Uni_char}, 
                %                { loc_restore} ]
                % end;
            true -> ok
        end,
        timer:sleep(round(Refresh_Value/MY)),      % was 3000 total
        NLine_Num = (Line_Num - 1) rem (MY-4),  
        update_screen(NLine_Num).
%  
%       ets:match(db1, {{char, 6, '$1'}, '$2'}).       ρ                          
%   [[42,"~rho"],[47,"~rho"]] 
%\


do_each_unicode(      [], Line_Num ) -> ok; 
do_each_unicode( Find_it, Line_Num ) -> 
                % Search_for_unicode = ets:match(db1, {{char, Line_Num, '$1'}, '$2'} ),
                Search_for_unicode = Find_it,
                [X, Uni_char] = hd(Search_for_unicode),
                if
                    Uni_char == "/nul" -> ok;    % ignore /nul chars 
                    true ->                      % otherwide do normal event transaction 
                        event_queue ! 
                        [   { loc_store}, 
                             { delta, -30, -30},        % janky crap to help unicode print 
                             {{mv, Line_Num, X}, 32},   % goto row and col
                                { refresh },            
                                { addstr, Uni_char}, 
                                { loc_restore} ]
                end,
                do_each_unicode( tl(Find_it), Line_Num ).

line_flag(Line_Num) ->
        {_MY, MX} = cecho:getmaxyx(),
        event_queue ! [
            { label, update},                      % label event can be filtered out of event watching
                                                   %     b/c line refresh is way to busy 
            { loc_store},                          % save cursor position
            { {mv, Line_Num,   MX-1}, _Long_Line_Flag=$0 },
            { loc_restore}                         % restore cursor position
        ],
    ok.

crystalize(Line) ->
    % if line is shorter than screen width the pad with spaces, called crystalize 
    {_MY, MX}  = cecho:getmaxyx(),
    Line_Len   = length(Line),
    Limit = MX-3,
    if  
        Line_Len < Limit ->
            Extra = string:copies(" ", MX-3-Line_Len);
        true ->
            Extra = ""
    end,
    Line ++ Extra.

% draw line of text used by update screen 

draw_a_line( Line, Y, _) ->
    event_queue ! [{ loc_store}, { mvaddstr, Y, 1, [Line] }, {refresh}, { loc_restore}],
    ok.

draw_a_line_w_color( Line, Y, _X, Color) ->
    event_queue ! [{ loc_store}, { mvaddstr_w_color, Y, 1, [Line], Color }, {refresh}, { loc_restore}],
    ok.

% tether: connect editors in different nodes/windows, and send copy of key-presses

tether() ->
    Found_Nodes = find_nodes(),
    ets:insert(db1, [{found_nodes, Found_Nodes}]),
    ets:insert(db1, [{tether, true}]),
    ok.

% part of tether()

find_nodes() ->
    % ---------- find other local NaCl editors and say hello -----------
    Digits = "0123456789",
    Upper  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Lower  = "abcdefghijklmnopqrstuvwxyz",
    All    = Digits ++ Upper ++ Lower,
    % Range  = lists:seq(32,126),
    Node_Names = [ list_to_atom("book"++[X]++"@127.0.0.1") || X <- All],    
    Send_Move = fun(Target) ->
                    {event_queue, Target} ! [{label, node_probe},{ delta, 0, 1},{ delta, 0, -1}]
                end,
    lists:map(Send_Move, Node_Names),
    timer:sleep(500),    
    % wait half second for messages to cause updates to nodes()
    Found_Nodes = nodes(),
    send_Hi(target),
    Found_Nodes.

send_Hi(target) ->
    % Send_Hi  =   fun(Target) ->
    %                    {event_queue, Target} ! [{show,32},{show,72},{show,72+33} ]
    %                end,
    % lists:map(Send_Hi, Found_Nodes),
    %% return value is inored 
    target.

% An ant is a small automated piece of text, can crawl around and do things on screen

spawn_ant() ->
    {Y, X} = cecho:getyx(),
    ets:insert(db1,{state,none}),
    spawn(?MODULE, loop_ant, [Y,X-1]),
    ok.

loop_ant(1,1) ->
    event_queue ! [{loc_store}, {{mv, 1, 1}, $2}, {show, $2}, {loc_restore}],
    exit(0);
loop_ant(1,X) when X > 1 ->
    ets:insert(db1, [{loc, 1, X, 32}]),
    event_queue !   [{{loc_store},{mv,   1, X}, 32},{loc_restore}],
    event_queue !   [{{loc_store},{mv, 1, X-1}, 50},{loc_restore}],
    ets:insert(db1, [{loc, 1, X-1, 50}]),  
    timer:sleep(500),
    loop_ant(1,X-1);
loop_ant(Y,X) when Y > 1 ->
    % - does not check to see if space is empty
    % - does not store in database
    event_queue ! [{{loc_store},{mv,   Y, X}, 32},{loc_restore}],
    event_queue ! [{{loc_store},{mv, Y-1, X}, 50},{loc_restore}],
    timer:sleep(500),
    loop_ant(Y-1, X).

%%
%% code for the queues: listen_char, char_queue_for_tether, char_queue, event_queue
%%

listen_char() ->  
    Key = cecho:getch(),
    char_queue_for_tether ! Key,
    listen_char().

% note: char 410 may be a signal for console size change

% char_queue_for_tether is in the stack of queues to process key-presses

char_queue_for_tether() ->    
    receive
        _Alt = 27                    -> char_queue ! {alt, $k};
        Char when Char < 400         -> char_queue ! Char
        after 1000 -> ok
    end,
    char_queue_for_tether().

% char_queue is in the stack of queues to process key-presses

char_queue() ->    
    receive
        _Alt = 27                    -> build_event ! {alt, $k};
        Char when Char < 400         -> build_event ! Char
        after 1000 -> ok
    end,
    char_queue().

% Execute merged event transaction when length > 10

% event_queue(An_Evt) when length(An_Evt) > 7 ->
%    execute_event_transaction(An_Evt);

event_queue(An_Evt) ->    
    receive
        { delta, DY, DX }   ->   execute_event_transaction([ { delta, DY, DX } ]);  % mv(DY,DX);
        { Action }          ->   execute_event({Action});
        { Action, Char }    ->   execute_event({Action, Char});
        [{{loc_store},{mv, Y, X}, C},{loc_restore}] ->  
                            execute_event_transaction([
                                {loc_store},  
                                {{mv, Y, X}, C}, {loc_restore},
                                {refresh}, {loc_restore} ]);

        % -- merge event transactions experiment failed

        %[{loc_store},{ mvaddstr, R, C, D}|Tail]  ->
        %                        event_queue(
        %                            merge(
        %                                An_Evt,
        %                                [{loc_store},{ mvaddstr, R, C, D}|Tail]
        %                            )
        %                        );
       
        [H|T]               ->  execute_event_transaction([H|T])
                       
    after 400 -> ok
    end,
    event_queue(An_Evt).

% merge: two events is first cmd is loc_store, removing middle {loc_restore} and {loc_store}
%   note: we should only merge event transactions that have absolute cursor locations

%merge(Event_1, Event_2) ->
%    Chop_Event_1    = remove_last_element(Event_1),
%    [_|T2]          = Event_2,
%    Merge           = Chop_Event_1 ++ T2,   % [{refresh}] ++
%    Merge.

%remove_last_element(List) ->
%    [_|T] = lists:reverse(List),
%    Out   = lists:reverse(T),
%    Out.


% bump() cycles the bump count from
%   1 to the number of lines on screen
%   so we can send each event to an individual line on the
%   export screen, where we can watch the events
%   of another instance of the editor

bump() ->
    [{bump, Bump_Count}] = ets:lookup(db1, bump),
    {MY, _MX}            = cecho:getmaxyx(),
    Out_Bump_Count = (Bump_Count + 1) rem (MY-5),
    if
        Out_Bump_Count == 0 ->  
            Next_Bump_Count = 1;
        true ->
            Next_Bump_Count = Out_Bump_Count
    end,
    ets:insert(db1, [{bump, Next_Bump_Count}]),
    Next_Bump_Count.

% ----------  ----------
% Notes on events
% ----------  ----------
%   * Events
%   ** all events are tuples
%
%   * Event Transactions
%   ** all event transactions are a list of event tuples
%   ** all event transactions normally begins with { loc_store }
%   ** all event transactions normally ends   with { loc_restore }
%   ** event transactions are atomic operations
%  
execute_event_transaction([]) -> ok;
execute_event_transaction(T) when is_tuple(T) -> execute_event(T);
execute_event_transaction(T) when length(T) > 0 ->
    First_Tuple = hd(T),
    First_Event = tuple_to_list(First_Tuple),
    First_Item  = hd(First_Event),
    %  
    % send copy of events to export/peer screen to watch events as strings
    %   to help with debug 
    %
    [{export_events, Export_Flag}]    = ets:lookup(db1, export_events),
    [{exclude_events,Exclude_Events}] = ets:lookup(db1, exclude_events),
    if
        First_Item == label     -> then_ignore_transaction;    % ignore lable event is busy background events
        First_Item == loc_store -> then_ignore_transaction;    % ignore loc_store event 
        First_Item == req_yx    -> then_ignore_transaction;    % ignore req_yx event 
        true ->
            if
                Export_Flag -> ok,
                    Bump_Count = bump(),
                    [{found_nodes, Found_Nodes}] = ets:lookup(db1, found_nodes),
                    % [{update_line_no, Line_Num}] = ets:lookup(db1, update_line_no),
                    _Event_Transaction = T,
                    Info = io_lib:format( "~p          ", [ [{export}] ++ T] ),
                    One_Line_Info = string:join(string:split(Info, "\n", all), " "),
                    % hd(Found_Nodes)
                    Event_Share_Target = hd(lists:reverse(Found_Nodes)),   % get the last item
                    {event_queue, Event_Share_Target} !
                        [ {mvaddstr, Bump_Count, 1, One_Line_Info }];
                true -> ok
            end,
            ok
    end,
    execute_event_transaction_parts(T).

execute_event_transaction_parts([]) -> ok;
execute_event_transaction_parts([H|T]) ->
    execute_event(H),
    execute_event_transaction_parts(T).

execute_event(Event) ->
    case Event of
        { ignore }                 -> ok;
        { label, _Label }          -> ok;     % events that happen in background, border updates etc. are not passed to peer screen.
        { debug, _Info }           -> ok;      % dummy event for debug and watching things  
        { help, _Info }            -> ok;       % help info for certain patterns of text
        { clear_state }            -> clear_state();
        { spawn_ant }              -> spawn_ant();
    {temporary_key_binding, Key}   -> temporary_key_binding(Key);
    {set_timeout_for_key_binding}  -> set_timeout_for_key_binding(_Key=10, _Timeout=3000);
    {simple_update_line, Line_Num} -> simple_update_line(Line_Num);
        { space }               -> space_key();
        { backspace }           -> backspace_key();
        { move_left_all }       -> {CY,_CX} = cecho:getyx(), cecho:mvaddch(CY,0,32);
        { get_line }            -> {CY,_CX} = cecho:getyx(),
                                        [{{line, CY},Line}] = ets:lookup( db1, {line, CY}),
                                        ets:insert( db1, {it, Line});
        { req_yx, SASE}          -> request_yx(SASE);
        { req_max_yx, SASE}      -> request_max_yx(SASE);
        { req_line, N, SASE}     -> request_line( N, SASE);
        { req_lookup, Key, SASE} -> request_lookup( Key, SASE);
        { store_line, Line_no, L} -> ets:insert( db1, [{{line, Line_no}, L}]);

        { save_val, Key, Val}    -> ets:insert( db1, [{ Key, Val }] );
        { get_val, Key, SASE}    -> [{Key, Value}] = ets:lookup( db1, key), SASE ! Value;

        % not used
        %{ line_into_db }        -> [{line, Line}] = ets:lookup( db1, line),
        %                                {CY,_CX} = cecho:getyx(),
        %                                ets:insert(db1, {{line,CY}, Line});
        % put line into It  
        { enter }               -> enter();
        { create_line_after, L} -> create_line_after(L);
        { move_cursor_up }      -> move_cursor_up();
        { move_cursor_down }    -> move_cursor_down();
        { help_info }           -> help_info();
        { info_info }           -> info_info();
        { execute_line }        -> execute_line();
        { get, Line_No }        -> [{Line_No, Line}] = ets:lookup( db1, {line, Line_No}),
                                        ets:insert( db1, {it, Line});

        { 'EXIT', _PID, _MSG }  -> ignore;
        { eval_str }            -> eval_string();
            %[{it, It}] = ets:lookup( db1, it),  
             %                           % Out = 
              %                          process_flag(trap_exit, true),
               %                         ets:insert(db1, {eval_result, "crash"}),
                %                        spawn_link(?MODULE, eval_str, [It]), % eval_str puts result in db as eval_result
                 %                                                       %    if it doen't crash. 
                  %                      timer:sleep(500),              % wait for result to be available 
                   %                     [{_,Result}] = ets:lookup(db1, eval_result),
                    %                    ets:insert( db1, {it, Result});           % It was Out 
        { put_into_cmd }        -> [{it, It}] = ets:lookup( db1, it),
                                        cecho:mvaddstr( 20, 20, io_lib:format( "~p", [It] ));
        { show_in_cmd_bar, Col} -> [{it, It}] = ets:lookup( db1, it),
                                        {MY,_MX} = cecho:getmaxyx(),
                                        cecho:mvaddstr(MY-2, Col, io_lib:format( "~p", [It] ));
        { put_into_remark }     -> put_into_remark(); 
                                    %[{it, It}] = ets:lookup( db1, it),
                                     %   {CY,_CX} = cecho:getyx(),              
                                      %  cecho:mvaddstr( CY+1, 1, io_lib:format( "% ~p", [It] )),
                                       % S = io_lib_pretty:print(It),
                                        %Out = lists:flatten(S),
                                        %ets:insert(db1,{{line,CY+1}, "% " ++ Out  }),
                                        %ok;
        % { clear_line }          -> ets:insert( db1, {line, " "});   % should rewrite
        { delta, DY, DX}        -> mv(DY,DX);
        { load_file }           -> load_file();
        { save_file }           -> save_file();
        { loc_store}            -> ets:insert( db1, { loc, cecho:getyx() } );
        { loc_restore}          -> [{loc,{JY,JX}}] = ets:lookup( db1, loc), cecho:move(JY,JX), cecho:refresh();
        {{ mv, Y, X}, C}        -> cecho:mvaddch(Y,X,C);   % does not put into database
        { addstr, Data }        -> cecho:addstr( Data );  
        { mvaddstr, Row, Col, Data } ->
                                   cecho:mvaddstr(Row, Col,  [Data] );
        { mvaddstr_w_color, Row, Col, Data, Color } -> 
                                    set_color(Color),
                                    cecho:mvaddstr(Row, Col,  [Data] );
                                    % cecho:attroff(?ceA_BOLD bor ?ceCOLOR_PAIR(1));
                                    % set_color(none);                          
                                   % cecho:mvaddstr(Row, Col, io_lib:format( "~p", [Data] ));
                                   % cecho:mvaddstr(Row, Col,   Data );
        { refresh}                    -> cecho:refresh();           % redraw screen
        { kill_line}                  -> kill_line();               % cut line to kill buffer
        { find_unit }                 -> find_unit();               % find structure in line
        { insert_char, C, at_loc, CX} -> insert_char_at_loc(C,CX);  % insert char C at_loc CX in It
        { update_line, Y, with_it}    -> update_line_with_it(Y);    % put It into current line
        { show, Char }                -> show(Char);                % insert char in current line
        { lint }                      -> do_lint();
        { tether }                    -> tether();
        { toggle_update_screen }      -> toggle_update_screen(); 
        { delete_line, Line_Num }     -> delete_line(Line_Num);
        % { insert_line, Line_Num }     -> insert_line();
        {_Action, Char}               -> show(Char)                
    end.    

%% build_event( K) when K == ?ceKEY_F(1) ->
%%    halt();
%% build_event( ?ceKEY_ESC) ->
%%     application:stop(cecho)

% ---------------------------------------------------

% put it into a remark 

put_into_remark() ->
    [{it, It}] = ets:lookup( db1, it),
        {CY,_CX} = cecho:getyx(),              
        % cecho:mvaddstr( CY+1, 1, io_lib:format( "% ~p", [It] )),
        K = lists:flatten( io_lib:format( "% ~p", [It] ) ),
        % S = io_lib_pretty:print(It),
        % Out = lists:flatten(S),
        L = string:split(K, "\n"),
        % ets:insert(db1,{{line,CY+1}, "% " ++ hd(L)  }),
        put_each_into_remark( L, CY+1),
        ok.

% put each line into document limit of 100 lines.  

put_each_into_remark( String, 100) when length(String) < 10 -> 
    ok;
put_each_into_remark( [H|T], N) ->
    ets:insert(db1,{{line, N}, "% " ++ H }),
    Out = string:split(T, "\n"),
    timer:sleep(10),
    put_each_into_remark( Out, N+1).

%% eval_string in It

eval_string() ->  
    [{it, It}] = ets:lookup( db1, it),            % get It  
    % 
    process_flag(trap_exit, true),                % trap_exit   
    ets:insert(db1, {eval_result, "crash"}),      % default result is "crash" or fail
    spawn_link(?MODULE, eval_str, [It]),          % spawn eval_str with data 
                                                  %   put the result in db as eval_result
                                                  %   if it doen't crash. 
    timer:sleep(500),                             % wait for result to be available 
    [{_,Result}] = ets:lookup(db1, eval_result),  % get result 
    ets:insert( db1, {it, Result}),               % put result into It  
    ok.

% SASE: "self addressed stamped envelope", the full address of sender 

request_line( N, SASE ) ->
    % {MY, _MX}  = cecho:getmaxyx(),
    % event_queue ! [{loc_store}, { mvaddstr, MY-2, 1, " get req " }, {loc_restore}],
    % get current line 
    % {CY, _CX}  = cecho:getyx(),
    % need to add in scroll
    
    %[{scroll, Scroll}] = ets:lookup(db1, scroll),
    %Scroll_Line_Num    = N + Scroll,
    %Exists             = ets:member(db1, {line, Scroll_Line_Num}),
    %if 
    %    Exists == true -> 
            [{{line,_}, Line}] = ets:lookup(db1, {line, N}),
    %        [{{line,_}, Line}] = ets:lookup(db1, {line, Scroll_Line_Num});
    %    true -> 
    %        Line = line_does_not_exist
    % end,
    SASE ! Line.

request_yx( SASE ) ->
        % {MY, _MX}  = cecho:getmaxyx(),
        % event_queue ! [{loc_store}, { mvaddstr, MY-2, 1, " get req " }, {loc_restore}],
        % get current line 
        {CY, CX}  = cecho:getyx(),
        % need to add in scroll
        % [{scroll, Scroll}] = ets:lookup(db1, scroll),
        % Scroll_Line_Num    = N + Scroll,
        % Exists             = ets:member(db1, {line, Scroll_Line_Num}),
        SASE ! {CY, CX}.

request_max_yx( SASE ) ->
        % {MY, _MX}  = cecho:getmaxyx(),
        % event_queue ! [{loc_store}, { mvaddstr, MY-2, 1, " get req " }, {loc_restore}],
        % get current line 
        {MY, MX}  = cecho:getmaxyx(),
        % need to add in scroll
        % [{scroll, Scroll}] = ets:lookup(db1, scroll),
        % Scroll_Line_Num    = N + Scroll,
        % Exists             = ets:member(db1, {line, Scroll_Line_Num}),
        SASE ! {MY, MX}.

request_lookup( Key, SASE ) ->
    [{_,Value}] = ets:lookup(db1, Key),
    SASE ! Value.

create_line_after(Line_Num) ->

    {_MY, MX}  = cecho:getmaxyx(),
    {_CY, CX}  = cecho:getyx(),

    % need to add in scroll
    [{scroll, Scroll}] = ets:lookup(db1, scroll),
    SLine_Num = Line_Num + Scroll,
    move_unicode_line_down(SLine_Num),
    
    [{{line,_}, Line}] = ets:lookup(db1, {line, SLine_Num}),
    Left_Side  = string:sub_string(Line,    1, CX-1),
    Right_Side = string:sub_string(Line,   CX, MX),
    % ----- make room, make room
    Last_Line_Num = find_last_line(),
    event_queue ! [{debug, Last_Line_Num }],          % watching what is happening  
    ets:insert(db1, [{{line, Last_Line_Num+1}, "                 "}] ),
    shift_lines_up_one(SLine_Num, Last_Line_Num),     % shift up each line inbetween  start and end 
    % ----- insert parts of old line
    Spaces = string:copies(" ",MX),
    ets:insert(db1, [{{line, SLine_Num},   Left_Side ++ Spaces }] ),
    ets:insert(db1, [{{line, SLine_Num+1}, Right_Side ++ Spaces }] ),
    ok.

% used by create new line after  

shift_lines_up_one(Line_Num, Line_Num) when Line_Num > 0 -> ok;
shift_lines_up_one(Line_Num, Target_Line_Num) ->
    Previous_Line_Num                   = Target_Line_Num - 1,
    [{{line, Previous_Line_Num}, Line}] = ets:lookup(db1, {line, Previous_Line_Num}),
    ets:insert(db1, [{{line, Target_Line_Num}, Line}]),
    shift_lines_up_one(Line_Num, Target_Line_Num-1).

find_last_line() ->
    Last_Line_Num = find_last_line(1),
    Last_Line_Num-1.

find_last_line(N) ->
    Exists = ets:member(db1, {line, N}),
    if
        Exists == true -> Output = find_last_line(N+1);
        true -> Output = N
    end,
    Output.

move_unicode_line_down(SLine_Num) ->
    Line_Num = SLine_Num,
    % find unicode characters and locations  
    % [[X, Char] | List_of_chars] = ets:match(db1, {{char, Line_Num, '$1'}, '$2'} ),
    List_of_chars = ets:match(db1, {{char, Line_Num, '$1'}, '$2'} ),
    move_list_of_chars(Line_Num, List_of_chars).

move_list_of_chars(_, []) -> ok;
move_list_of_chars(Line_Num, [H|T]) ->
    [X, Char] = H,
    ets:insert(db1, {{char, Line_Num+1, X}, Char}),   % copy it down a line
    ets:delete(db1, {char, Line_Num, X}),             % delete source 
    move_list_of_chars(Line_Num, T).

% used by kill_line to copy line to kill_buffer 

delete_line(Line_Num) ->
    {_MY, MX}  = cecho:getmaxyx(),
    % {_CY, _CX}  = cecho:getyx(),
    % ----- need to add in scroll
    [{scroll, Scroll}] = ets:lookup(db1, scroll),
    SLine_Num          = Line_Num + Scroll,
    Last_Line_Num      = find_last_line(),
    event_queue ! [{debug, Last_Line_Num }],          % watching what is happening  
    shift_lines_down_one(SLine_Num, Last_Line_Num),  % down one in data base is up on screen
    % ----- make last line empty
    Spaces = string:copies(" ",MX),
    ets:insert(db1, [{{line, Last_Line_Num}, Spaces}] ),
    % note: last line is not deleted, only filled with spaces 
    ok.

% used by delete line 

shift_lines_down_one(Last_Line_Num, Last_Line_Num) -> done;
shift_lines_down_one(Line_Num, Last_Line_Num) ->
    % ----- get next line 
    Next_Line_Num            = Line_Num + 1,
    [{{line, _}, Next_Line}] = ets:lookup(db1, {line, Next_Line_Num}),
    % ----- save it 
    ets:insert(db1, [{{line, Line_Num}, Next_Line}]),
    shift_lines_down_one(Line_Num + 1, Last_Line_Num).

insert_char_at_loc(C,CX) ->
    [{it, Line}] = ets:lookup(db1, it),
    Tail         = string:substr(Line, CX),
    Head         = string:substr(Line, 1, CX-1),
    New_Line     = Head ++ [C] ++ Tail,
    ets:insert(db1, {it, New_Line}),
    ok.

update_line_with_it(CY) ->
    [{it, Line}] = ets:lookup(db1, it),
    ets:insert(db1, {{line, CY}, Line}),
    ok.

goto_mark() ->
    [{mark_loc, {CY, CX}}] = ets:lookup(db1, mark_loc ),
    
    % note: mark should be less than max_x and max_y
    {MY, MX}  = cecho:getmaxyx(),
    Y = min(CY, MY),
    X = min(CX, MX),
    event_queue ! [{{ mv, Y, X}, 32}],
    ok. 

set_mark() ->
    % note: consider multiple marks 
    {CY, CX}  = cecho:getyx(),
    ets:insert(db1, {mark_loc, {CY, CX}}),
    ok.

key_not_bound(C) ->
    {MY, _MX}  = cecho:getmaxyx(),
    {CY, CX}  = cecho:getyx(),
    if C == 0 ->
        set_mark(),
        event_queue ! [
            {loc_store}, 
            { mvaddstr, MY-2, 1, "-> Mark set: " ++ integer_to_list(CY) ++ " , " ++ integer_to_list(CX) ++ "     " }, 
            {loc_restore}];
    true ->
        event_queue ! [
            {loc_store}, 
            { mvaddstr, MY-2, 1, integer_to_list(C) ++ ":key not bound          " }, 
            {loc_restore}]
    end,
    ok.

build_event( ) ->
    receive
        C -> C
    end,
    % ----- send copy to tethered editor if it exists
    % ----- 
    [{tether, Tether}]                 = ets:lookup(db1, tether),
    [{export_events, Export_Flag}]     = ets:lookup(db1, export_events),   % flag to stop export of events 
    % [{exclude_events, Exclude_Events}] = ets:lookup(db1, exclude_events),  % do not export these events 
    [{found_nodes, Found_Nodes}]       = ets:lookup(db1, found_nodes),
    {JY,JX}  = cecho:getyx(),
    if
        (Tether == true) and (Export_Flag == false) and (length(Found_Nodes) > 0) ->
            {event_queue, hd(lists:reverse(Found_Nodes))} ! [
                {loc_store},
                {{mv, JY, JX}, C},  
                {loc_restore}];
        true -> ok
    end,

    % remember jumpback location
    % {CY, CX} = cecho:getyx(),  
     
    % state of keyboard, control-x, or control-q, etc.
    [{state, State}]      = ets:lookup(db1, state),

    % ----- experiment to test char bindings
    % -----   test on control keys less than/eq-to control-I
    % -----   13 is ?ceKEY_ENTER
    %  previous value  (C < 9) or (C == 10) ->
    if  
        (C < 20) or (C == 20) ->
                Exists = ets:member(db1, {bind, C}),
                if
                    Exists == false ->
                        key_not_bound(C),  % report to user: key not bound  
                        build_event();     % safe excape, restrart loop 
                    true -> safe_to_go_forward
                end,
                [{{bind, _Char}, Action}] = ets:lookup(db1, {bind, C}),
                if
                    element(1, Action) == run ->
                        {run, Func} = Action,
                        erlang:apply(?MODULE, Func, []),
                        build_event();
                    element(1, Action) == send ->
                        {send, Script} = Action,
                        event_queue ! Script,
                        build_event();
                    element(1, Action) == send_to ->
                        {send_to, The_Target, Script} = Action,
                        The_Target ! Script,
                        build_event();
                    true -> ok                    
                end;
        true -> ok
    end,

    % character generates events
    case C of
      ?ceKEY_UP        -> event_queue ! [{move_cursor_up}];  
      %  event_queue ! {delta, -1,  0};    % raw movement generates no new cffhars so no  
       %   event transaction is needed.    
      ?ceKEY_DOWN      -> event_queue ! [{move_cursor_down}];        
      ?ceKEY_RIGHT     -> event_queue ! {delta,  0,  1};  
      ?ceKEY_LEFT      -> event_queue ! {delta,  0, -1};
      ?ceKEY_ENTER     -> event_queue ! [{enter}];
                            % event_queue ! [{line_into_db},
                            % {delta, 1, 0}, {move_left_all},
                            % {clear_line}, {refresh} ];  
      _BACKSPACE = 263 -> event_queue ! {backspace};    % backup_key();
      % _Space = 32      -> event_queue ! {space};
      
      % is not really a delete key
      ?ceKEY_DEL       -> event_queue ! [   { loc_store},
                                            { delta, 0, -1},
                                            { action, 32},
                                            { delta, 0, -1},
                                            { loc_restore} ];
      % ?ceCtrl_s        -> save_file();     % save buffer to file on path
      ?ceCtrl_r        -> cecho:addstr("read");
      _Ctrl_o = 15     -> % show( 4194410 );  % lower right corner
                            show_strange_char( 4194410 );
      _Ctrl_n = 14     -> % show( 4194413 );  % lower left  corner
                            show_strange_char( 4194413 );
      _Ctrl_q = 17     -> % show( 4194412 );  % upper left  corner
                            show_strange_char( 4194412 );
      _Ctrl_p = 16     -> % show( 4194411 );  % upper right  corner
                            show_strange_char( 4194411 );
      % _Ctrl_k = 11     -> event_queue ! {kill_line};  % show($K);        
      % _Ctrl_t = 20     -> tether();         % send copy of keyboard input to editor running in another node/window
      % _Ctrl_l = 12     -> toggle_update_screen();
      _Crtl_z = 

      % _Ctrl_e = 5      -> execute_line();
      _Ctrl_u = 21     -> yank_kill_buf();

      _Ctrl_x = 24     -> control_x();
      _Ctrl_y = 25     -> yank_kill_buf();                 % yank contents of kill buffer
      
      % events less than ascii 11 are stored in ets data-store key binding and functions 
    
      % _Ctrl_g = 7      -> event_queue ! {kill_line};       % grab line, cut line, store in kill buffer
      % _Ctrl_h = 8      -> help_info();
      % _Ctrl_a = 1      -> event_queue ! {show, 4194421};   % right ceACS_RTEE
      % _Ctrl_b = 2      -> toggle_update_screen();
      % _Ctrl_d = 4      -> toggle_update_screen();
      _Ctrl_w = 23       -> input_filename();    

      {alt, _Key}         -> event_queue ! {kill_line};
      % _Alt = 27         -> event_queue ! {kill_line};  
      %$2                 -> event_queue ! [{show, 50},{spawn_ant}];   % number 2 spawns an ant
      $o when State==ctrl_x -> cecho:addstr("other window");
      $2 when State==ctrl_x -> event_queue ! [{show, $2}, {spawn_ant}];
      $s when State==ctrl_x -> load_file(), event_queue ! [ {clear_state}];
      $e when State==ctrl_x -> toggle_send_event_stream_to_peer();
      $p when State==ctrl_x -> input_path();
      $n when State==ctrl_x -> input_filename();
      $w when State==ctrl_x -> save_file(),          ets:insert(db1, {state, none});
      $r when State==ctrl_x -> cecho:addstr("/rho"), ets:insert(db1, {state, none}),
                                ets:insert(db1, {{char, JY, JX-1}, "/rho"}  );
      $i when State==ctrl_x -> cecho:addstr("/iota"), ets:insert(db1, {state, none}),
                                ets:insert(db1, {{char, JY, JX-1}, "/iota"}  );    
      $t when State==ctrl_x -> read_tex_symbol();
      $l when State==ctrl_x -> ets:insert(db1, [{{refresh, value}, 10000 }]),
                                ets:insert(db1, {state, none});
      32 when State==ctrl_x -> goto_mark();
      % $h when State==ctrl_x -> toggle_helper();
      A                     -> event_queue ! [{show, A}, {clear_state}]
    end,
    cecho:curs_set(?ceCURS_NORMAL),  
    build_event().  

% announce state in mini-buffer

read_tex_symbol() -> ok.

toggle_send_event_stream_to_peer() ->
    {MY, _MX}     =  cecho:getmaxyx(),
    [{_, State}]  =  ets:lookup(db1, export_events),
    NState        =  not State, 
    ets:insert(db1,{export_events, NState}),
    %----------------------------
    [{found_nodes, Found_Nodes}] = ets:lookup(db1, found_nodes),
    Last_Found_Node = hd(lists:reverse( Found_Nodes)),
    %----------------------------
    event_queue ! [  
       { loc_store },
       { mvaddstr, MY-2, 1, [ "toggle send event stream to peer: " ++ atom_to_list(NState) ++ " " ++
        atom_to_list(Last_Found_Node)          ] },
       { loc_restore }    
    ],
    ok.

toggle_helper() ->
        Where_Is = whereis(helper),   % gets pid
        if
            Where_Is == undefined   -> Is_Alive = undefined;
            true                    -> Is_Alive = is_process_alive(Where_Is)
        end,
        event_queue ! [{ mvaddstr, 10, 1, [ atom_to_list(Is_Alive) ] }],
        if
            Is_Alive == true ->
                exit(whereis(helper), kill);
            true ->
                PID = spawn(?MODULE, helper, []),
                register(helper, PID)
        end,
        ok.

% toggle_update_screen() 
%   if update_screen process is alive then kill it
%   if update_screen process is undefined then start it 
%   flash status of process on the screen 

toggle_update_screen() ->
    Where_Is = whereis(update_screen),
    if
        Where_Is == undefined   -> 
            Is_Alive     = undefined,
            Screen_Value = false;
        true                    -> 
            Is_Alive = is_process_alive(Where_Is),
            Screen_Value = true
    end,
    {MY, _MX}           = cecho:getmaxyx(),
    % be careful, this is not an event transaction 
    event_queue ! [{ loc_store },    
     [{ mvaddstr, MY-2, 1, [ "update_screen: " ++ atom_to_list(not Screen_Value ) ++ " " ] }],
      { loc_restore }],
    if
        Is_Alive == true ->
            exit(whereis(update_screen), kill);
        true ->
            PID = spawn(?MODULE, update_screen, []),
            register(update_screen, PID)
    end,
    ok.

% load help file into buffer 

help_info() ->
    ets:insert(db1, {filename, "help.txt"}),
    Path = "/home/eric/Downloads/cecho/data/",
    ets:insert(db1, {path, Path}),
    load_file().

info_info() ->
    ets:insert(db1, {filename, "info.txt"}),
    Path = "/home/eric/Downloads/cecho/data/",
    ets:insert(db1, {path, Path}),
    load_file().

    % ets:insert(db1, {filename, "default.txt"}).

% load a file into buffer 

load_file() ->
        {MY, _MX}               = cecho:getmaxyx(),
        [{_, The_Filename}]     = ets:lookup(db1, filename),
        Filename                = string:trim(The_Filename),

        [{_, The_Pathname}]     = ets:lookup(db1, path),
        Pathname                = string:trim(The_Pathname),

        event_queue ! [
            { loc_store },
            { mvaddstr, MY-2, 1, [ "load file: " ++ Filename ++ "          "] },
            { loc_restore }    
        ],
        % Lines = collect_lines(),
        % CWD = get_cwd()  
        % Path = "/home/eric/Downloads/cecho/data/",
        {ok, Data}   = file:read_file(Pathname ++ Filename  ),
        Binary_Lines = binary:split(Data, [<<"\n">>], [global]),
        Binary_Line  = hd(Binary_Lines),
        Line         = binary_to_list(Binary_Line),
        ets:insert(db1,{{line,1}, Line}),
        parse_lines(Binary_Lines, 1),
        Line.

parse_lines([], _Line_Number) -> ok;
parse_lines([Binary_Line | Binary_Lines], Line_Number) ->
    Line = binary_to_list(Binary_Line),
    ets:insert(db1,{{line,Line_Number}, Line}),
    parse_lines(Binary_Lines, Line_Number+1).


input_filename() ->
    {MY, MX} = cecho:getmaxyx(),
    ets:insert(db1,{minibuffer_target, filename}),
    event_queue ! [{ mvaddstr, MY-2, 1, [ "                " ] }],
    show_spaces(round(MX/2)),
    event_queue ! [{ mvaddstr, MY-2, 1, [ "input filename: " ] }],
    ok.

show_spaces(0) -> done;
show_spaces(N) ->
    show_space(),
    show_spaces(N-1).

show_space() ->
    event_queue ! [{ show,32}].

input_path() ->
    ets:insert(db1,{minibuffer_target, path}), 
    % there is a process that stores the minibuffer into
    %   the minibuffer_target,  
    {MY, _MX}          = cecho:getmaxyx(),
    event_queue ! [{ mvaddstr, MY-2, 1, [ "                                           " ] }],
    event_queue ! [{ mvaddstr, MY-2, 1, [ "input path: " ] }],
    ok.

% save file from buffer 

save_file() ->
    {MY, _MX}       = cecho:getmaxyx(),
    [{_, Filename}] = ets:lookup(db1, filename),
    The_Filename    = string:trim(Filename),
    [{_, Path}]     = ets:lookup(db1, path),
    event_queue ! [
        { loc_store   },
        { mvaddstr, MY-2, 1, [ "save file: " ++ The_Filename ] },
        { loc_restore }
    ],
    Lines = collect_lines(),
    file:write_file(Path ++ The_Filename, [Lines]),
    ok.

% used by save_file

collect_lines() ->
    {MY, _MX}  = cecho:getmaxyx(),
    collect_lines(MY-5, "").

collect_lines(0, Acc) -> Acc;
collect_lines(N, Acc) ->
    [{{line, N}, Line}] = ets:lookup(db1, {line, N} ),
    collect_lines(N-1, Line ++ [10] ++ Acc).

clear_state() ->
    ets:insert(db1, {state, none}).

% show strange char: helps with extended ascii chars

show_strange_char(C) ->
        {CY, CX}  = cecho:getyx(),
        {MY, _MX}  = cecho:getmaxyx(),
        if
            CY < (MY-5) ->
                event_queue ! [
                    { loc_store },
                    { get_line },  
                    { insert_char, C, at_loc, CX },
                    { update_line, CY, with_it },
                    {{ mv, CY, CX}, C },
                    { loc_restore }
                ];
            true -> ok
        end,
        ok.

move_cursor_down() ->
    {CY, _CX}  = cecho:getyx(),
    {MY, _MX}  = cecho:getmaxyx(),
    % mark
    % Last_Line_Num   = find_last_line(),
    % Screen_Line_Num = CY + Scrool,
    % Last_Safe_Line = Screen_Line_Num - (MY-5),  
    if
        CY > (MY-6) ->             % (MY- 6)
            % ets:insert(db1, [{refresh_speed, 500}]),
            [{scroll, Scroll}] = ets:lookup(db1, scroll),
            Next_Line_Exists = ets:member(db1, {line, CY+Scroll+1}),
            if
            not Next_Line_Exists -> 
                NScroll = Scroll,
                event_queue !
                [   { loc_store},
                    { mvaddstr, MY-2, 1, [ "scroll: " ++ integer_to_list(NScroll) ++ " at eof"] },
                    { loc_restore}
                ];
            true ->
                NScroll = Scroll + 1,
                event_queue !
                [   { loc_store},
                    { mvaddstr, MY-2, 1, [ "scroll: " ++ integer_to_list(NScroll) ++ "          "] },
                    { loc_restore}
                ]
            end, 
            ets:insert(db1, [{scroll, NScroll}]),
            ok;
        true -> ok
    end,
    event_queue ! {delta,  1,  0},
    ok.

move_cursor_up() ->
        {CY, _CX}  = cecho:getyx(),
        {MY, _MX}  = cecho:getmaxyx(),
        if
            CY == 0 ->
                % ets:insert(db1, [{refresh_speed, 500}]),  % temporary speed up refresh
                [{scroll, Scroll}] = ets:lookup(db1, scroll),
                if
                    Scroll > 0 -> NScroll = Scroll - 1;
                    true       -> NScroll = Scroll
                end,
                ets:insert(db1, [{scroll, NScroll}]),
                event_queue !
                    [   { loc_store},
                        { mvaddstr, MY-2, 1, [ "scroll: " ++ integer_to_list(NScroll) ++ "  " ] },
                        { loc_restore}
                    ],
                % ets:insert(db1, [{refresh_speed, 1000}]),   % stop speed up refresh
                ok;
            true -> ok
        end,
        event_queue ! {delta,  -1,  0},
        ok.

enter() ->  
        { CY, _CX}  = cecho:getyx(),
        {_MY, MX}  = cecho:getmaxyx(),
        % {{line,CY}, Line} = ets:lookup({line,CY}),
        % _Left_String  = string:sub_string(Line, 1, CX),
        % _Right_String = string:sub_string(Line, CX+1, MX),
        % send create line msg to create_line process
        event_queue ! [{create_line_after, CY }, {delta,  1,  -MX}],        
         % move down one line then all the way left
        ok.

control_x() ->
    ets:insert(db1, {state, ctrl_x}).

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
    % show_row_and_col(CY, CX+1),
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
    cecho:refresh(),
    ok.

%show_row_and_col(CY, CX) ->
 %   {MY, _MX}   = cecho:getmaxyx(),
  %  cecho:mvaddstr(MY-2, 10, io_lib:format("(~w x ~w)==", [CY, CX] )),
   % cecho:move(CY, CX).

% not used

space_key() ->
        {CY, CX} = cecho:getyx(),
        % ------------------------------------------- update line
        { _MY, MX}   = cecho:getmaxyx(),
        { JY, JX}   = cecho:getyx(),    % remember jumpback location
        % show_row_and_col(CY, CX+1),

        [{{line, CY}, Line}]  = ets:lookup(db1, {line, CY}),

        Rotated_Line          = rotate(CX-1, Line),
        Updated_Line          = reverse_rotate( CX-1, [32] ++ Rotated_Line ),

        % -------------------------------------------- save Updated_Line        
        ets:insert(db1, {line,       Updated_Line}),
        ets:insert(db1, {{line, CY}, Updated_Line}),
        % print Updated_Line
        % show_line(CY),
        cecho:mvaddstr(CY, 1, io_lib:format("~s", [lists:sublist(Updated_Line,MX-3)] )),
        if
            length(Updated_Line) > (MX-2) ->  
                cecho:mvaddstr(CY, MX-1, io_lib:format("%", [] )),  
                cecho:move(JY,JX);
            true -> ok
        end,
        cecho:move(CY, CX+1),
        % cecho:refresh(),
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
%  todo: need to add scroll 

yank_kill_buf() ->
    [{_, Kill_buf_line_count}] = ets:lookup(db1, kill_buf_line_count),   % get kill buf line count 
    yank_kill_buf(Kill_buf_line_count),

    ets:insert(db1, {kill_buf_line_count, 1}),
    
    %
    % ----- Build Updated_Line
    %
    % {CY, CX}                = cecho:getyx(),
    % [{kill_buf, Kill_Buf}]  = ets:lookup(db1, kill_buf),                        % get value of kill buffer
    % [{{line, CY}, Line}]    = ets:lookup(db1, {line, CY}),                       % get value of line number CY
    % Rotated_Line            = rotate(CX, Line),                                   % rotate line to position CX
    % Kill_Buf_Len            = length(Kill_Buf),                                    %
    % Kill_Buf_Inserted       = lists:append(Rotated_Line, Kill_Buf),                 % insert Kill_Buf Value
    % Updated_Line            = reverse_rotate(CX + Kill_Buf_Len, Kill_Buf_Inserted),  % fix rotation
    %  
    % save Updated_Line
    %
    % ets:insert(db1, {line,       Updated_Line}),
    % ets:insert(db1, {{line, CY}, Updated_Line}),
    % print Updated_Line
    % show_line(CY),
    % cecho:mvaddstr(CY, 1, io_lib:format("~s", [Updated_Line] )),
    % ets:insert(db1, {kill_buf_line_count, 1}),
    ok.

yank_kill_buf(1) -> ok;
yank_kill_buf(N) ->
    %
    % ----- Build Updated_Line
    %

    [{{_, {_, _}}, Kill_Buf}] = ets:lookup(db1, {kill_buf, {line, N}} ),   % get particular line of kill buf 

    {CY, CX}                = cecho:getyx(),
    % [{kill_buf, Kill_Buf}]  = ets:lookup(db1, kill_buf),                        % get value of kill buffer
    [{{line, CY}, Line}]    = ets:lookup(db1, {line, CY}),                       % get value of line number CY
    Rotated_Line            = rotate(CX, Line),                                   % rotate line to position CX
    Kill_Buf_Len            = length(Kill_Buf),                                    %
    Kill_Buf_Inserted       = lists:append(Rotated_Line, Kill_Buf),                 % insert Kill_Buf Value
    Updated_Line            = reverse_rotate(CX + Kill_Buf_Len, Kill_Buf_Inserted),  % fix rotation
    %  
    % save Updated_Line
    %
    % ets:insert(db1, {line,       Updated_Line}),
    ets:insert(db1, {{line, CY}, Updated_Line}),
    % print Updated_Line
    show_line(CY),
    % cecho:mvaddstr(CY, 1, io_lib:format("~s", [Updated_Line] )),
    
    % event_queue ! {}
    event_queue ! {enter},
    event_queue ! {move_cursor_up},
    yank_kill_buf(N-1).

show_line(CY) ->
        {_CY, CX}                    = cecho:getyx(),
        {_MY, _MX}                   = cecho:getmaxyx(),
        [{{line, CY}, Line}]         = ets:lookup(db1, {line, CY}),            % get value of line number CY
        % Useful_Line_Len             = min( length(Line), MX),
        % {Truncated_Line, _Rest}     = lists:split(Useful_Line_Len-1, Line),   % chop line at width of screen or less
        % cecho:mvaddstr(CY, 0, io_lib:format([Line], [] )),
        cecho:mvaddstr(CY, 1, [Line] ),
        cecho:move(CY, CX),
        ok.

% kill line with control_g for grab or control_k for kill 

kill_line() ->
    {CY, _CX} = cecho:getyx(),
    %      get line of text at line number CY
    [{{line, CY}, Line}] = ets:lookup(db1, {line, CY}),

    % multi line kill buffer 
    [{_, Kill_buf_line_count}] = ets:lookup(db1, kill_buf_line_count),   % get kill buf line count 
    ets:insert(db1, {kill_buf_line_count,  Kill_buf_line_count + 1 }),       % increment kill buf line count
    ets:insert(db1, {{kill_buf,      {line, Kill_buf_line_count + 1}}, Line}),    % save each line 
    % end multi line kill buffer 

    %      put it into kill buffer
    ets:insert(db1, {kill_buf, Line}),
    Spaces = string:copies(" ",10),
    ets:insert(db1, {{line, CY}, Spaces}),
    % erase line
    cecho:mvaddstr(CY, 1, io_lib:format(Spaces, [] )),
    delete_line(CY), 
    % cecho:move(CY+1, 1),
    cecho:refresh(),
    ok.

% evaluate current line, put result on the screen 
%   uses the 'it' variable to pass data forward in program 

execute_line() -> event_queue !
     [  {get_line},                                       % put current line into it
        {delta,  1,  0}, {move_left_all}, {delta, 0, 1},  % move down
        {loc_store},                                      % save loc
        {eval_str},                                       % evaluate it 
        {put_into_remark},                                % put it into remark 
        %  {clear_line},      % compute                  
        {loc_restore}, {delta, 2, 0}].                    % goto starting loc then go down 2 lines 

%redraw() ->
 %   ok.

%read_filename(Filename) ->
%    C = _Char = cecho:getch(),
%    case C of  
%      ?ceKEY_ENTER -> Filename;  
%      A -> cecho:addch(A),
%            cecho:refresh(),
%            read_filename(Filename++[A])
%    end.

% ask_filename is not used  

% ask_filename() ->
%  {MY, _MX} = cecho:getmaxyx(),
%  cecho:mvaddstr(MY-1, 1, io_lib:format(" filename: ", [] )),    
%  Filename = read_filename([]),
%  cecho:mvaddstr(MY-2, 50, io_lib:format(" saved filename:~s ",[Filename] )),
%  file:write_file(Filename, "test data"),
%  ok.

mv(OffsetY, OffsetX) ->
    {CY, CX} = cecho:getyx(),
    {MY, MX} = cecho:getmaxyx(),
    FinalY = min(MY-4, max(0, CY + OffsetY)), % lowest line no is 1
    FinalX = min(MX-3, max(1, CX + OffsetX)), % lowest col no is 1
    cecho:move(FinalY, FinalX),
    cecho:refresh().

% insert typed char in buffer  
%   note: minibuffer needs its own version of show() 

show(Char) ->  
    {CY, CX}  = cecho:getyx(),
    {MY, MX}  = cecho:getmaxyx(),
    % cecho:addch(Char),

    % am I in the minibuffer? 
    if
        CY == MY-2 -> Mini_Buffer = true;
        true       -> Mini_Buffer = false
    end,
   
    %% build and save line
    [{scroll, Scroll}]      = ets:lookup(db1, scroll),
    Y = CY + Scroll,

    if
        Mini_Buffer == true ->
            [{minibuffer, Line_CY}] = ets:lookup(db1, minibuffer);        
        true ->
            [{{line, Y}, Line_CY}]  = ets:lookup(db1, {line, Y}) 
    end,

    Left_str      = string:sub_string(Line_CY, 1, CX-1),
    Right_str     = string:sub_string(Line_CY, CX),

    Updated_Line  = Left_str ++ [Char] ++ Right_str,  
    El_Choppo     = lists:sublist(Updated_Line, MX-3),
    cecho:mvaddstr(CY, 1, [El_Choppo] ),

    % then store in the minibuffer 
    if
        Mini_Buffer == true ->
            ets:insert(db1, {minibuffer, Updated_Line});   %  put into minibuffer 
        true ->
            ets:insert(db1, {{line, Y}, Updated_Line})   %  Update numbered line
    end,

    % show character (ascii/uni) code in command border
    %    one digit at a time

    D0 = round(math:floor(Char/100)),  % digit one
    C1 = Char-D0*100,
    D1 = round(math:floor(C1/10)),  % digit one
    D2 = Char - D1*10 - D0*100,
    cecho:mvaddch(MY-1, 1, 48+D0),          
    cecho:mvaddch(MY-1, 2, 48+D1),
    cecho:mvaddch(MY-1, 3, 48+D2),
    % cecho:mvaddstr(MY-2, 80, io_lib:format(" ~p ", [Char] )),
    cecho:move(CY, CX+1),
    cecho:refresh().

% evel a line of code  

eval_str(Expression) ->
   [{env, Env}]             = ets:lookup( db1, env),
   {ok, Tokens, _}          = erl_scan:string(Expression),
   {Ok, Parsed}             = erl_parse:parse_exprs(Tokens),
   if 
      Ok == ok -> ok;
      true     -> exit(normal)
   end,  
   try
     {_Value, Result, New_Env} = erl_eval:exprs(Parsed, Env),
     % save new environment and the result 
     ets:insert( db1, {env, New_Env}),
     ets:insert( db1, {eval_result, Result})
   catch
     error:_Error -> "bad juju"
   end,
   ok.

% First time eval to setup environment, Env

eval_str_init(Expression) ->
    {ok, Tokens, _}       = erl_scan:string(Expression),
    {ok, Parsed}          = erl_parse:parse_exprs(Tokens),
    {value, Result, Env}  = erl_eval:exprs(Parsed, []),
    ets:insert( db1, {env, Env}),
    Result.

% ===========================================================================

do_lint() ->
    % [{path, Path}] = ets:lookup(db1, path),
    % [{file, File}] = ets:lookup(db1, file),
    % Output = lint(Path ++ "hello.erl"),
    % {ok, [{_File, [{{Py,_Px},erl_lint,{Problem,Info}}] }] } = 
    {ok, [{_File, List }]} = lint("/home/eric/Downloads/cecho/data/hello.erl"),
    % {{Py,_Px},erl_lint,{Problem,Info}} = hd(List),
    {CY, _CX}  = cecho:getyx(),
    % Output = atom_to_list(Problem) ++ " on line " ++ erlang:integer_to_list(Py) ++ " var: " ++ atom_to_list(Info),
    Output = lint_loop(List, []),
    ets:insert(db1, {{line, CY}, Output}),
    ok.

lint(File) ->
    {ok,_,Bin} = compile:file(File, [debug_info, binary]),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Bin,[abstract_code]),
    erl_lint:module(AC,File).
	
lint_loop([], Accumulator) -> Accumulator;
lint_loop(List, Accumulator) -> 
    {{Py,_Px}, erl_lint, {Problem, Info}} = hd(List), 
    Output   = atom_to_list(Problem) ++ " on line " ++ erlang:integer_to_list(Py) ++ " var: " ++ atom_to_list(Info),
    Out_List = tl(List),
    lint_loop(Out_List, Output ++ " | " ++ Accumulator).