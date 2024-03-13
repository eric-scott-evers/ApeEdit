-module(test_salt).
% -include("cecho.hrl").
% -compile(export_all).
-export ([ start/0 ]).

start() ->
Salt_node = test_0(),
test_1(Salt_node),
test_2(Salt_node),
% test_3(Salt_node),
ok.

test_0() ->
Long_Name = list_to_atom( "bookZ@127.0.0.1" ),
net_kernel:start([Long_Name, longnames]),
% unregister(event_queue),
% register(event_queue, self()),
erlang:set_cookie(node(), cookie),
Nodes = find_nodes(),
Salt_node = hd(Nodes),
io:format("test_0: set up commumication with editor. done.~n"),
io:format(" editor node name is: ~w .~n",[Salt_node]),
Salt_node.

%% send grab to char_queue

test_1(Salt_node) ->
cls(Salt_node),
io:format("test_1: send grab/(kill line) to char_queue done.~n"),
ok.

%% send text event_queue

test_2(Salt_node) ->
cls(Salt_node),
Range_1 = "Wake up, Neo...",
Range_2 = "The Matrix has you...",
Range_3 = "Knock, knock.",
Range_4 = "  ",
% Enter = ?ceKEY_ENTER,
{event_queue, Salt_node} ! [{ delta, 1, -80}],
{event_queue, Salt_node} ! [{ delta, 1, -80}],
send_all(Salt_node, Range_1),
{event_queue, Salt_node} ! [{ delta, 1, -80}],
{event_queue, Salt_node} ! [{ delta, 1, -80}],
send_all(Salt_node, Range_2),
{event_queue, Salt_node} ! [{ delta, 1, -80}],
{event_queue, Salt_node} ! [{ delta, 1, -80}],
send_all(Salt_node, Range_3),
{event_queue, Salt_node} ! [{ delta, 1, -80}],
send_all(Salt_node, Range_4),
{event_queue, Salt_node} ! [{ delta, 1, -80}],
io:format("test_2: send text to event_queue. done.~n"),
ok.

% draw box by moving cursor

test_3(Salt_node) ->
{event_queue, Salt_node} ! [{ delta, 1, -80}],
{event_queue, Salt_node} ! [{ delta, 0, 3}],
{event_queue, Salt_node} ! [{ show, $*}],
{event_queue, Salt_node} ! [{ delta, 0, 1}],
{event_queue, Salt_node} ! [{ show, $*}],
{event_queue, Salt_node} ! [{ delta, 0, 1}],
{event_queue, Salt_node} ! [{ show, $*}],
{event_queue, Salt_node} ! [{ delta, 1, -1}],
{event_queue, Salt_node} ! [{ show, $*}],
{event_queue, Salt_node} ! [{ delta, 1, -1}],
{event_queue, Salt_node} ! [{ show, $*}],
{event_queue, Salt_node} ! [{ delta, 0, -2}],
{event_queue, Salt_node} ! [{ show, $*}],
{event_queue, Salt_node} ! [{ delta, 0, -2}],
{char_queue, Salt_node} ! 263, % Backspace
timer:sleep(100),
{event_queue, Salt_node} ! [{ show, $*}],
{event_queue, Salt_node} ! [{ delta, 0, -2}],
timer:sleep(100),
{char_queue, Salt_node} ! 263, % Backspace
timer:sleep(100),
{event_queue, Salt_node} ! [{ delta, -1, 1}],
timer:sleep(100),
{event_queue, Salt_node} ! [{ show, $*}],
timer:sleep(100),
{event_queue, Salt_node} ! [{ delta, 0, 1}],
timer:sleep(100),
{char_queue, Salt_node} ! 263, % Backspace
timer:sleep(100),
{event_queue, Salt_node} ! [{ delta, 3, 0}],
io:format("test_3: move cursor to draw a box. done.~n"),
ok.

send_all(_Salt_node, []) -> done;
send_all(Salt_node, [H|T]) ->
{event_queue, Salt_node} ! {show, H},
timer:sleep(100),
send_all(Salt_node, T).

cls(Salt_node) ->
% 15 is control_g for grab/kill line
go_up(Salt_node, 15),
grab_some(Salt_node, 7),
timer:sleep(500),
% must wait some after pumping the char_queue before pumping event_queue
% and/or grab is a slow process
% {event_queue, Salt_node} ! [{ delta, -1, -80}],
go_up(Salt_node, 15),
ok.

go_up(_Salt_node, 0) -> done;
go_up(Salt_node, N) ->
{event_queue, Salt_node} ! [{ delta, -1, -80}],
go_up(Salt_node, N-1).

grab_some(_Salt_node, 0) -> done;
grab_some(Salt_node, N) ->
{char_queue, Salt_node} ! 7,
grab_some(Salt_node, N-1).

find_nodes() ->
% ---------- find other local NaCl editors and say hello -----------
Digits = "0123456789",
Upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
Lower = "abcdefghijklmnopqrstuvwxyz",
All = Digits ++ Upper ++ Lower,
Node_Names = [ list_to_atom("book"++[X]++"@127.0.0.1") || X <- All],
Send_Move = fun(Target) ->
{event_queue, Target} ! [{ delta, 0, 1},{ delta, 0, -1}]
end,
lists:map(Send_Move, Node_Names),
timer:sleep(500),
% wait half second for messages to cause updates to nodes()
Found_Nodes = nodes(),
Send_Hi = fun(Target) ->
{event_queue, Target} ! [{show,32},{show,72},{show,72+33} ]
end,
lists:map(Send_Hi, Found_Nodes),
Found_Nodes.

