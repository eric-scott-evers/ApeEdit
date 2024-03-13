#!/bin/bash

# needed to get the beam the mood to do long names 
#   after fresh boot up, it runs quickly then stops
#   the actual node name is different like bookA  

erl -name sample@127.0.0.1 &  

# run NaCl

cd "/home/eric/Downloads/cecho"
# ./NaCl.escript

# create new terminal window
# ignore control-s
# run editor 
# run bash 

gnome-terminal -- sh -c "bash -c \"stty -ixon; ./NaCl.escript; exec bash \""

sleep 3

# run menubot inside playing.ex

cd "/home/eric/lang/elixir/playing"
iex -S mix

# elixir --no-halt playing.ex &









