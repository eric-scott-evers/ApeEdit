#!/bin/bash

# needed to get the beam the mood to do long names 
#   after fresh boot up, it runs quickly then stops 

erl -name sample@127.0.0.1 &  

# run NaCl

cd "/home/eric/Downloads/cecho"
# ./NaCl.escript

gnome-terminal -- sh -c "bash -c \"./NaCl.escript; exec bash \""
