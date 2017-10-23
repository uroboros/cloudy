#!/bin/bash

stack build
stack exec cloudy-exe "slave" "127.0.0.1" "10502" &
stack exec cloudy-exe "slave" "127.0.0.1" "10503" &
stack exec cloudy-exe "slave" "127.0.0.1" "10504" &
stack exec cloudy-exe "slave" "127.0.0.1" "10505" &

sleep 1

stack exec cloudy-exe "master" "127.0.0.1" "10501" 2000000 1000000 0
# (run with 2 sec SEND period, 1 sec GRACE period, SEED=0)
