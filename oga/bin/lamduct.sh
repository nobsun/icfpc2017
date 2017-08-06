#!/bin/sh

NAME=$1
PORT=$2
NUM=$3
rm -f ./$1.$3.log
/usr/local/bin/lamduct --log-level 3 --game-port $2 --client-instance-logfile ./$1.$3.log ./$1.sh

