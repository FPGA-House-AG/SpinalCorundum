#!/bin/sh

set -e

# sudo apt-get update && sudo apt-get install -f inotify-tools gconf2

while true;
do
  inotifywait -t 10 -e close_write $1
  #iverilog -o main $1
  #vvp main -lxt2
  gconftool-2 --type string --set /com.geda.gtkwave/0/reload 0
done
