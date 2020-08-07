#!/bin/bash

offlineimap -a Personal -o & pid1=$!
offlineimap -a Northeastern -o & pid2=$!

wait $pid1
wait $pid2
echo "Last execution: $(date)"
