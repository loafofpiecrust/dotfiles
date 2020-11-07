#!/usr/bin/env bash

content=$(emacsclient -e "(polybar--contents)")
echo "${content:1:-1}"
