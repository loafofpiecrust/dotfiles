#!/usr/bin/env bash

content=$(emacsclient -ne "(bitwarden-get-password \"$1\" \"$2\")")
echo "${content:1:-1}"
