#!/usr/bin/env bash

content=$(emacsclient -ne "(bitwarden-getpass-for-user \"$1\" \"$2\")")
echo "${content:1:-1}"
