#!/bin/bash

echo "Switching yadm repo origin URL to SSH"
yadm remote set-url origin "git@github.com:${USER}/dotfiles.git"

