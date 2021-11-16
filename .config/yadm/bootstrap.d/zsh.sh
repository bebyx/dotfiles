#!/bin/bash

# Install oh-my-zsh if absent

if [ -d "$HOME/.oh-my-zsh" ]; then
	echo "Oh-My-Zsh is installed on this userspace"
else
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi
