#!/bin/bash

# Install oh-my-zsh if absent

if [ -d "$HOME/.oh-my-zsh" ]; then
	echo "Oh-My-Zsh is installed on this userspace"
else
	echo "Installing Oh-My-Zsh"
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
	curl -fsSLo "$ZSH/themes/bebyx.zsh-theme" https://gist.githubusercontent.com/bebyx/38ce753760f4f3a71e56dc081e64aa8e/raw/97c7395ef610afafc0d971c3090252264a37e445/bebyx.zsh-theme
fi
