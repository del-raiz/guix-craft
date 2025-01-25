#!/bin/sh
# Deploy Foot Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
foot=~/.config/foot/
if [ ! -d $foot ]; then
    echo "~/.config/foot/ does not exist - creating..."
    mkdir ~/.config/foot/
fi

# Initialization File
ln -s ~/repos/guix-craft/config/foot/foot.ini \
   ~/.config/foot/foot.ini
