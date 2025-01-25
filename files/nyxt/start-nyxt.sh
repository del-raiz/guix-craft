#!/bin/sh

# Setup for GI-GTK Nyxt to look descent in Gnome (on Guix)

# Set title bar of GTK applications 
export GTK_THEME=Adwaita:dark

# Launch xterm in the background
/home/logoraz/.guix-home/profile/bin/nyxt &

# Sleep long enough to get the window open, and set transparency
sleep 2.00s
transset -a 0.90
