
# Add ~/.guix-profile if it exists
if [ -L ~/.guix-profile ]; then
    GUIX_PROFILE="/home/logoraz/.guix-profile"
    . "$GUIX_PROFILE/etc/profile"
fi
