# Where the launchd agent's stdout/stderr land. Sourced (not executed) by both
# service.sh — which writes the path into the plist — and mirror.sh, which
# rotates the file it is being appended to. The two must name the SAME file: an
# agent logging to one path while the rotator truncates another is how a 23MB
# log got there in the first place.
MIRROR_LOG="$HOME/Library/Logs/kinowo-local-mirror.log"
