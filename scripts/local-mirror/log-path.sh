# Where the launchd agent's stdout/stderr land. Sourced (not executed) by both
# service.sh — which writes the path into the plist — and mirror.sh, which
# rotates the file it is being appended to. The two must name the SAME file: an
# agent logging to one path while the rotator truncates another is how a 23MB
# log got there in the first place.
MIRROR_LOG="$HOME/Library/Logs/kinowo-local-mirror.log"

# Prefix every line with when it was written. The log had none, so a burst of
# `MongoServerSelectionError` could not be dated or tied to a restart
# (2026-11-04). An executed mirror.sh routes ALL of its output through this —
# its own lines and every mongosh's raw stderr — and service.sh its few own, so
# nothing reaches the file undated. In mirror.sh rather than once around the
# agent: service.sh is re-read only when launchd restarts it (a Touch ID
# prompt), while mirror.sh is restarted on any change to its code. A `read` loop and date(1), not perl/gawk/bash-5 printf %(…)T: launchd
# runs /bin/bash 3.2, and the self-tests run on Linux too.
stamp_lines() {
  local line
  while IFS= read -r line || [ -n "$line" ]; do
    printf '%s %s\n' "$(date '+%Y-%m-%dT%H:%M:%S%z')" "$line"
  done
}
