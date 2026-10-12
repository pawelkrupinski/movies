#!/usr/bin/env bash
# `mongosh URI ARGS…` without the URI — and the password in it — on mongosh's command line, where
# `ps` shows it to every user: the URI travels in the environment, and the shell connects to it
# before any other --eval or --file runs (every one of them then sees that `db`).
#
# A SCRIPT that execs mongosh, not a shell function: a caller that backgrounds it (mirror.sh's
# tailer) must get mongosh's own pid in $! to kill, and a backgrounded function always hands back
# a forked subshell's instead. `; undefined`: mongosh prints the last --eval's value when only a
# --file follows, which would be the database name, ahead of the script's own output.
#
#   mongosh-at.sh <uri> [mongosh args…]
export KINOWO_MONGOSH_URI="$1"; shift
exec mongosh --nodb --eval 'db = connect(process.env.KINOWO_MONGOSH_URI); undefined' "$@"
