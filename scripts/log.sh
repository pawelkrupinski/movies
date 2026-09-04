#!/usr/bin/env bash
# The four coloured output helpers every long-running script in scripts/ prints
# through, so a release, a screenshot run and a sync all look like the same tool.
#
# WHY IT EXISTS. store-screenshots-common.sh defined these, and then
# mobile-release.sh and ios-release.sh — which do not source it — defined
# `say`/`ok`/`warn`/`die` again, byte for byte. Three copies of an escape code is
# how one of them ends up a slightly different shade of green.
#
# `die` is the superset version: it tails $NOISE when the caller keeps a log of
# subprocess chatter (the Android screenshot driver funnels adb and gradle into
# one), and is exactly the plain one-liner when NOISE is unset — which is why the
# release scripts can take it unchanged.

say()   { printf '\033[36m▸\033[0m %s\n' "$*"; }
ok()    { printf '\033[32m✓\033[0m %s\n' "$*"; }
warn()  { printf '\033[33m!\033[0m %s\n' "$*" >&2; }
die()   { printf '\033[31m✗\033[0m %s\n' "$*" >&2
          [ -n "${NOISE:-}" ] && [ -s "$NOISE" ] && tail -5 "$NOISE" >&2
          exit 1; }
