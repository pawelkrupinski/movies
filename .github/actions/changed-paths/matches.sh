#!/usr/bin/env bash
#
# Does any path on stdin fall under one of the patterns in $1? Prints `true` or `false`.
#
# THIS IS THE PER-TIER DEPLOY GATE, extracted from the workflow into a file of its own for one
# reason: it is the piece that decides whether a push restarts the worker, and a shell fragment
# buried in a YAML `run:` block cannot be tested. `K8sTierPathGatingSpec` feeds it the real pattern
# lists out of `.github/workflows/main.yml` and asserts that a web-only change does not match the
# worker's set.
#
# TWO PATTERN SHAPES ONLY, because that is all the tier filters use and a real glob engine would be
# a lot of surface for no gain:
#
#   `dir/**`  — anything under `dir/`
#   `path`    — that exact file
#
# Both the pattern list and the changed list are normalised the same way, so a YAML list can be
# handed over nearly verbatim: leading `- `, surrounding quotes, blank lines and `#` comments are
# all dropped.
#
# No `mapfile`/`readarray`: this runs under the spec on macOS too, where /bin/bash is still 3.2.
set -euo pipefail

patterns_file=${1:?usage: matches.sh <patterns-file> < changed-paths}

normalise() {
    sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' \
        -e 's/^-[[:space:]]*//' -e "s/^['\"]//" -e "s/['\"]\$//" |
        grep -v '^#' | grep -v '^$' || true
}

patterns=$(normalise <"$patterns_file")
changed=$(normalise)

while IFS= read -r path; do
    [ -n "$path" ] || continue
    while IFS= read -r pattern; do
        [ -n "$pattern" ] || continue
        case "$pattern" in
            */'**')
                # `web/**` → prefix `web/`. A file named `webhook.md` must not match `web/**`,
                # which is why the trailing slash stays in the prefix.
                if [ "${path#"${pattern%\*\*}"}" != "$path" ]; then
                    echo true
                    exit 0
                fi
                ;;
            *)
                if [ "$path" = "$pattern" ]; then
                    echo true
                    exit 0
                fi
                ;;
        esac
    done <<<"$patterns"
done <<<"$changed"

echo false
