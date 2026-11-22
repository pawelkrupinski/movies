#!/usr/bin/env bash
# Purpose: The last green base of a main row, kept in git as refs/verified/<name>
# THE LAST GREEN BASE OF A MAIN ROW, KEPT IN GIT: refs/verified/<name> records the newest commit on main the row held
# on, so the row judges every commit since -- a superseded (replaced) or red run's commits stay in its range until a run
# passes with them. A row's base was the push's own `before` or "the newest green Main run": a replaced queued run's
# commits fell between two pushes and were never judged (the old-ref compile, 2026-11-21), and a red Main held every
# other row's range open (commit rules, simplification-2026-10-31 item 5). Shared by scripts/ci/commit-rules-ci.sh
# (refs/verified/commit-rules) and main.yml's old-ref compile (refs/verified/old-ref-compile).
#
#   verified-ref.sh base <name>      the commit the ref records, on stdout; nothing (exit 0) when the ref does not exist
#                                    yet (the row's first run: its caller falls back to the push's `before`); exit 2 when
#                                    origin cannot be read
#   verified-ref.sh advance <name>   moves the ref to a marker of HEAD -- only forward: the push leases the ref's current
#                                    value and goes only when the commit it records is HEAD's ancestor; a newer run past
#                                    HEAD already is said and passes; exit 2 when it cannot (the next run would judge from
#                                    a stale base, never silently)
#
# THE REF HOLDS A MARKER, NOT MAIN'S COMMIT (Main #2301): GitHub refuses github.token, an App without the `workflows`
# permission, any ref update whose new commits (old..new) change a workflow file, on refs/verified/* as on a branch, so
# pointing the ref at main's own commit failed on every range that touched .github/workflows/. The ref points at a
# parentless commit whose one file, `verified`, names the commit the row held on: it adds no workflow change.
set -uo pipefail

usage() { echo "usage: $0 base|advance <name>" >&2; exit 2; }
[ "$#" -eq 2 ] || usage
what=$1 ref=refs/verified/$2
case "$what" in base|advance) ;; *) usage ;; esac

# recorded <ref oid>: the commit the ref records -- a marker's `verified` file, or the ref's first form, the commit itself
recorded() {
    if git cat-file -e "$1:verified" 2>/dev/null; then git cat-file blob "$1:verified" | tr -d '[:space:]'
    else printf '%s' "$1"; fi
}
# marker <commit>: a parentless commit recording <commit> as the base the row held on
marker() {
    local blob tree
    blob=$(printf '%s\n' "$1" | git hash-object -w --stdin) || return 1
    tree=$(printf '100644 blob %s\tverified\n' "$blob" | git mktree) || return 1
    git -c user.name="verified ref" -c user.email="verified-ref@ci.invalid" commit-tree "$tree" -m "$ref held on main at $1"
}
fetch() { timeout 120 git fetch --quiet --no-tags origin "+$ref:$ref"; }

if [ "$what" = base ]; then
    # ls-remote first: 2 is "no such ref yet" (the row's first run), anything else non-zero a failed read
    timeout 60 git ls-remote --exit-code origin "$ref" > /dev/null; ls=$?
    case $ls in
        0) fetch || { echo "::error title=$ref::could not fetch $ref" >&2; exit 2; }
           recorded "$(git rev-parse "$ref")"; echo ;;
        2) ;;
        *) echo "::error title=$ref::could not read $ref (git ls-remote exit $ls)" >&2; exit 2 ;;
    esac
    exit 0
fi

head=$(git rev-parse HEAD)
current=$(timeout 60 git ls-remote origin "$ref" | cut -f1)
[ -z "$current" ] || fetch || current=unreadable
if [ -n "$current" ] && [ "$current" != unreadable ] && ! git merge-base --is-ancestor "$(recorded "$current")" "$head" 2>/dev/null \
   && git merge-base --is-ancestor "$head" "$(recorded "$current")" 2>/dev/null; then
    echo "  $ref is past $(git rev-parse --short=9 HEAD) already (a newer run advanced it)"
elif [ "$current" != unreadable ] && next=$(marker "$head") \
   && timeout 120 git push --quiet --force-with-lease="$ref:$current" origin "$next:$ref"; then
    echo "  $ref -> $(git rev-parse --short=9 HEAD)"
elif fetch && git merge-base --is-ancestor "$head" "$(recorded "$(git rev-parse "$ref")")" 2>/dev/null; then
    echo "  $ref is past $(git rev-parse --short=9 HEAD) already (a newer run advanced it)"
else
    echo "::error title=$ref::could not advance $ref to $(git rev-parse --short=9 HEAD): the next run would judge from a stale base"
    exit 2
fi
