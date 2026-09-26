#!/usr/bin/env bash
# The read-model snapshot's INPUT STAMP: a hash of the fixture corpus the snapshot was
# projected from, checked in beside it, so a corpus change that skipped the regeneration is
# caught in seconds rather than by the ~110s whole-corpus replay in CI's e2e shard.
#
# WHAT IT COVERS, AND WHAT IT CANNOT. The snapshot is a function of two things: the recorded
# corpus under test/resources/fixtures/08-06-2026/ and the pipeline code. Only the first is
# stamped. Pipeline code moves in ~40% of commits and the snapshot in ~2% (measured over the
# 1,500 commits before 2026-09-24), so a stamp over the code would cry stale on nearly every
# push and teach everyone to re-stamp without looking. The corpus moves rarely, and when it
# does the snapshot moved with it in 7 of 11 commits: that is where a stamp earns its keep.
# A code change that shifts the snapshot is still caught by FilmScheduleEndToEndSpec.
#
# The hash is over git BLOB ids, so the committed tree (`git ls-tree`, instant) and the working
# tree (`git hash-object`, a few seconds over ~750 MB) produce the same value for the same bytes.
# The snapshot's own outputs (read-model-snapshot.json, expected-schedules.txt, expected-*.html,
# the derivation corpus read-model-derivation-{rows.jsonl,hashes.tsv} and this stamp) are not
# inputs and are left out.
#
# Usage:
#   scripts/read-model-snapshot-inputs.sh hash  [<rev>]   print the inputs' hash (working tree, or <rev>)
#   scripts/read-model-snapshot-inputs.sh check [<rev>]   fail when the stored stamp disagrees
#   scripts/read-model-snapshot-inputs.sh write           re-stamp from the working tree
#
# FilmScheduleEndToEndSpec writes the stamp whenever it regenerates the snapshot; run `write`
# by hand only after the spec has passed against the corpus as it stands.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

Dir=test/resources/fixtures/08-06-2026
Stamp=$Dir/read-model-snapshot.inputs.sha256
Tab=$'\t'
Outputs="^$Dir/(read-model-snapshot\\.json|read-model-snapshot\\.inputs\\.sha256|read-model-derivation-(rows\\.jsonl|hashes\\.tsv)|expected-schedules\\.txt|expected-[^/]*\\.html)(${Tab}|\$)"

sha256() { if command -v sha256sum >/dev/null; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi; }

# -z throughout: without it git C-quotes every non-ASCII path ("Anio\305\202+morski.json"), and
# ls-tree and ls-files do not quote in step with each other.
#
# "<blob> <path>" per input file, byte-sorted by path, from a commit.
listing_at() {
    git ls-tree -r -z "$1" -- "$Dir" | tr '\0' '\n' | awk -F'\t' '{ split($1, m, " "); print $2 "\t" m[3] }' \
        | grep -Ev "$Outputs" | LC_ALL=C sort | awk -F'\t' '{ print $2 " " $1 }'
}

# The same listing from the working tree (tracked and untracked, ignored files excluded).
listing_worktree() {
    local paths
    # `-c` still lists a tracked file deleted from the working tree; a deleted file is not an input.
    paths=$(git ls-files -z -co --exclude-standard -- "$Dir" | tr '\0' '\n' | grep -Ev "$Outputs" \
        | while IFS= read -r p; do [ -f "$p" ] && printf '%s\n' "$p"; done | LC_ALL=C sort)
    paste -d' ' <(printf '%s\n' "$paths" | git hash-object --stdin-paths) <(printf '%s\n' "$paths")
}

hash_of() { if [ -n "${1:-}" ]; then listing_at "$1"; else listing_worktree; fi | sha256; }

stored_at() {
    if [ -n "${1:-}" ]; then git show "$1:$Stamp" 2>/dev/null; else cat "$Stamp" 2>/dev/null; fi \
        | grep -Eo '^[0-9a-f]{64}' | head -n 1 || true
}

case "${1:-}" in
    hash)  hash_of "${2:-}" ;;
    write)
        printf '%s\n' "$(hash_of)" > "$Stamp"
        echo "stamped $Stamp"
        ;;
    check)
        rev="${2:-}"
        want=$(stored_at "$rev")
        got=$(hash_of "$rev")
        if [ "$want" != "$got" ]; then
            echo "read-model-snapshot.json is STALE: the fixture corpus under $Dir changed since it was last regenerated." >&2
            echo "  stamped inputs: ${want:-<none>}" >&2
            echo "  current inputs: $got" >&2
            echo "Regenerate it (and its stamp) — see the regenerate-snapshots skill:" >&2
            echo "  rm $Dir/read-model-snapshot.json && sbt 'e2e/testOnly services.movies.FilmScheduleEndToEndSpec'" >&2
            exit 1
        fi
        echo "read-model snapshot inputs match their stamp ($got)."
        ;;
    *)
        echo "usage: $0 hash [<rev>] | check [<rev>] | write" >&2
        exit 2
        ;;
esac
