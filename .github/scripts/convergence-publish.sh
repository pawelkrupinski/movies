#!/usr/bin/env bash
#
# The rolling-release writes a recording convergence leg makes (.github/actions/convergence-publish):
#
#   convergence-publish.sh tree <code> <complete> <log>
#       Upload enrichment-upload/enrichment-<code>.tar.zst over the country's working asset — and, when
#       the leg's boot completed (`[<code>] boot complete` in <log>, <complete> = true, a corpus run in
#       KINOWO_CONVERGENCE_CORPUS_RUN), its PINNED copy enrichment-<code>-<corpus run>.tar.zst in the same
#       call, named to the next step in $GITHUB_ENV as KINOWO_PINNED_TREE. Retires the gzip working asset.
#   convergence-publish.sh pin <code>
#       Write hermetic-<code>.txt naming the pinned pair, then prune all but the newest five pairs and the
#       fills / refetch / refused lists of the pairs it pruned.
#
# Every write goes through scripts/ci/gh-release.sh, which retries GitHub's transient answers. Reads
# FIXTURE_RELEASE_TAG from the environment (the calling workflow's `env`). Run from the repository root.
# Tested by .github/scripts/convergence-publish-test.sh against a stub `gh`.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RELEASE="$here/../../scripts/ci/gh-release.sh"

# Replaces the country's asset IN PLACE: one copy per country, never a history.
publish_tree() {
    local code="$1" complete="$2" log="$3"
    TAG="$FIXTURE_RELEASE_TAG"
    ARCHIVE="enrichment-upload/enrichment-$code.tar.zst"
    if [ ! -f "$ARCHIVE" ]; then
        echo "nothing to publish — no $ARCHIVE"
        return 0
    fi
    # `|| true`: the country legs race to create the release on its very first run and exactly one wins;
    # the others find it already there, which is the outcome they wanted anyway.
    "$RELEASE" view "$TAG" >/dev/null 2>&1 || \
      "$RELEASE" create "$TAG" --prerelease \
        --title "Convergence fixture trees (rolling)" \
        --notes "Recorded enrichment responses and remembered answers per country.
Replaced in place by every convergence leg — one copy each, no history.
Not a version of anything; do not treat as a release." || true
    # The PINNED copy rides the same upload when this leg pins: `gh release upload` sends its files side
    # by side (five at a time), and one stream to the release ran ~22 MB/s — the second, sequential upload
    # of the same bytes was 18 s of the UK recording's critical path (run 37071880312).
    pin=()
    if [ "$complete" = "true" ] && [ -n "${KINOWO_CONVERGENCE_CORPUS_RUN:-}" ] && \
       grep -q "^\[$code\] boot complete" "$log" 2>/dev/null; then
        pinned="enrichment-$code-$KINOWO_CONVERGENCE_CORPUS_RUN.tar.zst"
        ln -f "$ARCHIVE" "enrichment-upload/$pinned"
        pin=("enrichment-upload/$pinned")
        echo "KINOWO_PINNED_TREE=$pinned" >> "$GITHUB_ENV"
    fi
    "$RELEASE" upload "$TAG" "$ARCHIVE" "${pin[@]}" --clobber || return 1
    echo "published $ARCHIVE ${pin[*]} to release $TAG"
    # The gzip working tree this zstd one replaces: left beside it, readers' glob (`enrichment-<code>.tar.*`)
    # would download both. Gone after a country's first zstd publish, so a no-op from then on.
    legacy="enrichment-$code.tar.gz"
    if gh release view "$TAG" --json assets --jq '.assets[].name' | grep -qxF "$legacy"; then
        "$RELEASE" delete-asset "$TAG" "$legacy" -y || true
    fi
}

# The marker is written LAST, so a reader sees either the old pair or the new one.
pin_pair() {
    local code="$1"
    pinned="${KINOWO_PINNED_TREE:-}"
    if [ -z "$pinned" ]; then
        echo "the boot did not finish, or nothing was packed — the pinned pair stays as it was"
        return 0
    fi
    marker="hermetic-$code.txt"
    echo "$KINOWO_CONVERGENCE_CORPUS_RUN $KINOWO_CONVERGENCE_CORPUS_RECORDED_AT $pinned" > "$marker"
    "$RELEASE" upload "$FIXTURE_RELEASE_TAG" "$marker" --clobber || return 1
    echo "pinned $pinned as $code's hermetic pair"
    prune_pairs "$code"
}

# Keep the newest FIVE pairs: with recordings landing every ~15 min, keeping two evicted a pair before an
# identity measure dispatched on it could restore it (measure 37114004953). Either compressor's: the pairs
# pinned before the move to zstd are gzip.
prune_pairs() {
    local code="$1" names kept
    # allow-silenced: pruning old pairs is housekeeping; one that fails to delete is pruned next time.
    gh release view "$FIXTURE_RELEASE_TAG" --json assets \
            --jq ".assets[] | select(.name | test(\"^enrichment-$code-[0-9]+\\\\.tar\\\\.(gz|zst)$\")) | [.createdAt, .name] | @tsv" \
        | sort -r | tail -n +6 | cut -f2 \
        | while read -r stale; do "$RELEASE" delete-asset "$FIXTURE_RELEASE_TAG" "$stale" -y || true; done
    # ...and the fills, refetch and refused lists of the pairs just pruned (convergence-fill.sh): they only
    # mean anything over their own pair's tree.
    # allow-silenced: as above.
    names=$(gh release view "$FIXTURE_RELEASE_TAG" --json assets --jq '.assets[].name')
    kept=$(grep -oE "^enrichment-$code-[0-9]+\\.tar\\.(gz|zst)$" <<< "$names" | grep -oE '[0-9]+' | sort -u)
    grep -E "^(fill|refetch|refused)-$code-[0-9]+-[0-9]+(-[0-9]+)?(-[a-z][a-z-]*)?\\.(tar\\.zst|tsv)$" <<< "$names" \
        | while read -r asset; do
              run=$(cut -d- -f3 <<< "$asset")
              grep -qxF "$run" <<< "$kept" || "$RELEASE" delete-asset "$FIXTURE_RELEASE_TAG" "$asset" -y || true
          done
}

case "${1:-}" in
    tree) [ "$#" -eq 4 ] || { echo "usage: $0 tree <code> <complete> <log>" >&2; exit 64; }; publish_tree "$2" "$3" "$4" ;;
    pin)  [ "$#" -eq 2 ] || { echo "usage: $0 pin <code>" >&2; exit 64; }; pin_pair "$2" ;;
    *) echo "usage: $0 <tree <code> <complete> <log> | pin <code>>" >&2; exit 64 ;;
esac
