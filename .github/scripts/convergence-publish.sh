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
#   convergence-publish.sh latest <code>
#       Write prod-latest-<code>.txt naming this complete recording's pinned copy (the hermetic marker's format)
#       WITHOUT making it the hermetic pair, then prune as `pin` does. What `--recording prod-latest` and
#       scripts/identity-repro replay.
#   convergence-publish.sh dead <code>
#       Delete the country's assets nothing reads any more: an identity overlay (`identity-overlay-<code>-<run>`, no
#       longer published since the overlay mode went) whose recording's tree is not on the release -- the overlay is
#       only ever laid over its own tree (scripts/identity-fetch-recording.sh). Run before a leg's uploads: run
#       41351315090 found the release at GitHub's 1000-asset cap, 101 of them such overlays, and a release at the cap
#       refuses every upload, the fold that would shrink it included. And the takes check's results (`takes-<kind>-<code>-`),
#       stored on a release of their own since (TAKES_RELEASE_TAG): 483 of the 1000.
#
# Pruning never removes a KEPT recording: the hermetic pair, prod-latest, and the identity measure's landing
# recordings ($KINOWO_KEPT_RECORDINGS, default "528 573": identity-measure's pinned pair and its second recording),
# so nightly prod-latest copies can never age the landing bar out of the release.
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
    "$RELEASE" upload "$TAG" "$ARCHIVE" ${pin[@]+"${pin[@]}"} --clobber || return 1
    echo "published $ARCHIVE ${pin[*]:-} to release $TAG"
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

# prod-latest: the marker only, LAST (a reader sees the old recording or the new one), then the same pruning.
mark_latest() {
    local code="$1"
    pinned="${KINOWO_PINNED_TREE:-}"
    if [ -z "$pinned" ]; then
        echo "the boot did not finish, or nothing was packed — prod-latest stays as it was"
        return 0
    fi
    marker="prod-latest-$code.txt"
    echo "$KINOWO_CONVERGENCE_CORPUS_RUN $KINOWO_CONVERGENCE_CORPUS_RECORDED_AT $pinned" > "$marker"
    "$RELEASE" upload "$FIXTURE_RELEASE_TAG" "$marker" --clobber || return 1
    echo "marked $pinned as $code's prod-latest recording"
    prune_pairs "$code"
}

# The runs pruning must keep for a country: its hermetic pair's, its prod-latest's, and the kept landing recordings.
kept_runs() {
    local code="$1" dir marker
    dir=$(mktemp -d)
    for marker in "hermetic-$code.txt" "prod-latest-$code.txt"; do
        # allow-silenced: an absent marker keeps nothing; the kept list below still holds.
        gh release download "$FIXTURE_RELEASE_TAG" --pattern "$marker" --dir "$dir" --clobber >/dev/null 2>&1 || true
        [ -f "$dir/$marker" ] && cut -d' ' -f1 "$dir/$marker"
    done
    rm -rf "$dir"
    tr ' ' '\n' <<< "${KINOWO_KEPT_RECORDINGS-528 573}"
}

# Keep the newest FIVE pairs: with recordings landing every ~15 min, keeping two evicted a pair before an
# identity measure dispatched on it could restore it (measure 37114004953). Either compressor's: the pairs
# pinned before the move to zstd are gzip.
prune_pairs() {
    local code="$1" names kept protected
    protected=$(kept_runs "$code" | grep -E '^[0-9]+$' | sort -u)
    # allow-silenced: pruning old pairs is housekeeping; one that fails to delete is pruned next time.
    gh release view "$FIXTURE_RELEASE_TAG" --json assets \
            --jq ".assets[] | select(.name | test(\"^enrichment-$code-[0-9]+\\\\.tar\\\\.(gz|zst)$\")) | [.createdAt, .name] | @tsv" \
        | sort -r | tail -n +6 | cut -f2 \
        | while read -r stale; do
              grep -qxF "$(cut -d- -f3 <<< "$stale" | cut -d. -f1)" <<< "$protected" && continue
              "$RELEASE" delete-asset "$FIXTURE_RELEASE_TAG" "$stale" -y || true
          done
    # ...and the fills, refetch and refused lists of the pairs just pruned (convergence-fill.sh): they only
    # mean anything over their own pair's tree.
    # allow-silenced: as above.
    names=$(gh release view "$FIXTURE_RELEASE_TAG" --json assets --jq '.assets[].name')
    kept=$(grep -oE "^enrichment-$code-[0-9]+\\.tar\\.(gz|zst)$" <<< "$names" | grep -oE '[0-9]+' | sort -u)
    # `|| true`: a release holding none (a country's first pin) is nothing to prune, not a failure under pipefail.
    { grep -E "^(fill|refetch|refused)-$code-[0-9]+-[0-9]+(-[0-9]+)?(-[a-z][a-z-]*)?\\.(tar\\.zst|tsv)$" <<< "$names" || true; } \
        | while read -r asset; do
              run=$(cut -d- -f3 <<< "$asset")
              grep -qxF "$run" <<< "$kept" || "$RELEASE" delete-asset "$FIXTURE_RELEASE_TAG" "$asset" -y || true
          done
}

prune_dead() {
    local code="$1" names trees
    # allow-silenced: pruning is housekeeping; a listing that fails prunes nothing now and the next leg's prunes it.
    names=$(gh release view "$FIXTURE_RELEASE_TAG" --json assets --jq '.assets[].name' 2>/dev/null) || return 0
    trees=$({ grep -oE "^enrichment-$code-[0-9]+\\.tar\\.(gz|zst)$" <<< "$names" || true; } | grep -oE '[0-9]+' | sort -u)
    { grep -E "^identity-overlay-$code-[0-9]+\\.tar\\.(gz|zst)$" <<< "$names" || true; } \
        | while read -r overlay; do
              run=$(grep -oE '[0-9]+' <<< "$overlay" | head -1)
              grep -qxF "$run" <<< "$trees" || "$RELEASE" delete-asset "$FIXTURE_RELEASE_TAG" "$overlay" -y || true
          done
    [ "$FIXTURE_RELEASE_TAG" != "${TAKES_RELEASE_TAG:-}" ] || return 0
    { grep -E "^takes-[a-z]+-${code}[-.]" <<< "$names" || true; } \
        | while read -r takes; do "$RELEASE" delete-asset "$FIXTURE_RELEASE_TAG" "$takes" -y || true; done
}

case "${1:-}" in
    dead) [ "$#" -eq 2 ] || { echo "usage: $0 dead <code>" >&2; exit 64; }; prune_dead "$2" ;;
    tree) [ "$#" -eq 4 ] || { echo "usage: $0 tree <code> <complete> <log>" >&2; exit 64; }; publish_tree "$2" "$3" "$4" ;;
    pin)  [ "$#" -eq 2 ] || { echo "usage: $0 pin <code>" >&2; exit 64; }; pin_pair "$2" ;;
    latest) [ "$#" -eq 2 ] || { echo "usage: $0 latest <code>" >&2; exit 64; }; mark_latest "$2" ;;
    *) echo "usage: $0 <tree <code> <complete> <log> | pin <code> | latest <code>>" >&2; exit 64 ;;
esac
