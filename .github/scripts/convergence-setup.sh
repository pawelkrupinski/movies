#!/usr/bin/env bash
#
# The logic of `.github/actions/convergence-setup`'s fixture steps, out of the action's inline
# `run:` blocks so it is tested (convergence-setup-test.sh, against a stub `gh`) and runnable
# locally. Each subcommand is one step; the action passes its inputs as arguments and the step's
# exports go to $GITHUB_ENV / $GITHUB_OUTPUT exactly as the inline block wrote them.
#
#   convergence-setup.sh pinned <code>
#       The pair pinned for <code> — the first line of `hermetic-<code>.txt` in the release
#       $FIXTURE_RELEASE_TAG: `<corpus run> <recorded at> <tree asset> [fills...]`. Prints
#       nothing (exit 0) when no pair is pinned, exits 1 on a FAILED read (gh-optional.sh), 2 on
#       a line without a tree. The ONE reader of the pin: this action and
#       scripts/restore-country-corpus.sh both read it here.
#
#   convergence-setup.sh pair <code> <mode> [given pair] [corpus run]
#       "Resolve the recorded pair to replay". A RECORDING leg takes the corpus it is given (its
#       own recorder run) or the newest, and the working tree. A HERMETIC leg replays the given
#       pair, else the named corpus run's bare pair (its tree, no fills), else the pinned one with
#       every fill published over its tree — and with none, fails: no pin is never a quiet
#       fall-back to live filling.
#
#   convergence-setup.sh corpus <code>
#       "Restore the recorded scrape corpus" into ./scrape-archive: this job's own recording (no
#       download), the pinned run's artifact (gone or unreadable = exit 1, each said apart), else the newest of the last 10 green
#       `Record scrape fixtures` runs that still has one.
#
#   convergence-setup.sh unpack <code> <mode>
#       "Unpack whichever fixtures are present": waits for the background tree restore, moves the
#       staged tree in, unpacks the corpus, and counts both — the remembered-answer cache apart.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
gh_optional="$repo/scripts/ci/gh-optional.sh"
# gh, or Forgejo's API from a laptop checkout of a Forgejo remote (scripts/forge-gh; a CI job runs its own gh)
KINOWO_GH=${KINOWO_GH:-$repo/scripts/forge-gh}

pinned() {
  local code="$1" dir marker present line corpus recorded tree
  dir="$(mktemp -d)"
  marker="hermetic-$code.txt"
  present=$("$gh_optional" release download "$FIXTURE_RELEASE_TAG" --pattern "$marker" --dir "$dir" --clobber) ||
    { rm -rf "$dir"; return 1; }
  [ "$present" = present ] || { rm -rf "$dir"; return 0; }
  line=$(head -1 "$dir/$marker")
  rm -rf "$dir"
  read -r corpus recorded tree _ <<< "$line"
  [ -n "${tree:-}" ] || { echo "$marker is malformed: '$line'" >&2; return 2; }
  echo "$line"
}

pair() {
  local code="$1" mode="$2" given="${3:-}" corpus_run="${4:-}" pair corpus recorded tree fills
  if [ "$mode" = "record" ]; then
    [ -n "$corpus_run" ] && echo "KINOWO_CONVERGENCE_PIN_CORPUS_RUN=$corpus_run" >> "$GITHUB_ENV"
    # A glob: the working tree is `.tar.zst` since it moved off gzip, `.tar.gz` until a
    # country's first zstd publish replaces (and deletes) it.
    echo "KINOWO_CONVERGENCE_TREE_ASSET=enrichment-$code.tar.*" >> "$GITHUB_ENV"
    echo "recording leg: corpus ${corpus_run:-newest}, working tree enrichment-$code.tar.*"
    return 0
  fi
  pair="$given" named=""
  # A NAMED recording (identity-model-convergence's `recording` input, scripts/identity-measure --recording): that run's
  # corpus and the tree recorded with it, and no fills -- the bare pair identity-measure-local and identity-slice lay
  # (scripts/identity-fetch-recording.sh), so a measurement over it on CI and on a laptop resolve the same inputs.
  if [ -z "$pair" ] && [ -n "$corpus_run" ]; then
    # allow-silenced: the recording date only labels the log line and the provenance env.
    recorded=$("$KINOWO_GH" run view "$corpus_run" --json createdAt --jq .createdAt 2>/dev/null) || recorded=""
    pair="$corpus_run ${recorded:-unknown} enrichment-$code-$corpus_run.tar.*" named=1
  fi
  # A failed read or a malformed pin is no pair, and fails just below.
  [ -n "$pair" ] || pair=$(pinned "$code") || pair=""
  read -r corpus recorded tree fills <<< "$pair"
  if [ -z "${tree:-}" ]; then
    echo "::error::No recorded pair pinned for $code (release asset hermetic-$code.txt) — a hermetic leg replays nothing else. Record one: gh workflow run \"Record scrape fixtures\" --ref main"
    exit 1
  fi
  # The FILLS published over this tree (convergence-fill.sh) — earlier legs' convergence rows' pre-suite
  # fills and any hand-dispatched `Convergence fill` — named here, once: a convergence row lays its own
  # published pre-suite fill over them, and its bisect request carries all of them in the pair. Every one
  # is laid over, or the restore fails (restore-enrichment-tree.sh): the pair never names a fill the leg
  # replayed without.
  if [ -z "$given" ] && [ -z "$named" ]; then
    fills=$("$here/convergence-fill.sh" fills "$code" "$corpus")
    pair="$corpus $recorded $tree${fills:+ $fills}"
  fi
  echo "hermetic leg: corpus from recording run $corpus ($recorded), tree $tree, fills: ${fills:-none}"
  {
    echo "KINOWO_CONVERGENCE_PIN_CORPUS_RUN=$corpus"
    echo "KINOWO_CONVERGENCE_TREE_ASSET=$tree"
    echo "KINOWO_CONVERGENCE_FILL_ASSETS=${fills:-}"
    [ -z "$named" ] || echo "KINOWO_CONVERGENCE_NAMED_RECORDING=$corpus"
  } >> "$GITHUB_ENV"
  echo "pair=$pair" >> "$GITHUB_OUTPUT"
}

# corpus_from <run> <recorded at> <label>: the provenance of a downloaded corpus, logged and exported
# so the spec can say WHICH corpus it replayed (`CorpusProvenance`) and a green leg can record it.
corpus_from() {
  echo "scrape corpus from $3 $1 ($2)"
  echo "KINOWO_CONVERGENCE_CORPUS_RUN=$1" >> "$GITHUB_ENV"
  echo "KINOWO_CONVERGENCE_CORPUS_RECORDED_AT=$2" >> "$GITHUB_ENV"
  ls -l scrape-archive/
}

corpus() {
  local code="$1" pinned present recorded run
  mkdir -p scrape-archive
  pinned="${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}"
  # A recording leg's own run: the leg records the corpus itself, in a step after this
  # action ("Record <country>'s corpus" in country-convergence-leg.yml), so there is
  # nothing to download — only the provenance to name.
  if [ -n "$pinned" ] && [ "$pinned" = "${GITHUB_RUN_ID:-}" ]; then
    echo "scrape corpus recorded by this job, in recorder run $pinned"
    echo "KINOWO_CONVERGENCE_CORPUS_RUN=$pinned" >> "$GITHUB_ENV"
    echo "KINOWO_CONVERGENCE_CORPUS_RECORDED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$GITHUB_ENV"
    exit 0
  fi
  if [ -n "$pinned" ]; then
    # A failed read (gh-optional.sh has asked a throttled storage again for ~15 minutes already) is not
    # an expired pair: run 38763455103 told its reader to re-record over a 503 "Egress is over the
    # account limit" that cleared within the hour.
    present=$("$gh_optional" run download "$pinned" --name "scrape-fixtures-$code" --dir scrape-archive) || {
      echo "::error::could not download recording run $pinned's corpus (scrape-fixtures-$code) — a failed read (gh's error above), not an expired pair; re-run the failed job once GitHub answers"
      exit 1
    }
    if [ "$present" = present ]; then
      # allow-silenced: the recording date only labels the log line and the provenance env.
      recorded=$("$KINOWO_GH" run view "$pinned" --json createdAt --jq .createdAt 2>/dev/null || echo unknown)
      corpus_from "$pinned" "$recorded" "PINNED recording run"
    else
      echo "::error::recording run $pinned's corpus is no longer downloadable — the pinned pair has expired; re-run Record scrape fixtures"
      exit 1
    fi
    exit 0
  fi
  # `id recordedAt` per line, newest first.
  while read -r run recorded; do
    present=$("$gh_optional" run download "$run" --name "scrape-fixtures-$code" --dir scrape-archive)
    if [ "$present" = present ]; then
      corpus_from "$run" "$recorded" "recording run"
      exit 0
    fi
  done < <("$KINOWO_GH" run list --workflow "Record scrape fixtures" --status success \
               --limit 10 --json databaseId,createdAt --jq '.[] | "\(.databaseId) \(.createdAt)"')
  echo "no recorded scrape corpus for $code — the leg will fail and say so"
}

unpack() {
  set -e
  local code="$1" mode="$2" tree_status stage entry target archive available cache remembered
  # The enrichment tree, fetched and unpacked in the background since the top of the action
  # ("Fetch the enrichment tree in the background"). A replay leg's missing pin (exit 3) ends
  # the leg; a recording falls through to fetching live.
  tree_status=0
  "$repo/scripts/ci/in-background.sh" wait tree-restore 900 || tree_status=$?
  if [ "$tree_status" -ne 0 ] && ! { [ "$tree_status" -eq 3 ] && [ "$mode" = "record" ]; }; then
    exit "$tree_status"
  fi
  # Moved in by rename (same filesystem), entry by entry; a directory already present is
  # merged into rather than replaced.
  stage="$RUNNER_TEMP/tree-stage/test/resources/fixtures"
  mkdir -p test/resources/fixtures
  for entry in "$stage"/* "$stage"/.[!.]*; do
    [ -e "$entry" ] || continue
    target="test/resources/fixtures/$(basename "$entry")"
    if [ -e "$target" ]; then cp -a "$entry/." "$target/"; else mv "$entry" "$target"; fi
  done
  # The scrape corpus, by its magic (`unpack-fixture-archive.sh`), as the tree was.
  archive="scrape-archive/scrapes-$code.tar.gz"
  if [ -f "$archive" ]; then
    "$here/unpack-fixture-archive.sh" "$archive"
    echo "restored $archive ($(du -h "$archive" | cut -f1))"
  else
    echo "no $archive — this leg fetches live for it and the recorder captures it next"
  fi
  # `|| true` and a subshell: a MISSING fixture directory is the normal
  # fallback path, not an error — the leg then fetches live and the
  # recorder captures it next. Without this, `find` exiting non-zero on
  # an absent path tripped `set -euo pipefail` and failed the whole leg
  # AFTER printing the count, which is how three legs died 40 seconds in
  # with both restores reporting success.
  available=$( { find test/resources/fixtures/corpus \
                      "test/resources/fixtures/enrichment-$code" \
                      -type f 2>/dev/null || true; } | wc -l )
  echo "fixture files available: $available"

  # Count the REMEMBERED-ANSWER cache separately from the recorded
  # responses, because they fail independently and only one of them is
  # visible in the file count above.
  #
  # The tree records what a fetch RETURNED. Half a country's films never
  # resolve and still cost three or four rating-slug guesses that 404,
  # and a 404 returns nothing to record — so those verdicts live only in
  # `.enrichment-cache`, and a leg that arrives without it re-asks every
  # one of them live and rate-paced. That is a 22-minute Poland leg
  # versus a 12-minute one, and the ONLY symptom is the clock: nothing
  # errors, coverage is identical, and the run still passes. Printing the
  # entry count on both sides of the artifact hop is what makes the
  # difference between "warm" and "silently cold" legible in the log.
  cache="test/resources/fixtures/enrichment-$code/.enrichment-cache"
  remembered=$( { find "$cache" -name '*.entry' 2>/dev/null || true; } | wc -l )
  if [ "$remembered" -gt 0 ]; then
    echo "remembered enrichment answers restored: $remembered"
  else
    echo "NO remembered-answer cache in the artifact — this leg re-asks every"
    echo "unresolved film's rating slugs live, and publishes the cache it builds."
  fi
}

command="${1:-}"
[ "$#" -gt 0 ] && shift
case "$command" in
  pinned) [ "$#" -eq 1 ] || { echo "usage: $0 pinned <code>" >&2; exit 64; }; pinned "$@" ;;
  pair)   [ "$#" -ge 2 ] || { echo "usage: $0 pair <code> <mode> [given pair] [corpus run]" >&2; exit 64; }; pair "$@" ;;
  corpus) [ "$#" -eq 1 ] || { echo "usage: $0 corpus <code>" >&2; exit 64; }; corpus "$@" ;;
  unpack) [ "$#" -eq 2 ] || { echo "usage: $0 unpack <code> <mode>" >&2; exit 64; }; unpack "$@" ;;
  *) echo "usage: $0 <pinned|pair|corpus|unpack> ..." >&2; exit 64 ;;
esac
