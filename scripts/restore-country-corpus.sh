#!/usr/bin/env bash
#
# Restore the recorded PAIR a hermetic convergence leg replays — a country's scrape corpus and
# the enrichment tree recorded beside it — into this checkout, exactly as CI's
# `.github/actions/convergence-setup` does.
#
#   scripts/restore-country-corpus.sh <pl|de|uk|es|us|all>
#
# Where CI keeps them (none of it is in git, bar two stale July corpora — see below):
#   - `hermetic-<code>.txt` in the rolling `convergence-fixtures` release pins the pair:
#     `<recording run id> <recorded at> <tree asset>`.
#   - the corpus is the `scrape-fixtures-<code>` ARTIFACT of that `Record scrape fixtures` run
#     (14-day retention): `scrapes-<code>.tar.gz`, holding
#     test/resources/fixtures/corpus/{cinema-scrapes-<code>.json.gz, prod-coverage-<code>.json,
#     cinema-scrapes-<code>-sample.json.gz, prod-coverage-<code>-sample.json}.
#   - the tree is that release asset (`enrichment-<code>-<run>.tar.zst`, ~40-320 MB), unpacked to
#     test/resources/fixtures/enrichment-<code>, with every FILL later legs published over it laid
#     on top (`convergence-fill.sh fills`), as `restore-enrichment-tree.sh` does in CI.
#
# It fails loudly — never falls back to anything — when no pair is pinned, the pinned corpus
# has expired, or the tree is gone: a run over anything else is not the experiment CI ran.
# Record a fresh pair with `gh workflow run "Record scrape fixtures" --ref main` (~10 min).
#
# The repo still TRACKS a July Polish and German corpus (cinema-scrapes-{pl,de}.json.gz,
# prod-coverage-{pl,de}.json); restoring overwrites them, which `git checkout --` undoes. CI
# overwrites them the same way on every leg.
#
# FIXTURE_RELEASE_TAG (default convergence-fixtures) and RESTORE_ROOT (default this checkout)
# are overridable for the test, scripts/restore-country-corpus-test.sh.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/.." && pwd)"
root="${RESTORE_ROOT:-$repo}"
export FIXTURE_RELEASE_TAG="${FIXTURE_RELEASE_TAG:-convergence-fixtures}"
countries=(pl de uk es us)

fail() { echo "restore-country-corpus: $*" >&2; exit 1; }

restore() {
  local code="$1" work marker present corpus recorded tree fills archive stage
  work="$(mktemp -d)"
  # shellcheck disable=SC2064 # expand now: `work` is local to this call
  trap "rm -rf '$work'" RETURN

  marker="hermetic-$code.txt"
  present=$("$repo/scripts/ci/gh-optional.sh" release download "$FIXTURE_RELEASE_TAG" --pattern "$marker" --dir "$work" --clobber) ||
    fail "could not read release $FIXTURE_RELEASE_TAG for $marker (gh auth?)"
  [ "$present" = present ] ||
    fail "no recorded pair pinned for $code ($marker in release $FIXTURE_RELEASE_TAG). Record one: gh workflow run \"Record scrape fixtures\" --ref main"
  read -r corpus recorded tree _ < "$work/$marker"
  [ -n "${tree:-}" ] || fail "$marker is malformed: '$(head -1 "$work/$marker")'"
  echo "==> $code: corpus from recording run $corpus ($recorded), tree $tree"

  present=$("$repo/scripts/ci/gh-optional.sh" run download "$corpus" --name "scrape-fixtures-$code" --dir "$work/corpus") ||
    fail "could not download scrape-fixtures-$code from run $corpus"
  archive="$work/corpus/scrapes-$code.tar.gz"
  [ "$present" = present ] && [ -f "$archive" ] ||
    fail "recording run $corpus's scrape-fixtures-$code artifact is gone (14-day retention). Record one: gh workflow run \"Record scrape fixtures\" --ref main"
  "$repo/.github/scripts/unpack-fixture-archive.sh" "$archive" "$root"
  [ -f "$root/test/resources/fixtures/corpus/cinema-scrapes-$code.json.gz" ] ||
    fail "$archive held no test/resources/fixtures/corpus/cinema-scrapes-$code.json.gz"
  echo "    corpus: $(du -h "$root/test/resources/fixtures/corpus/cinema-scrapes-$code.json.gz" | cut -f1) test/resources/fixtures/corpus/cinema-scrapes-$code.json.gz"

  fills=$(cd "$repo" && "$repo/.github/scripts/convergence-fill.sh" fills "$code" "$corpus") ||
    fail "could not list the fills published over $tree"
  stage="$work/tree"
  (cd "$repo" && KINOWO_CONVERGENCE_TREE_ASSET="$tree" KINOWO_CONVERGENCE_FILL_ASSETS="$fills" \
     "$repo/.github/scripts/restore-enrichment-tree.sh" "$code" hermetic "$stage") ||
    fail "could not restore the pinned tree $tree${fills:+ and its fills $fills}"
  [ -d "$stage/test/resources/fixtures/enrichment-$code" ] || fail "$tree held no enrichment-$code tree"
  # The tree REPLACES the local one, never merges into it: what a local run fetched live is not
  # in the pair, and a hermetic replay must not find it.
  rm -rf "$root/test/resources/fixtures/enrichment-$code"
  mkdir -p "$root/test/resources/fixtures"
  local entry
  for entry in "$stage/test/resources/fixtures"/* "$stage/test/resources/fixtures"/.[!.]*; do
    [ -e "$entry" ] || continue
    cp -a "$entry" "$root/test/resources/fixtures/"
  done
  echo "    tree:   $(find "$root/test/resources/fixtures/enrichment-$code" -type f | wc -l | tr -d ' ') files in test/resources/fixtures/enrichment-$code${fills:+ (fills: $fills)}"
}

case "${1:-}" in
  all) for code in "${countries[@]}"; do restore "$code"; done ;;
  pl|de|uk|es|us) restore "$1" ;;
  *) echo "usage: $0 <pl|de|uk|es|us|all>" >&2; exit 64 ;;
esac
