#!/usr/bin/env bash
# generated-artefacts-drift.sh in a scratch repository whose generators are stand-ins: drift is what they REWRITE, not
# what differs from HEAD -- a tracked file rewritten or an untracked one made fails, naming it; an edit already in the
# tree before they ran does not. GeoNames' gazetteer is taken from GEONAMES_CACHE when it holds one (no download), else
# downloaded (a stub `curl`/`unzip`), present while the generators run and removed after.
# Run: bash scripts/ci/generated-artefacts-drift-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# the scratch repository is built only through the isolating helper: run from a hook, a bare git would act on the
# repository being pushed
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m generated-artefacts-drift.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# curl -o <zip> writes the archive; unzip -d <dir> lays PL.txt out of it
cat > "$work/bin/curl" <<'STUB'
#!/usr/bin/env bash
echo "curl $*" >> "$STUB/calls"
while [ "$#" -gt 0 ]; do [ "$1" = -o ] && echo zip > "$2"; shift; done
STUB
cat > "$work/bin/unzip" <<'STUB'
#!/usr/bin/env bash
echo "unzip $*" >> "$STUB/calls"
while [ "$#" -gt 0 ]; do [ "$1" = -d ] && echo "downloaded gazetteer" > "$2/PL.txt"; shift; done
STUB
chmod +x "$work/bin/"*

# Every generator the gate runs, read from the gate itself: a stand-in that logs it ran, with the gazetteer it saw, and
# runs $HOOK_CMD when it is $HOOK_SCRIPT.
generators=$(sed -n 's/^python3 //p' "$REPO_ROOT/scripts/ci/generated-artefacts-drift.sh")
# repo_for <case dir> -> a fresh scratch repository holding the gate, every generator and one generated artefact
repo_for() {
  local repo=$1/repo g
  mkdir -p "$repo/scripts/ci" "$repo/data/pl"
  cp "$REPO_ROOT/scripts/ci/generated-artefacts-drift.sh" "$repo/scripts/ci/"
  for g in $generators; do
    mkdir -p "$repo/$(dirname "$g")"
    cat > "$repo/$g" <<'PY'
import os, subprocess, sys
here = sys.argv[0]
with open(os.environ["STUB"] + "/ran", "a") as log:
    log.write("%s %s\n" % (here, "gazetteer" if os.path.isfile("data/pl/geonames/PL.txt") else "no-gazetteer"))
if os.environ.get("HOOK_SCRIPT") == here:
    subprocess.check_call(["bash", "-c", os.environ["HOOK_CMD"]])
PY
  done
  echo '{"venues": 1}' > "$repo/data/pl/venues.json"
  echo 'notes' > "$repo/README.md"
  echo 'data/pl/geonames/' > "$repo/.gitignore"
  scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm generated
}

# drift <case dir> [env assignments...] -> the gate's exit status, run from inside the repository; output in <case>/out
drift() {
  local dir=$1; shift
  (cd "$dir/repo" && env STUB="$dir" PATH="$work/bin:$PATH" "$@" bash scripts/ci/generated-artefacts-drift.sh > "$dir/out" 2>&1)
}

c="$work/clean"; mkdir -p "$c"; repo_for "$c"
drift "$c"
check "generators that change nothing: passes" "0" "$?"
check "...every generator ran" "$(printf '%s\n' "$generators" | wc -l | tr -d ' ')" "$(wc -l < "$c/ran" | tr -d ' ')"
check "...the gazetteer downloaded" "1" "$(grep -c '^curl .*geonames.org/export/dump/PL.zip' "$c/calls")"
check "...present while they ran" "0" "$(grep -c 'no-gazetteer' "$c/ran")"
check "...and removed after" "absent" "$([ -e "$c/repo/data/pl/geonames" ] && echo present || echo absent)"
check "...the tree as it was" "" "$(scratch_git "$c/repo" status --porcelain)"

c="$work/rewrites"; mkdir -p "$c"; repo_for "$c"
drift "$c" HOOK_SCRIPT=data/pl/scripts/build_venue_towns.py HOOK_CMD='echo "{\"venues\": 2}" > data/pl/venues.json'
check "a generator rewriting a tracked artefact: fails" "1" "$?"
check "...naming it" "1" "$(grep -cx 'data/pl/venues.json' "$c/out")"
check "...and the file it did not touch is not named" "0" "$(grep -c 'README.md' "$c/out")"

c="$work/dirty"; mkdir -p "$c"; repo_for "$c"
echo 'an unrelated edit' >> "$c/repo/README.md"; echo 'scratch' > "$c/repo/untracked-note.txt"
drift "$c"
check "edits already in the tree before the generators ran: passes" "0" "$?"
check "...and they are left as they were" "an unrelated edit" "$(tail -1 "$c/repo/README.md")"

c="$work/dirty-and-drifted"; mkdir -p "$c"; repo_for "$c"
echo '{"venues": 9}' > "$c/repo/data/pl/venues.json"
drift "$c" HOOK_SCRIPT=data/pl/scripts/build_venue_towns.py HOOK_CMD='echo "{\"venues\": 2}" > data/pl/venues.json'
check "a generator rewriting an already-edited artefact again: fails" "1" "$?"
check "...naming it" "1" "$(grep -cx 'data/pl/venues.json' "$c/out")"

c="$work/new-file"; mkdir -p "$c"; repo_for "$c"
drift "$c" HOOK_SCRIPT=data/scripts/generate_slug_index.py HOOK_CMD='echo new > data/new-index.json'
check "a generator making an untracked file: fails" "1" "$?"
check "...naming it" "1" "$(grep -cx 'data/new-index.json' "$c/out")"

c="$work/cached"; mkdir -p "$c/cache"; repo_for "$c"; echo "cached gazetteer" > "$c/cache/PL.txt"
drift "$c" GEONAMES_CACHE="$c/cache"
check "a GEONAMES_CACHE holding the gazetteer: passes" "0" "$?"
check "...without downloading it" "0" "$(grep -c '^curl' "$c/calls" 2>/dev/null || echo 0)"
check "...the cached copy present while they ran" "0" "$(grep -c 'no-gazetteer' "$c/ran")"
check "...and removed from the tree after" "absent" "$([ -e "$c/repo/data/pl/geonames" ] && echo present || echo absent)"

c="$work/fills-cache"; mkdir -p "$c"; repo_for "$c"
drift "$c" GEONAMES_CACHE="$c/cache"
check "an empty GEONAMES_CACHE: downloads once" "1" "$(grep -c '^curl' "$c/calls")"
check "...and keeps the gazetteer there for the next run" "downloaded gazetteer" "$(cat "$c/cache/PL.txt" 2>/dev/null)"

c="$work/fails"; mkdir -p "$c"; repo_for "$c"
drift "$c" HOOK_SCRIPT=data/us/scripts/build_pages.py HOOK_CMD='exit 3'
check "a generator that fails: the gate fails" "1" "$?"
check "...and runs none after it" "0" "$(grep -c 'data/scripts/generate_slug_index.py' "$c/ran")"
check "...still removing the gazetteer it fetched" "absent" "$([ -e "$c/repo/data/pl/geonames" ] && echo present || echo absent)"

spec_summary
