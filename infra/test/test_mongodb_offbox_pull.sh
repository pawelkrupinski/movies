#!/usr/bin/env bash
# Cases for nix/files/mongodb-offbox-pull.sh -- the off-box copy of mongo-1's nightly dump, pulled
# by monitoring-1 (roles/mongodb-offbox-backup.nix).
#
# ITS OTHER TEST IS "LOSE mongo-1 AND TRY TO RESTORE". A pull that copies a half-written dump, a
# rotation that deletes the last good copy, or a metric that reports a fresh copy that is not there
# all look exactly like a working backup until the day it is needed. So the script runs here
# against directories this test controls, with a local directory standing in for mongo-1 (rsync
# treats a plain path and `user@host:` alike; only the transport differs).
#
# Run: bash infra/test/test_mongodb_offbox_pull.sh   (also run by bin/check)
#
# shellcheck disable=SC2016 # the greps of roles/mongodb.nix match its `$partial` and `${pkgs...}` literally
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"
script="$infra/nix/files/mongodb-offbox-pull.sh"

pass=0
fail=0

check() {
  local name="$1" expected="$2" actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    printf '  ok   %s\n' "$name"
    pass=$((pass + 1))
  else
    printf '  FAIL %s -- expected [%s], got [%s]\n' "$name" "$expected" "$actual"
    fail=$((fail + 1))
  fi
}

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# A dump as mongo-1 writes it: a stamp directory, one per database, gzipped BSON and an oplog.
make_dump() {
  local dir="$1/$2"
  mkdir -p "$dir/kinowo" "$dir/admin"
  printf 'movies-%s' "$2" >"$dir/kinowo/movies.bson.gz"
  printf 'users' >"$dir/admin/system.users.bson.gz"
  printf 'oplog' >"$dir/oplog.bson"
}

fresh() {
  rm -rf "$work/source" "$work/copies" "$work/metrics.prom"
  mkdir -p "$work/source" "$work/copies"
}

run() { SOURCE="$work/source" DESTINATION="$work/copies" KEEP="${KEEP:-3}" METRICS_FILE="$work/metrics.prom" bash "$script" "$@"; }

listing() { (shopt -s dotglob nullglob; cd "$work/copies" && names=(*) && printf '%s\n' "${names[@]}" | sort | tr '\n' ' ' | sed 's/ $//'); }

metric() { awk -v name="$1" '$1 == name { print $2 }' "$work/metrics.prom"; }

[[ -f "$script" ]] || { echo "  FAIL $script is missing"; echo "0 passed, 1 failed"; exit 1; }

echo "pull: the newest completed dump, and nothing in flight"
fresh
make_dump "$work/source" 20261022T032217Z
make_dump "$work/source" 20261023T033747Z
make_dump "$work/source" .partial-20261024T032039Z # tonight's, still being written
mkdir -p "$work/source/lost+found"
touch "$work/source/20261099T000000Z" # a FILE named like a stamp is not a dump
run pull >/dev/null
check "copies only the newest completed dump" "20261023T033747Z" "$(listing)"
check "copies its contents" "movies-20261023T033747Z" "$(cat "$work/copies/20261023T033747Z/kinowo/movies.bson.gz")"
check "copies the oplog beside it" "oplog" "$(cat "$work/copies/20261023T033747Z/oplog.bson")"
check "the copy is private to the puller" "700" "$(stat -c %a "$work/copies/20261023T033747Z" 2>/dev/null || stat -f %Lp "$work/copies/20261023T033747Z")"

echo "pull: idempotent once the newest is held"
before="$(cat "$work/copies/20261023T033747Z/kinowo/movies.bson.gz")"
printf 'changed-at-source' >"$work/source/20261023T033747Z/kinowo/movies.bson.gz"
out="$(run pull)"
check "says it already holds it" "already holding 20261023T033747Z" "$out"
check "does not re-copy a held dump" "$before" "$(cat "$work/copies/20261023T033747Z/kinowo/movies.bson.gz")"

echo "pull: tonight's dump completes"
mv "$work/source/.partial-20261024T032039Z" "$work/source/20261024T032039Z"
run pull >/dev/null
check "adds the new dump" "20261023T033747Z 20261024T032039Z" "$(listing)"

echo "pull: a pull cut off half-way is cleared, never counted"
mkdir -p "$work/copies/.partial-20261025T032000Z/kinowo"
make_dump "$work/source" 20261025T032000Z
run pull >/dev/null
check "leaves no partial behind" "20261023T033747Z 20261024T032039Z 20261025T032000Z" "$(listing)"

echo "pull: rotation by count"
fresh
for stamp in 20261020T030000Z 20261021T030000Z 20261022T030000Z 20261023T030000Z; do
  mkdir -p "$work/copies/$stamp"
done
mkdir -p "$work/copies/keep-me" # not a copy: never rotated
make_dump "$work/source" 20261024T030000Z
KEEP=3 run pull >/dev/null
check "keeps the newest KEEP copies" "20261022T030000Z 20261023T030000Z 20261024T030000Z keep-me" "$(listing)"

echo "pull: rotation cannot empty the copies when the source has nothing new"
fresh
mkdir -p "$work/copies/20261020T030000Z"
run pull >/dev/null 2>&1
check "an empty source fails" "1" "$?"
check "and the held copy survives" "20261020T030000Z" "$(listing)"
check "and publishes nothing (pull failed, metrics never runs)" "absent" "$([[ -e "$work/metrics.prom" ]] && echo present || echo absent)"

echo "pull: an unreachable source fails as itself"
fresh
mkdir -p "$work/copies/20261020T030000Z"
out="$(SOURCE="$work/no-such-host" DESTINATION="$work/copies" KEEP=3 bash "$script" pull 2>&1)"
status=$?
check "fails" "1" "$((status != 0 ? 1 : 0))"
check "not as 'no completed dump'" "0" "$(grep -c 'no completed dump' <<<"$out")"
check "keeps the held copy" "20261020T030000Z" "$(listing)"

echo "metrics"
fresh
make_dump "$work/copies" 20261023T033747Z
make_dump "$work/copies" 20261024T032039Z
mkdir -p "$work/copies/.partial-20261025T032000Z"
run metrics
check "the newest copy's dump time, from its stamp" \
  "$(python3 -c 'import calendar,time; print(calendar.timegm(time.strptime("20261024T032039Z","%Y%m%dT%H%M%SZ")))')" \
  "$(metric kinowo_mongodump_offbox_newest_dump_timestamp_seconds)"
check "the stamp arithmetic across a leap day" \
  "$(python3 -c 'import calendar,time; print(calendar.timegm(time.strptime("20280229T235959Z","%Y%m%dT%H%M%SZ")))')" \
  "$(mkdir -p "$work/leap/20280229T235959Z" && SOURCE=x DESTINATION="$work/leap" METRICS_FILE="$work/leap.prom" bash "$script" metrics && awk '$1 == "kinowo_mongodump_offbox_newest_dump_timestamp_seconds" { print $2 }' "$work/leap.prom")"
now="$(date +%s)"
success="$(metric kinowo_mongodump_offbox_last_success_timestamp_seconds)"
check "the pull's own success time is now" "1" "$(((now - success) >= 0 && (now - success) < 60 ? 1 : 0))"
check "counts the copies, not the partial" "2" "$(metric kinowo_mongodump_offbox_copies)"
size="$(metric kinowo_mongodump_offbox_last_size_bytes)"
check "publishes a non-zero size" "1" "$((size > 0 ? 1 : 0))"
check "every series is typed (GrafanaMetricCoverageSpec reads these)" "4" "$(grep -c '^# TYPE kinowo_mongodump_offbox_.* gauge$' "$work/metrics.prom")"
check "leaves no temporary file" "0" "$(shopt -s nullglob; leftovers=("$work"/metrics.prom.*); echo "${#leftovers[@]}")"

echo "metrics: nothing held"
fresh
run metrics >/dev/null 2>&1
check "fails" "1" "$?"
check "and writes nothing" "absent" "$([[ -e "$work/metrics.prom" ]] && echo present || echo absent)"

# THROUGH THE REAL FORCED COMMAND. On mongo-1 every request arrives through `rrsync -ro`, which
# rewrites and polices each path -- and refuses some spellings a plain rsync takes (a bare `/` or
# `./` is an "unsafe arg"). So the pull runs once more with an `ssh` that, like sshd with a
# `command=` key, hands the client's request to rrsync as SSH_ORIGINAL_COMMAND, and a write is
# shown to be refused. rrsync comes from the fleet's pinned nixpkgs -- the one mongo-1 runs, with
# its own rsync -- and from PATH only where there is no nix. NOT PATH FIRST: Homebrew's rrsync
# execs macOS's /usr/bin/rsync (openrsync), which rejects rrsync's own `--confine-root`.
echo "through mongo-1's forced command (rrsync -ro)"
rrsync_bin=""
if command -v nix >/dev/null 2>&1; then
  # The pinned revision read straight out of flake.lock rather than `--inputs-from "$infra"`, which
  # evaluates the checkout as a git flake and so refuses a shallow clone ("'revCount' is not
  # available") -- the state of every worktree of this repository and of a CI checkout.
  nixpkgs_rev="$(python3 -c 'import json,sys; l=json.load(open(sys.argv[1])); print(l["nodes"][l["nodes"]["root"]["inputs"]["nixpkgs"]]["locked"]["rev"])' "$infra/flake.lock")"
  rrsync_pkg="$(nix --extra-experimental-features 'nix-command flakes' \
    build --no-link --print-out-paths "github:NixOS/nixpkgs/$nixpkgs_rev#rrsync" 2>/dev/null)"
  [[ -x "$rrsync_pkg/bin/rrsync" ]] && rrsync_bin="$rrsync_pkg/bin/rrsync"
else
  rrsync_bin="$(command -v rrsync || true)"
fi
if [[ -z "$rrsync_bin" ]]; then
  check "rrsync is available (PATH or the pinned nixpkgs)" "found" "missing"
else
  fresh
  make_dump "$work/source" 20261023T033747Z
  make_dump "$work/source" 20261024T032039Z
  make_dump "$work/source" .partial-20261025T032000Z
  cat >"$work/forced-ssh" <<FORCED
#!/usr/bin/env bash
# ssh [options] host command... -> rrsync, as sshd runs a command= key.
while [[ "\$1" == -* ]]; do case "\$1" in -l|-i|-o|-p) shift ;; esac; shift; done
shift
export SSH_ORIGINAL_COMMAND="\$*"
exec "$rrsync_bin" -ro "$work/source"
FORCED
  chmod +x "$work/forced-ssh"
  SOURCE="mongodb-offbox@mongo-1:" DESTINATION="$work/copies" KEEP=3 RSYNC_RSH="$work/forced-ssh" \
    bash "$script" pull >/dev/null 2>"$work/rrsync.err" || sed 's/^/       /' "$work/rrsync.err"
  check "pulls the newest completed dump through rrsync" "20261024T032039Z" "$(listing)"
  check "with its contents" "movies-20261024T032039Z" "$(cat "$work/copies/20261024T032039Z/kinowo/movies.bson.gz" 2>/dev/null)"
  printf 'evil' >"$work/evil"
  rsync -e "$work/forced-ssh" "$work/evil" "mongodb-offbox@mongo-1:/evil" >/dev/null 2>&1
  check "a write through the key is refused" "absent" "$([[ -e "$work/source/evil" ]] && echo present || echo absent)"
  rsync -e "$work/forced-ssh" -r --delete "$work/copies/" "mongodb-offbox@mongo-1:/" >/dev/null 2>&1
  check "a delete through the key is refused" "kept" "$([[ -d "$work/source/20261023T033747Z" ]] && echo kept || echo deleted)"
fi

# THE PRODUCER'S HALF OF THE CONTRACT, which this script cannot see: mongo-1 must write each dump
# under `.partial-` and rename it only on success, or "the newest stamp directory" above is
# sometimes a dump still being written. And the puller's key must be confined READ-ONLY to the dump
# directory, or a compromised puller could reach mongo-1's copy -- the property the pull exists for.
echo "mongo-1's side of the contract (roles/mongodb.nix)"
role="$infra/nix/modules/roles/mongodb.nix"
check "mongodump writes to a .partial- directory" "1" "$(grep -cF -- '--out="$partial"' "$role")"
check "and renames it into place after mongodump exits" "1" "$(grep -cF 'mv "$partial" "$target"' "$role")"
check "the puller's key runs rrsync read-only, restricted" "1" \
  "$(grep -cF 'command="${pkgs.rrsync}/bin/rrsync -ro ${cfg.backup.directory}",restrict' "$role")"

echo
echo "$pass passed, $fail failed"
((fail == 0))
