#!/usr/bin/env bash
# prune-actions-caches.sh against a stub `gh` that answers `cache list` with canned JSON and
# records every `cache delete`.
# Run: bash scripts/ci/prune-actions-caches-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m prune-actions-caches.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/gh" <<'STUB'
#!/usr/bin/env bash
case "$1 $2" in
  "cache list")   printf '%s\n' "$*" >> "$STUB_LOG.list"; cat "$STUB_JSON" ;;
  "cache delete") printf '%s\n' "$3" >> "$STUB_LOG" ;;
  *) echo "unexpected gh $*" >&2; exit 1 ;;
esac
STUB
chmod +x "$stub_dir/gh"
export STUB_LOG="$stub_dir/deleted" STUB_JSON="$stub_dir/caches.json"

sha() { printf '%064d' "$1"; }   # a 64-digit stand-in for a hashFiles() suffix
git_sha() { printf '%040d' "$1"; }

cat > "$STUB_JSON" <<JSON
[
  {"id": 1, "key": "sbt-target-e2e-Linux-$(sha 1)", "ref": "refs/heads/main", "sizeInBytes": 300000000, "createdAt": "2026-10-01T08:00:00Z"},
  {"id": 2, "key": "sbt-target-e2e-Linux-$(sha 2)", "ref": "refs/heads/main", "sizeInBytes": 300000000, "createdAt": "2026-10-02T08:00:00Z"},
  {"id": 3, "key": "sbt-target-e2e-Linux-$(sha 3)", "ref": "refs/heads/main", "sizeInBytes": 300000000, "createdAt": "2026-10-01T12:00:00Z"},
  {"id": 4, "key": "sbt-target-Linux-$(sha 4)", "ref": "refs/heads/main", "sizeInBytes": 50000000, "createdAt": "2026-10-01T09:00:00Z"},
  {"id": 5, "key": "sbt-target-Linux-$(sha 5)", "ref": "refs/heads/main", "sizeInBytes": 50000000, "createdAt": "2026-10-01T10:00:00Z"},
  {"id": 6, "key": "gradle-home-v1|Linux-X64|build[751e3a27013e646c8eb8af27bfacc3ef]-$(git_sha 6)", "ref": "refs/heads/main", "sizeInBytes": 60000000, "createdAt": "2026-10-01T09:00:00Z"},
  {"id": 7, "key": "gradle-home-v1|Linux-X64|mobile-local-server[602cb68cbf9e3cca39dd11647e23ff86]-$(git_sha 7)", "ref": "refs/heads/main", "sizeInBytes": 7000000, "createdAt": "2026-10-01T09:00:00Z"},
  {"id": 8, "key": "gradle-home-v1|Linux-X64|mobile-local-server[602cb68cbf9e3cca39dd11647e23ff86]-$(git_sha 8)", "ref": "refs/heads/main", "sizeInBytes": 7000000, "createdAt": "2026-10-02T09:00:00Z"},
  {"id": 9, "key": "avd-34-google_apis-x86_64-pixel_6", "ref": "refs/heads/main", "sizeInBytes": 2400000000, "createdAt": "2026-09-01T09:00:00Z"},
  {"id": 10, "key": "gradle-dependencies-v1-18327c13c82b9e0a56c22d75d85a390a", "ref": "refs/heads/main", "sizeInBytes": 500000000, "createdAt": "2026-09-01T09:00:00Z"},
  {"id": 11, "key": "gradle-dependencies-v1-ea48cd94bbcad6bc9739ba5bba319b1f", "ref": "refs/heads/main", "sizeInBytes": 450000000, "createdAt": "2026-10-01T09:00:00Z"},
  {"id": 12, "key": "node-cache-Linux-x64-npm-$(sha 12)", "ref": "refs/heads/main", "sizeInBytes": 2000000, "createdAt": "2026-09-01T09:00:00Z"},
  {"id": 13, "key": "node-cache-Linux-x64-npm-$(sha 13)", "ref": "refs/heads/main", "sizeInBytes": 4000000, "createdAt": "2026-10-01T09:00:00Z"},
  {"id": 14, "key": "sbt-target-e2e-Linux-$(sha 14)", "ref": "refs/pull/7/merge", "sizeInBytes": 300000000, "createdAt": "2026-09-01T09:00:00Z"}
]
JSON

: > "$STUB_LOG"
out="$(PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/prune-actions-caches.sh" 2>&1)"; status=$?
deleted="$(sort -n "$STUB_LOG" | tr '\n' ' ')"

check "exits 0" "0" "$status"
check "deletes all but the newest of each per-commit family, and nothing else" "1 3 4 7 " "$deleted"
check "lists only main's caches" "cache list --ref refs/heads/main --limit 1000 --json id,key,ref,sizeInBytes,createdAt" \
  "$(head -1 "$STUB_LOG.list")"
check "reports the bytes it freed" "4 entries, freed 657 MB" "$(tail -1 <<< "$out")"

: > "$STUB_LOG"
out="$(PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/prune-actions-caches.sh" --dry-run 2>&1)"
check "--dry-run deletes nothing" "" "$(cat "$STUB_LOG")"
check "--dry-run names what it would delete" "4" "$(grep -c '^would delete' <<< "$out")"
check "--dry-run reports what it would free" "4 entries, would free 657 MB" "$(tail -1 <<< "$out")"

: > "$STUB_LOG"
PRUNE_EXCLUDE='^sbt-target-e2e-' PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/prune-actions-caches.sh" >/dev/null 2>&1
check "PRUNE_EXCLUDE replaces the default exclusion" "4 7 12 " "$(sort -n "$STUB_LOG" | tr '\n' ' ')"

PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/prune-actions-caches.sh" --bogus >/dev/null 2>&1
check "refuses an unknown argument" "2" "$?"

spec_summary
