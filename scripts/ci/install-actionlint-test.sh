#!/usr/bin/env bash
# install-actionlint.sh against a stub `curl` that logs its arguments and writes a fake release tarball (or fails on
# cue), and a structural check that every workflow and action installs actionlint only through
# .github/actions/install-actionlint.
# Run: bash scripts/ci/install-actionlint-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m install-actionlint.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
# The fake release: a tarball holding an `actionlint` that prints its name.
mkdir -p "$work/release"
printf '#!/usr/bin/env bash\necho fake-actionlint\n' > "$work/release/actionlint"
chmod +x "$work/release/actionlint"
tar -czf "$work/good.tar.gz" -C "$work/release" actionlint
good_sha=$(sha256sum "$work/good.tar.gz" | cut -d' ' -f1)
mkdir -p "$work/bin"
cat > "$work/bin/curl" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
[ -n "${STUB_FAIL:-}" ] && exit 22
out=""; while [ $# -gt 0 ]; do [ "$1" = "-o" ] && { out="$2"; shift; }; shift; done
cp "$STUB_TARBALL" "$out"
STUB
chmod +x "$work/bin/curl"

# install <dir> [sha]  -> runs the script, prints "<exit status>"; its output lands in $work/out
install() {
  rm -f "$work/curls"
  PATH="$work/bin:$PATH" STUB_LOG="$work/curls" STUB_TARBALL="${STUB_TARBALL:-$work/good.tar.gz}" \
    ACTIONLINT_VERSION=1.7.12 ACTIONLINT_SHA256="${2:-$good_sha}" \
    bash "$REPO_ROOT/scripts/ci/install-actionlint.sh" "$1" > "$work/out" 2>&1
  echo $?
}

check "a cold cache downloads, verifies and installs" "0" "$(install "$work/cold")"
check "...the installed binary runs" "fake-actionlint" "$("$work/cold/actionlint")"
check "...from the pinned release URL" "1" \
  "$(grep -c 'https://github.com/rhysd/actionlint/releases/download/v1.7.12/actionlint_1.7.12_linux_amd64.tar.gz' "$work/curls")"
check "...retrying every error class, not just curl's transient ones (a 504 is one)" "1" "$(grep -c -- '--retry-all-errors' "$work/curls")"
check "...with a retry budget of minutes, not seconds" "1" "$(grep -c -- '--retry 8 .*--retry-max-time 180' "$work/curls")"

rm -f "$work/cold/actionlint"
check "a warm cache installs with no download at all, even while GitHub's CDN fails" "0" "$(STUB_FAIL=1 install "$work/cold")"
check "...curl never ran" "0" "$( [ -f "$work/curls" ] && wc -l < "$work/curls" | tr -d ' ' || echo 0)"
check "...and the binary is there" "fake-actionlint" "$("$work/cold/actionlint")"

mkdir -p "$work/tampered"
echo 'not the release' > "$work/tampered/actionlint_1.7.12_linux_amd64.tar.gz"
check "a cached tarball that fails its checksum is downloaded again" "0" "$(install "$work/tampered")"
check "...once" "1" "$(wc -l < "$work/curls" | tr -d ' ')"

echo 'evil' > "$work/evil.tar.gz"
check "a download that fails its checksum fails the install" "1" "$(STUB_TARBALL="$work/evil.tar.gz" install "$work/evil")"
check "...and leaves nothing behind for the cache to keep" "absent" \
  "$( [ -e "$work/evil/actionlint_1.7.12_linux_amd64.tar.gz" ] || [ -e "$work/evil/actionlint" ] && echo present || echo absent)"
check "a download that keeps failing fails the install" "22" "$(STUB_FAIL=1 install "$work/down")"

printf '\033[36m▸\033[0m every actionlint install goes through .github/actions/install-actionlint\n'
inline=$(grep -rlE 'rhysd/actionlint/releases|ACTIONLINT_SHA256' "$REPO_ROOT/.github" | sed "s|$REPO_ROOT/||" \
  | grep -v '^.github/actions/install-actionlint/action.yml$' || true)
check "no workflow or other action downloads actionlint itself" "" "$inline"

spec_summary
