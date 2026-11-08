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

# The fallbacks: a `go` whose install drops an actionlint reporting STUB_GO_VERSION into GOBIN, and a `nix` whose
# build prints a store path holding one reporting STUB_NIX_VERSION. Each logs its arguments.
cat > "$work/bin/stub-go" <<'STUB'
#!/usr/bin/env bash
printf '%s GOPROXY=%s GOSUMDB=%s\n' "$*" "$GOPROXY" "$GOSUMDB" >> "$STUB_LOG.go"
printf '#!/usr/bin/env bash\necho %s\n' "$STUB_GO_VERSION" > "$GOBIN/actionlint"
chmod +x "$GOBIN/actionlint"
STUB
cat > "$work/bin/stub-nix" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG.nix"
mkdir -p "$STUB_STORE/bin"
printf '#!/usr/bin/env bash\necho %s\n' "$STUB_NIX_VERSION" > "$STUB_STORE/bin/actionlint"
chmod +x "$STUB_STORE/bin/actionlint"
echo "$STUB_STORE"
STUB
chmod +x "$work/bin/stub-go" "$work/bin/stub-nix"

# install <dir> [sha]  -> runs the script, prints "<exit status>"; its output lands in $work/out, its
# GITHUB_OUTPUT in $work/github-output. The fallbacks are absent unless ACTIONLINT_GO / ACTIONLINT_NIX name a stub.
install() {
  rm -f "$work/curls" "$work/curls.go" "$work/curls.nix" "$work/github-output"
  PATH="$work/bin:$PATH" STUB_LOG="$work/curls" STUB_TARBALL="${STUB_TARBALL:-$work/good.tar.gz}" \
    STUB_STORE="$work/store" GITHUB_OUTPUT="$work/github-output" \
    ACTIONLINT_GO="${ACTIONLINT_GO:-no-such-go}" ACTIONLINT_NIX="${ACTIONLINT_NIX:-no-such-nix}" \
    ACTIONLINT_VERSION=1.7.12 ACTIONLINT_SHA256="${2:-$good_sha}" \
    bash "$REPO_ROOT/scripts/ci/install-actionlint.sh" "$1" > "$work/out" 2>&1
  echo $?
}

check "a cold cache downloads, verifies and installs" "0" "$(install "$work/cold")"
check "...and says the release is the source, the one the action caches" "source=release" "$(cat "$work/github-output")"
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
check "a download that keeps failing, with no go or nix, fails the install" "1" "$(STUB_FAIL=1 install "$work/down")"

printf '\033[36m▸\033[0m a release CDN that keeps failing: the non-GitHub fallbacks\n'
check "go builds the pinned version through the Go module proxy" "0" \
  "$(STUB_FAIL=1 ACTIONLINT_GO=stub-go STUB_GO_VERSION=1.7.12 install "$work/go")"
check "...the pinned module version, the proxy and the checksum database" \
  "install github.com/rhysd/actionlint/cmd/actionlint@v1.7.12 GOPROXY=https://proxy.golang.org GOSUMDB=sum.golang.org" \
  "$(cat "$work/curls.go")"
check "...the binary is installed" "1.7.12" "$("$work/go/actionlint")"
check "...as source=go, which the action never caches" "source=go" "$(cat "$work/github-output")"
check "...and no tarball is left for a cache to keep" "absent" \
  "$( [ -e "$work/go/actionlint_1.7.12_linux_amd64.tar.gz" ] && echo present || echo absent)"
check "a go build of another version fails the install" "1" \
  "$(STUB_FAIL=1 ACTIONLINT_GO=stub-go STUB_GO_VERSION=1.7.11 install "$work/go-wrong")"
check "...and removes that binary" "absent" "$( [ -e "$work/go-wrong/actionlint" ] && echo present || echo absent)"
check "with no go, nix builds the nixos-26.05 channel's actionlint (not the registry's github: nixpkgs)" "0" \
  "$(STUB_FAIL=1 ACTIONLINT_NIX=stub-nix STUB_NIX_VERSION=1.7.12 install "$work/nix")"
check "...from that channel" "1" "$(grep -c 'build --no-link --print-out-paths nixpkgs/nixos-26.05#actionlint' "$work/curls.nix")"
check "...as source=nix" "source=nix" "$(cat "$work/github-output")"
check "a channel that moved past the pin fails the install" "1" \
  "$(STUB_FAIL=1 ACTIONLINT_NIX=stub-nix STUB_NIX_VERSION=1.7.13 install "$work/nix-moved")"
check "a cache hit never reaches a fallback" "0" "$(ACTIONLINT_GO=stub-go STUB_GO_VERSION=1.7.12 install "$work/cold")"
check "...go never ran" "absent" "$( [ -e "$work/curls.go" ] && echo present || echo absent)"

printf '\033[36m▸\033[0m every actionlint install goes through .github/actions/install-actionlint\n'
inline=$(grep -rlE 'rhysd/actionlint/releases|ACTIONLINT_SHA256' "$REPO_ROOT/.github" | sed "s|$REPO_ROOT/||" \
  | grep -v '^.github/actions/install-actionlint/action.yml$' || true)
check "no workflow or other action downloads actionlint itself" "" "$inline"

spec_summary
