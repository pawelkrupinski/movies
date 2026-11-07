#!/usr/bin/env bash
# recording-held.sh: a recording is held where identity-fetch-recording.sh would find its corpus -- the archive release
# (a Forgejo-era number such as 573 or 528, whose run this forge never had) or its run's scrape-fixtures-* artifacts.
# gh is a stub answering from files under $STUB.
# Run: bash scripts/ci/recording-held-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m recording-held.sh\n'

stub="$(mktemp -d)"
trap 'rm -rf "$stub"' EXIT
mkdir -p "$stub/bin" "$stub/runs"
cat >"$stub/bin/gh" <<'GH'
#!/usr/bin/env bash
case "$1 $2" in
    "release view") [ -f "$STUB/release-fails" ] && { echo "HTTP 502" >&2; exit 1; }; cat "$STUB/assets" ;;
    "api "*)
        run=$(echo "$2" | sed -E 's|.*/runs/([0-9]+)/artifacts.*|\1|')
        [ -f "$STUB/api-fails" ] && { echo "HTTP 503" >&2; exit 1; }
        [ -f "$STUB/runs/$run" ] || { echo "gh: Not Found (HTTP 404)" >&2; exit 1; }
        cat "$STUB/runs/$run" ;;
esac
GH
chmod +x "$stub/bin/gh"

# run <run> -> "<exit status>", the output in $stub/out
run() {
    env PATH="$stub/bin:/usr/bin:/bin" STUB="$stub" GH_REPO=o/r \
        bash "$REPO_ROOT/scripts/ci/recording-held.sh" "$@" >"$stub/out" 2>&1
    echo $?
}
said() { grep -c -- "$1" "$stub/out"; }

printf '%s\n' enrichment-de-573.tar.zst scrapes-de-573.tar.zst scrapes-uk-573.tar.zst enrichment-pl-528.tar.zst \
    scrapes-pl-528.tar.gz >"$stub/assets"
printf '%s\n' scrape-fixtures-pl scrape-fixtures-de >"$stub/runs/42157268413"
printf '%s\n' recording-health >"$stub/runs/42157268414"

check "a Forgejo-era recording the archive release holds is held" "0" "$(run 573)"
check "so is one the archive holds packed by gzip" "0" "$(run 528)"
check "a run whose artifacts hold a corpus is held" "0" "$(run 42157268413)"
check "a run whose artifacts hold no corpus is not" "1" "$(run 42157268414)"
check "and says so" "1" "$(said 'holds no scrape-fixtures-\* corpus')"
check "a recording neither the archive nor this forge holds is not" "3" "$(run 999)"
check "and says where it looked" "1" "$(said 'neither the archive release identity-recordings nor this forge holds recording 999')"
touch "$stub/api-fails"
check "an artifact listing that fails is no evidence: unchecked, passed" "0" "$(run 42157268413)"
check "and warned" "1" "$(said '::warning::could not list recording 42157268413')"
rm -f "$stub/api-fails"; touch "$stub/release-fails"
check "an archive listing that fails falls back to the run's artifacts" "0" "$(run 42157268413)"

spec_summary
