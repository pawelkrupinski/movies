#!/usr/bin/env bash
# ci-writes.sh: Forgejo and a laptop write; github.com writes only with CI_ROLE=primary, and unset is shadow. The
# script writers it gates skip their write in shadow. Run: bash scripts/ci/ci-writes-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m ci-writes.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
gate() { # <server url> <ci role> -> exit status
    env -u GITHUB_SERVER_URL -u CI_ROLE ${1:+GITHUB_SERVER_URL=$1} ${2:+CI_ROLE=$2} \
        bash "$REPO_ROOT/scripts/ci/ci-writes.sh"; echo $?
}

check "Forgejo writes, CI_ROLE unset" 0 "$(gate https://forgejo.example "")"
check "Forgejo writes whatever CI_ROLE says" 0 "$(gate https://forgejo.example shadow)"
check "a laptop (no server url) writes" 0 "$(gate "" "")"
check "github.com with CI_ROLE unset is shadow" 1 "$(gate https://github.com "")"
check "github.com with CI_ROLE=shadow is shadow" 1 "$(gate https://github.com shadow)"
check "github.com with CI_ROLE=primary writes" 0 "$(gate https://github.com primary)"

# The writers: a stub gh/docker/curl records every call; in shadow none of the write calls may happen.
mkdir -p "$work/bin"
for tool in gh docker curl; do
    printf '#!/usr/bin/env bash\necho "%s $*" >> "%s/calls"\n' "$tool" "$work" > "$work/bin/$tool"
    chmod +x "$work/bin/$tool"
done
shadow() { : > "$work/calls"; env -u CI_ROLE PATH="$work/bin:$PATH" GITHUB_SERVER_URL=https://github.com "$@" > /dev/null 2>&1; cat "$work/calls"; }
check "gh-release.sh: an upload is skipped in shadow" "" "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" upload t f --clobber)"
check "gh-release.sh: a delete-asset is skipped in shadow" "" "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" delete-asset t f -y)"
check "gh-release.sh: a view still reads in shadow" "gh release view t" "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" view t)"
# The one exception: a convergence leg's fixture data in convergence-fixtures (fills, refetch and refused lists).
check "gh-release.sh: a leg's fill is published in shadow" "gh release upload convergence-fixtures fill-upload/fill-de-573-41209071921-1-convergence.tar.zst" \
    "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" upload convergence-fixtures fill-upload/fill-de-573-41209071921-1-convergence.tar.zst)"
check "...and its refetch and refused lists" "2" \
    "$(shadow bash -c "bash '$REPO_ROOT/scripts/ci/gh-release.sh' upload convergence-fixtures refetch-upload/refetch-pl-573-41209071921-1.tsv; bash '$REPO_ROOT/scripts/ci/gh-release.sh' upload convergence-fixtures refused-uk-573-41209071921-1.tsv" | grep -c 'gh release upload')"
check "...and an old list pruned" "gh release delete-asset convergence-fixtures refused-uk-573-4433-1.tsv -y" \
    "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" delete-asset convergence-fixtures refused-uk-573-4433-1.tsv -y)"
for pointer in prod-latest-de.txt hermetic-de.txt corpus-green-de.txt pin-de.txt enrichment-de-573.tar.zst \
        takes-final-de-abc.gz recording-health-de.json fill-de-573-1-convergence.tar.zst "fill-de-573-41209071921-1-convergence.tar.zst.sh"; do
    check "gh-release.sh: $pointer stays unwritten in shadow" "" \
        "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" upload convergence-fixtures "$pointer" --clobber)"
done
check "gh-release.sh: fixture data with a pointer beside it stays unwritten" "" \
    "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" upload convergence-fixtures fill-de-573-41209071921-1-convergence.tar.zst prod-latest-de.txt)"
check "gh-release.sh: fixture data in another release stays unwritten" "" \
    "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" upload identity-recordings fill-de-573-41209071921-1-convergence.tar.zst)"
check "gh-release.sh: creating or editing the release stays unwritten" "" \
    "$(shadow bash -c "bash '$REPO_ROOT/scripts/ci/gh-release.sh' create convergence-fixtures; bash '$REPO_ROOT/scripts/ci/gh-release.sh' edit convergence-fixtures --notes x; bash '$REPO_ROOT/scripts/ci/gh-release.sh' delete convergence-fixtures -y")"
check "gh-release.sh: an upload naming no asset stays unwritten" "" \
    "$(shadow bash "$REPO_ROOT/scripts/ci/gh-release.sh" upload convergence-fixtures --clobber)"
check "scheduled-failure-issue.sh opens no issue in shadow" "" "$(shadow bash "$REPO_ROOT/scripts/ci/scheduled-failure-issue.sh" failed W https://x/run o)"
touch "$work/report"
check "bot-branch-ready.sh opens no issue in shadow" "" "$(shadow env GH_REPO=o/r bash "$REPO_ROOT/scripts/ci/bot-branch-ready.sh" b T "$work/report" https://x/run)"
check "publish-rolling-release.sh publishes nothing in shadow" "" "$(shadow env GITHUB_REPOSITORY=o/r GITHUB_SHA=abc bash "$REPO_ROOT/scripts/ci/publish-rolling-release.sh" t T N f)"
check "build-image.sh still builds in shadow, with --load, not --push" "1 0" \
    "$(shadow bash "$REPO_ROOT/scripts/ci/build-image.sh" worker r/w abc > /dev/null; echo "$(grep -c -- 'buildx build .*--load' "$work/calls") $(grep -c -- '--push' "$work/calls")")"
check "mark-grafana-deploy posts nothing in shadow" "" "$(shadow env GRAFANA_URL=https://g GRAFANA_ANNOTATION_TOKEN=t TIER=web SHA=abcdef0 MSG=m bash "$REPO_ROOT/.github/actions/mark-grafana-deploy/mark.sh")"
check "drop-bot-branch deletes nothing in shadow" "" "$(shadow env GH_REPO=o/r BRANCH=b bash "$REPO_ROOT/.github/actions/drop-bot-branch/drop.sh")"

# Copied without the helper, a writer fails loudly -- never a silent skip of its write (identity-takes-check-test.sh
# copied gh-release.sh alone once, and every store "succeeded" by writing nothing).
mkdir -p "$work/alone"; cp "$REPO_ROOT/scripts/ci/gh-release.sh" "$work/alone/"
check "gh-release.sh without ci-writes.sh beside it fails, writing nothing" "2 " \
    "$(: > "$work/calls"; env PATH="$work/bin:$PATH" CI_ROLE=primary bash "$work/alone/gh-release.sh" upload t f > /dev/null 2>&1; echo "$? $(cat "$work/calls")")"

# A sparse checkout that runs a local writer action checks out its helper too: Main #2939's publish-web and
# publish-worker checked out `.github` alone, so mark-grafana-deploy failed on the missing ci-writes.sh.
# Prints `<workflow> <job>` for every job using such an action whose sparse list holds neither scripts nor scripts/ci.
sparse_without_helper() {
    awk '
        function flush() { if (job != "" && sparse && uses && !helper) print FILENAME " " job; job = ""; sparse = uses = helper = inlist = 0 }
        FNR == 1 { flush(); injobs = 0 }
        /^jobs:/ { injobs = 1; next }
        injobs && /^    [A-Za-z0-9_-]+:[ \t]*$/ { flush(); job = $1; sub(/:$/, "", job); next }
        job == "" { next }
        /uses: \.\/\.github\/actions\/(mark-grafana-deploy|drop-bot-branch)/ { uses = 1 }
        /sparse-checkout:/ {
            sparse = 1; v = $0; sub(/.*sparse-checkout:[ \t]*/, "", v)
            if (v == "|") { inlist = 1; next }
            if (v == "scripts" || v == "scripts/ci") helper = 1
            next
        }
        inlist {
            v = $0; gsub(/^[ \t]+|[ \t]+$/, "", v)
            if (v ~ /^[A-Za-z0-9_.\/-]+$/) { if (v == "scripts" || v == "scripts/ci") helper = 1; next }
            inlist = 0
        }
        END { flush() }
    ' "$@"
}
check "every sparse checkout running a writer action checks out scripts/ci/ci-writes.sh" "" \
    "$(sparse_without_helper "$REPO_ROOT"/.github/workflows/*.yml)"
printf 'jobs:\n    publish:\n        steps:\n            - uses: actions/checkout@v5\n              with:\n                  sparse-checkout: .github\n            - uses: ./.github/actions/mark-grafana-deploy\n' > "$work/bad.yml"
check "the sparse-checkout check catches a writer job with .github alone" "$work/bad.yml publish" \
    "$(sparse_without_helper "$work/bad.yml")"

spec_summary
