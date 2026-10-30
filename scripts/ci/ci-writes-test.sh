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
check "scheduled-failure-issue.sh opens no issue in shadow" "" "$(shadow bash "$REPO_ROOT/scripts/ci/scheduled-failure-issue.sh" failed W https://x/run o)"
touch "$work/report"
check "bot-branch-ready.sh opens no issue in shadow" "" "$(shadow env GH_REPO=o/r bash "$REPO_ROOT/scripts/ci/bot-branch-ready.sh" b T "$work/report" https://x/run)"
check "publish-rolling-release.sh publishes nothing in shadow" "" "$(shadow env GITHUB_REPOSITORY=o/r GITHUB_SHA=abc bash "$REPO_ROOT/scripts/ci/publish-rolling-release.sh" t T N f)"
check "build-image.sh still builds in shadow, with --load, not --push" "1 0" \
    "$(shadow bash "$REPO_ROOT/scripts/ci/build-image.sh" worker r/w abc > /dev/null; echo "$(grep -c -- 'buildx build .*--load' "$work/calls") $(grep -c -- '--push' "$work/calls")")"
check "mark-grafana-deploy posts nothing in shadow" "" "$(shadow env GRAFANA_URL=https://g GRAFANA_ANNOTATION_TOKEN=t TIER=web SHA=abcdef0 MSG=m bash "$REPO_ROOT/.github/actions/mark-grafana-deploy/mark.sh")"
check "drop-bot-branch deletes nothing in shadow" "" "$(shadow env GH_REPO=o/r BRANCH=b bash "$REPO_ROOT/.github/actions/drop-bot-branch/drop.sh")"

spec_summary
