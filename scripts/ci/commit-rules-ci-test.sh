#!/usr/bin/env bash
# scripts/ci/commit-rules-ci.sh over a scratch repository and a stub gh: the range starts at the newest green Main run's
# commit (else $BEFORE, else nothing to judge); both rule sets run whatever the commits touched; a broken rule fails.
# Run: bash scripts/ci/commit-rules-ci-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m commit-rules-ci.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
scratch_repo "$repo" -b main
mkdir -p "$repo/scripts/ci" "$repo/.github" "$repo/test/resources/fixtures" "$work/bin"
cp "$REPO_ROOT/scripts/ci/commit-rules-ci.sh" "$REPO_ROOT/scripts/ci/commit_rules.py" "$repo/scripts/ci/"
list=test/resources/fixtures/identity-flips-allow.tsv
printf '# the registry\n%s\n' "$list" > "$repo/.github/allow-lists.txt"
printf '# reviewed flips\ncountry\tvenue\trawTitle\treason\n' > "$repo/$list"
scratch_git "$repo" add -A; scratch_git "$repo" commit -qm base
base=$(scratch_git "$repo" rev-parse HEAD)
commit() { # <path> <line> <message>
    mkdir -p "$repo/$(dirname "$1")"; printf '%s\n' "$2" >> "$repo/$1"
    scratch_git "$repo" add -A; scratch_git "$repo" commit -qm "$3"
}
# gh: the newest green Main run's commit is $GREEN (empty: none)
printf '#!/usr/bin/env bash\necho "$*" >> "%s/gh-calls"\n[ -n "${GREEN:-}" ] && echo "$GREEN"\nexit 0\n' "$work" > "$work/bin/gh"
chmod +x "$work/bin/gh"
ungit() { local v; for v in $(compgen -e | grep '^GIT_'); do unset "$v"; done; }
ci() { (cd "$repo" && ungit; KINOWO_GH="$work/bin/gh" bash scripts/ci/commit-rules-ci.sh "$@" > "$work/out" 2>&1; echo $?); }

commit docs/x.md "a doc" "docs"
check "a range breaking no rule passes, from the newest green Main run" "0 1" "$(GREEN=$base ci) $(grep -c "over ${base:0:9}" "$work/out")"

commit "$list" "$(printf 'de\t*\tCAMOUFLAGE\tlooks intended')" "list a flip, pushed --no-verify"
check "a row added to an allow-list after its check's birth fails, though no hook ran" "1 1" \
    "$(GREEN=$base ci) $(grep -c 'an allow-list only shrinks' "$work/out")"
check "...and so it does from the push's own base when no Main run is green" "1" "$(BEFORE=$base ci)"
check "...or from a named base (the landing check's merge-base)" "1" "$(ci --base "$base")"

scratch_git "$repo" reset -q --hard "$base"
commit identity/src/main/scala/services/identity/FeedLink.scala "rule" "$(printf 'overruled\n\nFixes-Red: 2473')"
check "a red fix changing a resolver rule fails the driftguards set" "1 1" \
    "$(GREEN=$base ci) $(grep -c 'driftguards: .*RULE code' "$work/out")"

cp "$REPO_ROOT/scripts/ci/gating_checks.py" "$repo/scripts/ci/"
printf 'kind\tworkflow\tname\tproven\tnote\njob\tmain.yml\tnew\tunproven\n' > "$repo/.github/gating-checks.tsv"
check "a gating-checks registry out of step fails, though no hook ran" "1 1" \
    "$(GREEN=$base ci) $(grep -c 'commit rules (gating checks)' "$work/out")"
rm "$repo/.github/gating-checks.tsv"

check "nothing to judge from (a dispatch, no green run): passes, said" "0 1" \
    "$(BEFORE=0000000000000000000000000000000000000000 ci) $(grep -c 'nothing to judge' "$work/out")"

# A base older than the rules on CI (github.com's newest green Main run predated them by 1392 commits) starts at the
# commit that added this script: what came before it was never CI's to judge; everything since still is.
old="$work/old"
scratch_repo "$old" -b main
mkdir -p "$old/scripts/ci" "$old/.github" "$old/$(dirname "$list")"
printf '# the registry\n%s\n' "$list" > "$old/.github/allow-lists.txt"
printf '# reviewed flips\ncountry\tvenue\trawTitle\treason\n' > "$old/$list"
scratch_git "$old" add -A; scratch_git "$old" commit -qm root
green=$(scratch_git "$old" rev-parse HEAD)
mkdir -p "$old/identity/src/main/scala/services/identity"; echo rule > "$old/identity/src/main/scala/services/identity/FeedLink.scala"
scratch_git "$old" add -A; scratch_git "$old" commit -qm "$(printf 'before the rules\n\nFixes-Red: 2473')"
cp "$REPO_ROOT/scripts/ci/commit-rules-ci.sh" "$REPO_ROOT/scripts/ci/commit_rules.py" "$old/scripts/ci/"
scratch_git "$old" add -A; scratch_git "$old" commit -qm "the rules on CI"
oldci() { (cd "$old" && ungit; KINOWO_GH="$work/bin/gh" bash scripts/ci/commit-rules-ci.sh "$@" > "$work/out" 2>&1; echo $?); }
check "a green run older than the rules on CI judges from the commit that added them" "0 1" \
    "$(GREEN=$green oldci) $(grep -c 'predates the rules on CI' "$work/out")"
echo again >> "$old/identity/src/main/scala/services/identity/FeedLink.scala"
scratch_git "$old" add -A; scratch_git "$old" commit -qm "$(printf 'after the rules\n\nFixes-Red: 2474')"
check "...and still fails a commit after them" "1" "$(GREEN=$green oldci)"

spec_summary
