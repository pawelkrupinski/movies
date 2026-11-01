#!/usr/bin/env bash
# scripts/ci/commit-rules-ci.sh over a scratch repository and its bare origin: the range starts at refs/verified/commit-rules
# (else $BEFORE, else nothing to judge); both rule sets run whatever the commits touched; a broken rule fails; a run on
# main whose rules hold advances the ref.
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
origin="$work/origin.git"
scratch_repo "$origin" --bare -b main
scratch_git "$repo" remote add origin "$origin"
scratch_git "$repo" push -q origin main
verify() { scratch_git "$repo" push -q -f origin "$1:refs/verified/commit-rules"; }   # <commit>
verified() { scratch_git "$origin" rev-parse -q --verify refs/verified/commit-rules || echo none; }
ungit() { local v; for v in $(compgen -e | grep '^GIT_'); do unset "$v"; done; }
ci() { (cd "$repo" && ungit; bash scripts/ci/commit-rules-ci.sh "$@" > "$work/out" 2>&1; echo $?); }

commit docs/x.md "a doc" "docs"
verify "$base"
check "a range breaking no rule passes, from refs/verified/commit-rules" "0 1" "$(ci) $(grep -c "over ${base:0:9}" "$work/out")"
check "...and a branch run (not a push to main) leaves the ref where it was" "$base" "$(verified)"
head=$(scratch_git "$repo" rev-parse HEAD)
check "a push to main whose rules hold advances the ref to HEAD" "0 $head" \
    "$(GITHUB_REF=refs/heads/main GITHUB_EVENT_NAME=push ci) $(verified)"
verify "$base"

commit "$list" "$(printf 'de\t*\tCAMOUFLAGE\tlooks intended')" "list a flip, pushed --no-verify"
check "a row added to an allow-list after its check's birth fails, though no hook ran" "1 1" \
    "$(ci) $(grep -c 'an allow-list only shrinks' "$work/out")"
check "...and a failing run on main does not advance the ref" "1 $base" \
    "$(GITHUB_REF=refs/heads/main GITHUB_EVENT_NAME=push ci) $(verified)"
scratch_git "$origin" update-ref -d refs/verified/commit-rules
check "...and so it fails from the push's own base before the ref exists" "1" "$(BEFORE=$base ci)"
verify "$base"
check "...or from a named base (the landing check's merge-base)" "1" "$(ci --base "$base")"

scratch_git "$repo" reset -q --hard "$base"
commit identity/src/main/scala/services/identity/FeedLink.scala "rule" "$(printf 'overruled\n\nFixes-Red: 2473')"
check "a red fix changing a resolver rule fails the driftguards set" "1 1" \
    "$(ci) $(grep -c 'driftguards: .*RULE code' "$work/out")"

cp "$REPO_ROOT/scripts/ci/gating_checks.py" "$repo/scripts/ci/"
printf 'kind\tworkflow\tname\tproven\tnote\njob\tmain.yml\tnew\tunproven\n' > "$repo/.github/gating-checks.tsv"
check "a gating-checks registry out of step fails, though no hook ran" "1 1" \
    "$(ci) $(grep -c 'commit rules (gating checks)' "$work/out")"
rm "$repo/.github/gating-checks.tsv"

scratch_git "$origin" update-ref -d refs/verified/commit-rules
check "nothing to judge from (a dispatch, no verified ref): passes, said" "0 1" \
    "$(BEFORE=0000000000000000000000000000000000000000 ci) $(grep -c 'nothing to judge' "$work/out")"
scratch_git "$repo" remote set-url origin "$work/no-such-origin"
check "an origin that cannot be read is no verdict (exit 2), never a judgement from nowhere" "2" "$(ci)"
scratch_git "$repo" remote set-url origin "$origin"

spec_summary
