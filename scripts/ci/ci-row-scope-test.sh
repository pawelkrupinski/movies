#!/usr/bin/env bash
# ci-row-scope.sh: which of ci.yml's page, mobile and e2e rows a push's paths can move, and the `scope` job's
# outputs -- diffed from the newest green Main run, every row on anything unreadable. gh is a stub; git is a real
# throwaway repository whose `origin` is itself.
# Run: bash scripts/ci/ci-row-scope-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# shellcheck source=scripts/scratch-git.sh
. "$REPO_ROOT/scripts/scratch-git.sh"
scope="$REPO_ROOT/scripts/ci/ci-row-scope.sh"

printf '\033[36m▸\033[0m ci-row-scope.sh\n'

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# A repository shaped like this one where it matters: the consumers name some scripts, one sources another.
repo="$tmp/repo"
mkdir -p "$repo/.github/workflows" "$repo/.github/actions/run-page-test" "$repo/scripts/ci" "$repo/e2e/src" \
    "$repo/web/src/page" "$repo/docs" "$repo/identity/src/main" "$repo/identity/src/test" "$repo/worker"
echo 'run: scripts/ci/tree-unchanged.sh' >"$repo/.github/workflows/ci.yml"
echo 'run: ../scripts/ci/install-playwright.sh' >"$repo/.github/actions/run-page-test/action.yml"
echo 'Process(Seq("scripts/read-model-snapshot-inputs.sh", "write"))' >"$repo/e2e/src/Spec.scala"
printf '#!/usr/bin/env bash\n. "$here/../tool-conventions.sh"\n# see scripts/identity-measure for the rest\n' \
    >"$repo/scripts/ci/tree-unchanged.sh"
for f in scripts/ci/install-playwright.sh scripts/read-model-snapshot-inputs.sh scripts/tool-conventions.sh \
    scripts/identity-measure scripts/identity_rule_ablation.py scripts/ci/identity_takes.py; do
    echo "# $f" >"$repo/$f"
done

# verdict <kind> <path...> -> true|false
verdict() {
    local kind=$1
    shift
    printf '%s\n' "$@" | bash "$scope" "$kind" "$repo"
}

for kind in e2e pages browsers; do
    check "$kind: docs alone move nothing" "false" "$(verdict $kind docs/identity-experiments.md)"
    check "$kind: a top-level markdown file moves nothing" "false" "$(verdict $kind CLAUDE.md)"
    check "$kind: skills move nothing" "false" "$(verdict $kind .claude/skills/x/SKILL.md)"
    check "$kind: the judgements move nothing" "false" "$(verdict $kind test/resources/fixtures/identity-judgements.tsv)"
    check "$kind: a script no row runs moves nothing" "false" \
        "$(verdict $kind scripts/identity_rule_ablation.py scripts/ci/identity_takes.py)"
    check "$kind: a script named only in a live script's comment moves nothing" "false" "$(verdict $kind scripts/identity-measure)"
    check "$kind: a script ci.yml runs moves it" "true" "$(verdict $kind scripts/ci/tree-unchanged.sh)"
    check "$kind: a script a composite action runs moves it" "true" "$(verdict $kind scripts/ci/install-playwright.sh)"
    check "$kind: a script an e2e spec runs moves it" "true" "$(verdict $kind scripts/read-model-snapshot-inputs.sh)"
    check "$kind: a script a live script sources moves it" "true" "$(verdict $kind scripts/tool-conventions.sh)"
    check "$kind: application code moves it" "true" "$(verdict $kind docs/a.md worker/src/main/scala/A.scala)"
    check "$kind: a workflow moves it" "true" "$(verdict $kind .github/workflows/ci.yml)"
    check "$kind: a markdown file inside a module moves it" "true" "$(verdict $kind web/src/main/resources/a.md)"
    check "$kind: an empty change moves nothing" "false" "$(printf '' | bash "$scope" $kind "$repo")"
done
# A live-scripts list past the pipe buffer (64 KiB): the membership test's `grep -q` closing its pipe early must not turn
# a live script into "not live" (SIGPIPE on printf, 141 under pipefail). The match sorts first, so grep quits at once.
big="$tmp/big"
mkdir -p "$big/.github/workflows" "$big/scripts"
: >"$big/.github/workflows/ci.yml"
for i in $(seq 1 1200); do
    name="zz-live-script-with-a-deliberately-long-name-to-fill-the-pipe-buffer-$i.sh"
    echo "# $name" >"$big/scripts/$name"
    echo "run: scripts/$name" >>"$big/.github/workflows/ci.yml"
done
echo "# first" >"$big/scripts/aa-first.sh"
echo "run: scripts/aa-first.sh" >>"$big/.github/workflows/ci.yml"
check "a live script is live though the live list outgrows the pipe buffer" "true" \
    "$(echo scripts/aa-first.sh | bash "$scope" e2e "$big")"
check "e2e: identity's tests move it (main.yml's web tier ships identity/**)" "true" \
    "$(verdict e2e identity/src/test/scala/ASpec.scala)"
check "pages: identity's tests move nothing" "false" "$(verdict pages identity/src/test/scala/ASpec.scala)"
check "browsers: identity's tests move nothing" "false" "$(verdict browsers identity/src/test/scala/ASpec.scala)"
check "e2e: identity's code moves it" "true" "$(verdict e2e identity/src/main/scala/A.scala)"
check "pages: identity's code moves it (the admin pages render it)" "true" "$(verdict pages identity/src/main/scala/A.scala)"
check "browsers: identity's code moves nothing (the public pages render the snapshot)" "false" \
    "$(verdict browsers identity/src/main/scala/A.scala)"
check "an unknown kind is refused" "2" "$(echo docs/a.md | bash "$scope" deploy "$repo" >/dev/null 2>&1; echo $?)"

# --outputs: a real git history, `origin` itself; gh answers the newest green Main run from $tmp/green.
scratch_repo "$repo"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
green_sha=$(scratch_git "$repo" rev-parse HEAD)
scratch_git "$repo" remote add origin "$repo"
mkdir -p "$tmp/bin"
cat >"$tmp/bin/gh" <<'GH'
#!/usr/bin/env bash
[ -f "$STUB/gh-fails" ] && { echo "HTTP 502" >&2; exit 1; }
cat "$STUB/green" 2>/dev/null
GH
chmod +x "$tmp/bin/gh"
[ -x "$(command -v timeout)" ] || printf '#!/usr/bin/env bash\nshift\nexec "$@"\n' >"$tmp/bin/timeout"
chmod +x "$tmp/bin/timeout" 2>/dev/null || true

# outputs <commit> [env...] -> the e2e/pages/browsers lines written to GITHUB_OUTPUT, space-joined
outputs() {
    local sha=$1
    shift
    rm -f "$tmp/out"
    (cd "$repo" && env PATH="$tmp/bin:$PATH" STUB="$tmp" GITHUB_OUTPUT="$tmp/out" GITHUB_SHA="$sha" \
        BEFORE="$green_sha" PULL_REQUEST= "$@" bash "$scope" --outputs >"$tmp/log" 2>&1)
    tr '\n' ' ' <"$tmp/out" | sed 's/ $//'
}
commit() { # <path> -> the new commit
    mkdir -p "$repo/$(dirname "$1")" && echo "$RANDOM" >>"$repo/$1"
    scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "$1" && scratch_git "$repo" rev-parse HEAD
}

echo "$green_sha" >"$tmp/green"
docs_sha=$(commit docs/b.md)
check "a docs-only push since the green run skips every row" "e2e=false pages=false browsers=false" "$(outputs "$docs_sha")"
identity_sha=$(commit identity/src/main/scala/A.scala)
check "identity's code since the green run keeps e2e and the Scala page row" "e2e=true pages=true browsers=false" \
    "$(outputs "$identity_sha")"
# A RED or superseded run in between changes nothing: the base stays the newest GREEN run's commit.
docs2_sha=$(commit docs/c.md)
check "a later docs push still carries identity's change until a run passes with it" \
    "e2e=true pages=true browsers=false" "$(outputs "$docs2_sha")"
echo "$identity_sha" >"$tmp/green"
check "once a run passed with it, the docs push skips every row" "e2e=false pages=false browsers=false" \
    "$(outputs "$docs2_sha")"
scratch_git "$repo" tag deployed-web "$green_sha"
check "a tier not yet deployed keeps the e2e row, which builds its image" "e2e=true pages=false browsers=false" \
    "$(outputs "$docs2_sha")"
scratch_git "$repo" tag -d deployed-web >/dev/null
check "a pull request runs every row" "e2e=true pages=true browsers=true" "$(outputs "$docs2_sha" PULL_REQUEST=7)"
check "a dispatch (no before) runs every row" "e2e=true pages=true browsers=true" "$(outputs "$docs2_sha" BEFORE=)"
check "a new branch's all-zero before runs every row" "e2e=true pages=true browsers=true" \
    "$(outputs "$docs2_sha" BEFORE=0000000000000000000000000000000000000000)"
: >"$tmp/green"
check "no green Main run runs every row" "e2e=true pages=true browsers=true" "$(outputs "$docs2_sha")"
touch "$tmp/gh-fails"
check "an unreadable runs list runs every row" "e2e=true pages=true browsers=true" "$(outputs "$docs2_sha")"
check "and says why" "1" "$(grep -c 'could not read Main.s runs (HTTP 502).*every row runs' "$tmp/log")"
rm -f "$tmp/gh-fails"
# More paths than the job prints: the listing's `head` closing its pipe early must not kill the job (exit 141 under
# pipefail, Main 2280: 123 paths since the green run).
echo "$docs2_sha" >"$tmp/green"
mkdir -p "$repo/docs/many" && for i in $(seq 1 3000); do echo x >"$repo/docs/many/page-$i-with-a-long-enough-name.md"; done
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm many && many_sha=$(scratch_git "$repo" rev-parse HEAD)
check "a push of thousands of paths still decides every row" "e2e=false pages=false browsers=false" "$(outputs "$many_sha")"
check "...listing only the first 50" "50" "$(grep -c '^  docs/many/' "$tmp/log")"
echo 0123456789abcdef0123456789abcdef01234567 >"$tmp/green"
check "a green commit that cannot be fetched runs every row" "e2e=true pages=true browsers=true" "$(outputs "$docs2_sha")"

spec_summary
