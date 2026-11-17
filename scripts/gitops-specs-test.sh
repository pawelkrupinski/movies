#!/usr/bin/env bash
# gitops-specs: the plan read off a fixture lint registry, and a --manifests run against a scratch movies repository
# whose sbt-warm is a stub that passes or fails on the manifests it is shown.
# Run: bash scripts/gitops-specs-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
TOOL="$REPO_ROOT/scripts/gitops-specs"

printf '\033[36m▸\033[0m gitops-specs\n'

work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT

registry="$work/registry.tsv"
printf '%s\n' \
  '# header row naming @gitops in a comment' \
  "repochecks:deploy.ASpec	@gitops" \
  "common:models.NotGitops	@repo *.scala" \
  "worker:deploy.BSpec	@gitops infra/nix/files/x.rules" \
  "repochecks:deploy.CSpec	@repo @gitops" \
  "repochecks:deploy.LookalikeSpec	@gitopsx" > "$registry"

plan="$(KINOWO_LINT_REGISTRY="$registry" "$TOOL" --plan)"
check "the plan is one testOnly line per module, @gitops rows only, in first-seen order" \
  "repochecks/testOnly deploy.ASpec deploy.CSpec|worker/testOnly deploy.BSpec" "$(printf '%s' "$plan" | tr '\n' '|')"

# ---- a scratch movies: origin with a main carrying the registry and an sbt-warm stub that records what it was asked
# and fails when the manifests carry a file named `breaks-main`.
origin="$work/origin.git"
movies="$work/movies"
scratch_repo "$origin" --bare
scratch_repo "$movies"
mkdir -p "$movies/scripts/hooks"
cp "$registry" "$movies/scripts/hooks/lint-registry.tsv"
cat > "$movies/scripts/sbt-warm" <<'EOF'
#!/usr/bin/env bash
[ "${1:-}" = --stop ] && { echo stopped >> "$PWD/../sbt-calls"; exit 0; }
printf '%s\n' "$@" > "$PWD/../sbt-calls"
ls -A infra/kubernetes > "$PWD/../seen"
[ -e infra/kubernetes/breaks-main ] && { echo "[info] - x *** FAILED ***"; exit 1; }
echo "[info] Tests: succeeded 3, failed 0"
EOF
chmod +x "$movies/scripts/sbt-warm"
scratch_git "$movies" add -A
scratch_git "$movies" commit -qm base
scratch_git "$movies" remote add origin "$origin"
scratch_git "$movies" push -q origin HEAD:main

gitops="$work/gitops"
scratch_repo "$gitops"
echo manifest > "$gitops/all.yaml"
scratch_git "$gitops" add -A
scratch_git "$gitops" commit -qm base

run() { KINOWO_MOVIES_REPO="$movies" KINOWO_GITOPS_CHECK_WORKTREE="$work/check" "$TOOL" --manifests "$gitops" > "$work/out" 2>&1; echo $?; }

check "a tree main's specs accept is GREEN (exit 0)" 0 "$(run)"
check "the specs ran in the check worktree, as main's registry plans them" \
  "repochecks/testOnly deploy.ASpec deploy.CSpec|worker/testOnly deploy.BSpec|stopped" "$(tr '\n' '|' < "$work/check/../sbt-calls" | sed 's/|$//')"
check "they saw the tree under test as infra/kubernetes, without its .git" "all.yaml" "$(cat "$work/seen")"
check "the check worktree sits at movies origin/main" \
  "$(scratch_git "$movies" rev-parse origin/main)" "$(scratch_git "$work/check" rev-parse HEAD)"

touch "$gitops/breaks-main"
check "a tree that fails a spec is RED (exit 1)" 1 "$(run)"
check "the RED names the failure" 1 "$(grep -c 'RED -- this tree would turn movies main red' "$work/out")"
rm "$gitops/breaks-main"
check "a file the last tree had is gone from the next run's infra/kubernetes" 0 "$(run)"
check "only the current tree is shown" "all.yaml" "$(cat "$work/seen")"

check "no manifests directory: could not run (exit 2)" 2 \
  "$(KINOWO_MOVIES_REPO="$movies" KINOWO_GITOPS_CHECK_WORKTREE="$work/check" "$TOOL" --manifests "$work/nope" > /dev/null 2>&1; echo $?)"

spec_summary
