#!/usr/bin/env bash
# Purpose: The scalafix tree: what scripts/scalafix-move and scripts/scalafix-organize share
# THE SCALAFIX TREE: what scripts/scalafix-move and scripts/scalafix-organize share -- a worktree KEPT between runs
# (detached, at the caller's HEAD) whose build compiles with SemanticDB on, so a second rewrite recompiles only what
# changed since the first, and the caller's own checkout never pays the full recompile that turning SemanticDB on costs
# (every module's scalacOptions change). Sourcing it defines functions only. bash 3.2 compatible.
#
#   scalafix_tree_open <tool>
#        refuses a caller's checkout with uncommitted changes to tracked files (the rewrite is of HEAD, and its patch is
#        laid back over the checkout); SF_REPO (this checkout; $KINOWO_SCALAFIX_REPO names another: the tests' seam),
#        SF_HEAD (its HEAD), SF_OUT (a work directory, removed at exit) and SF_TREE: $KINOWO_SCALAFIX_TREE, default
#        ~/.cache/kinowo-scalafix/<checkout's name>, made the first time (seeded from the caller's build, as
#        scripts/fail-before seeds), else moved to SF_HEAD (`checkout --detach -f`, untracked files cleaned, ignored ones
#        such as target/ kept)
#   scalafix_tree_sbt <log> <sbt command>...
#        the commands in the tree's kept sbt server, SemanticDB on first (`set ThisBuild / semanticdbEnabled := true`)
#        and sbt-scalafix's cache off (it skips a source it once rewrote even after the tree is moved back to HEAD;
#        $KINOWO_SCALAFIX_SBT names another sbt: the tests' seam); its exit status
#   scalafix_tree_apply
#        the tree's changes against SF_HEAD as one patch (SF_OUT/rewrite.patch, renames kept), laid over the caller's
#        checkout and its index (`git apply --index`); 1 when there was nothing to lay
#
# Every module's main, test, integration, page and fixtures sources are rewritten (`<module>/<config>/scalafix`: the
# configurations refactor-tools.sbt gives scalafix besides Compile and Test).

# DEPENDENTS FIRST: sbt-scalafix compiles each configuration before rewriting it, so a configuration is rewritten only
# after everything that depends on it -- a rewritten dependency (the moved class's own source) would leave a later
# dependent uncompilable against it mid-run.
SCALAFIX_CONFIGURATIONS="quality/Test quality/Compile e2e/Test web/PageTest web/IntegrationTest web/Test web/Compile
worker/IntegrationTest worker/Test worker/Fixtures worker/Compile repochecks/Test repochecks/Fixtures scrapers/Test
scrapers/Fixtures scrapers/Compile itkit/Compile testkit/Test testkit/Compile identity/Test identity/Compile common/Test
common/Compile http/Test http/Fixtures http/Compile base/Test base/Compile geodata/Test geodata/Compile"

scalafix_tree_open() {
  local tool=$1 dirty
  SF_TOOLS=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
  SF_REPO=${KINOWO_SCALAFIX_REPO:-$SF_TOOLS}
  dirty=$(git -C "$SF_REPO" status --porcelain --untracked-files=no)
  [ -z "$dirty" ] || die "the checkout has uncommitted changes (commit or stash them first: the rewrite is of HEAD):
$dirty"
  SF_HEAD=$(git -C "$SF_REPO" rev-parse HEAD)
  SF_OUT=$(mktemp -d "${TMPDIR:-/tmp}/$tool.XXXXXX") || die "cannot create a work directory"
  trap 'rm -rf "$SF_OUT"' EXIT
  trap 'exit 130' INT TERM
  SF_TREE=${KINOWO_SCALAFIX_TREE:-$HOME/.cache/kinowo-scalafix/$(basename "$SF_REPO")}
  if git -C "$SF_TREE" rev-parse --git-dir >/dev/null 2>&1; then
    git -C "$SF_TREE" checkout -q --detach -f "$SF_HEAD" || die "cannot move the scalafix tree $SF_TREE to $SF_HEAD"
    git -C "$SF_TREE" clean -fdq || die "cannot clean the scalafix tree $SF_TREE"
  else
    mkdir -p "$(dirname "$SF_TREE")" || die "cannot create $(dirname "$SF_TREE")"
    git -C "$SF_REPO" worktree add -q --detach "$SF_TREE" "$SF_HEAD" >/dev/null 2>&1 || die "cannot make the scalafix tree $SF_TREE"
    export KINOWO_SBT_WARM_SEED=${KINOWO_SBT_WARM_SEED:-$SF_REPO}
  fi
}

scalafix_tree_sbt() {
  local log=$1
  shift
  (cd "$SF_TREE" && KINOWO_SBT_WARM_ROUTE=local "${KINOWO_SCALAFIX_SBT:-$SF_TOOLS/scripts/sbt-warm}" \
    "set ThisBuild / semanticdbEnabled := true" "set ThisBuild / scalafixCaching := false" "$@") > "$log" 2>&1
}

# scalafix_tree_commands <scalafix arguments> -> one `<module>/<config>/scalafix <arguments>` command a configuration
scalafix_tree_commands() {
  local c
  for c in $SCALAFIX_CONFIGURATIONS; do printf '%s/scalafix %s\n' "$c" "$1"; done
}

scalafix_tree_apply() {
  git -C "$SF_TREE" add -A || die "cannot stage the scalafix tree's changes"
  git -C "$SF_TREE" diff --cached --binary -M "$SF_HEAD" > "$SF_OUT/rewrite.patch" || die "cannot diff the scalafix tree"
  git -C "$SF_TREE" reset -q
  [ -s "$SF_OUT/rewrite.patch" ] || return 1
  git -C "$SF_REPO" apply --index "$SF_OUT/rewrite.patch" || die "the rewrite does not apply to the checkout (patch: $SF_OUT/rewrite.patch)"
}
