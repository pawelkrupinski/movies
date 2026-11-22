#!/usr/bin/env bash
# Purpose: The touched-code tree: what scripts/mutation-check and scripts/coverage-touched share
# THE TOUCHED-CODE TREE: what scripts/mutation-check and scripts/coverage-touched share -- a throwaway detached worktree
# at the range's head, its kept sbt server, the methods the range touches and the specs that reach them. Sourcing it
# defines functions only. bash 3.2 compatible.
#
#   touched_tree_open <tool> <base>..<head>
#        resolves the range in TT_REPO (this checkout; $KINOWO_TOUCHED_REPO names another: the tests' seam) -- TT_BASE,
#        TT_HEAD: full shas --, TT_TOOLS being the checkout these scripts are in, makes TT_OUT (--out's directory, else a temporary one) and the
#        worktree TT_TREE at the head under $KINOWO_TOUCHED_TREE_DIR (default ~/.cache/kinowo-touched-tree), removed --
#        its sbt server stopped first -- by a trap. The caller's checkout is never touched: the instrumented or mutated
#        compile goes to the worktree's own target/. Its first compile is seeded from the caller's build
#        (KINOWO_SBT_WARM_SEED, as scripts/fail-before seeds). The caller's tooling measures it: its
#        project/refactor-tools.sbt (the plugins), refactor-tools.sbt (`touchedSpecs`) and the project/ sources that task
#        reads are laid over the worktree's build, and its scripts drive it -- so an older range is measured as the
#        current tools measure (a head whose project/TestImpact.scala predates `TestImpact.Unit` is out of reach).
#   touched_tree_sbt <log> <sbt command>...
#        the commands in the worktree's kept sbt server (the caller's scripts/sbt-warm, run there; $KINOWO_TOUCHED_SBT
#        names another: the tests' seam), output to <log>; its exit status
#   touched_tree_methods
#        TT_OUT/paths.txt (the head's changed production sources) and TT_OUT/touched.tsv (the touched methods:
#        scripts/touched_methods.py over quality.MethodSpans, run in the CALLER's kept server over the worktree's
#        sources: the quality module reads them as text); 1 when the range touches no production method
#   touched_tree_specs
#        TT_OUT/specs.txt: `<module> <spec class>` for every spec that reaches a touched method (`touchedSpecs`,
#        project/TouchedSpecs.scala: a spec of the method's source's classes, or of a production class that depends on
#        it and uses the method's name -- two steps by name, not the test-impact selection's every transitive dependent)
#
# The sbt server of the worktree holds a JVM slot for its life (stopped at exit); every sbt command takes its own
# governor ticket (scripts/sbt-warm), and the callers run the heavy step under a scripts/gov-run ticket of its own.

# what the caller's tooling lays over the worktree's build: the plugins, the touchedSpecs task and what it reads
TOUCHED_TREE_OVERLAY="project/refactor-tools.sbt refactor-tools.sbt project/AnalysisUnits.scala project/TouchedSpecs.scala project/UsedNamesAccess.scala"

touched_tree_root() { (cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd); }

touched_tree_open() {
  local tool=$1 range=$2 base head dir
  case "$range" in *..*) base=${range%%..*} head=${range#*..} ;; *) usage_error "a range <base>..<head>, not '$range'" ;; esac
  [ -n "$base" ] && [ -n "$head" ] || usage_error "a range <base>..<head>, not '$range'"
  TT_TOOLS=$(touched_tree_root)
  TT_REPO=${KINOWO_TOUCHED_REPO:-$TT_TOOLS}
  TT_BASE=$(git -C "$TT_REPO" rev-parse --verify -q "$base^{commit}") || usage_error "no commit $base"
  TT_HEAD=$(git -C "$TT_REPO" rev-parse --verify -q "$head^{commit}") || usage_error "no commit $head"
  if [ -n "${TT_OUT:-}" ]; then mkdir -p "$TT_OUT" || die "cannot create $TT_OUT"; TT_OUT=$(cd "$TT_OUT" && pwd)
  else TT_OUT=$(mktemp -d "${TMPDIR:-/tmp}/$tool.XXXXXX") || die "cannot create a work directory"; TT_OUT_TEMPORARY=1; fi
  dir=${KINOWO_TOUCHED_TREE_DIR:-$HOME/.cache/kinowo-touched-tree}
  mkdir -p "$dir" || die "cannot create $dir"
  TT_TREE=$(mktemp -d "$dir/$tool.XXXXXX") || die "cannot create a worktree directory"
  rmdir "$TT_TREE"
  trap touched_tree_close EXIT
  trap 'exit 130' INT TERM
  git -C "$TT_REPO" worktree add -q --detach "$TT_TREE" "$TT_HEAD" >/dev/null 2>&1 || die "cannot make a worktree at $TT_HEAD"
  for f in $TOUCHED_TREE_OVERLAY; do
    mkdir -p "$(dirname "$TT_TREE/$f")" && cp "$TT_TOOLS/$f" "$TT_TREE/$f" || die "cannot lay $f over the worktree"
  done
  # the caller's build is the nearest seed when it is at the head; sbt-warm's own choice (the newest main build) else
  if [ -z "${KINOWO_SBT_WARM_SEED:-}" ] && [ "$(git -C "$TT_REPO" rev-parse HEAD)" = "$TT_HEAD" ]; then
    export KINOWO_SBT_WARM_SEED=$TT_REPO
  fi
}

touched_tree_close() {
  if [ -n "${TT_TREE:-}" ] && [ -d "$TT_TREE" ]; then
    if [ -z "${KINOWO_TOUCHED_SBT:-}" ]; then (cd "$TT_TREE" && "$TT_TOOLS/scripts/sbt-warm" --stop) >/dev/null 2>&1; fi
    git -C "$TT_REPO" worktree remove --force "$TT_TREE" >/dev/null 2>&1
    git -C "$TT_REPO" worktree prune >/dev/null 2>&1
  fi
  if [ -n "${TT_OUT_TEMPORARY:-}" ]; then rm -rf "$TT_OUT"; fi
  return 0
}

touched_tree_sbt() {
  local log=$1
  shift
  (cd "${TT_SBT_DIR:-$TT_TREE}" && KINOWO_SBT_WARM_ROUTE=local "${KINOWO_TOUCHED_SBT:-$TT_TOOLS/scripts/sbt-warm}" "$@") > "$log" 2>&1
}

touched_tree_methods() {
  local paths
  python3 -I "$TT_TOOLS/scripts/touched_methods.py" paths "$TT_BASE" "$TT_HEAD" --repo "$TT_REPO" > "$TT_OUT/paths.txt" \
    || die "cannot read the range's changed sources"
  [ -s "$TT_OUT/paths.txt" ] || return 1
  paths=$(tr '\n' ' ' < "$TT_OUT/paths.txt")
  TT_SBT_DIR=$TT_TOOLS touched_tree_sbt "$TT_OUT/spans.log" "quality/runMain quality.MethodSpans --root $TT_TREE --out $TT_OUT/spans.tsv $paths" \
    || { tail -20 "$TT_OUT/spans.log" >&2; die "quality.MethodSpans failed (log: $TT_OUT/spans.log)"; }
  python3 -I "$TT_TOOLS/scripts/touched_methods.py" touched "$TT_BASE" "$TT_HEAD" --spans "$TT_OUT/spans.tsv" --repo "$TT_REPO" \
    > "$TT_OUT/touched.tsv" || die "cannot meet the range's hunks with the method spans"
  [ -s "$TT_OUT/touched.tsv" ]
}

touched_tree_specs() {
  touched_tree_sbt "$TT_OUT/specs.log" "touchedSpecs $TT_OUT/touched.tsv $TT_OUT/specs.txt" \
    || { tail -20 "$TT_OUT/specs.log" >&2; die "the reaching specs' selection failed (log: $TT_OUT/specs.log)"; }
}

# touched_tree_modules  -> the modules of the touched methods, one a line
touched_tree_modules() { cut -f1 "$TT_OUT/touched.tsv" | cut -d/ -f1 | sort -u; }
