#!/usr/bin/env bash
# The hash an sbt build cache is keyed by: the content of the sources a job compiles, read off git's index (a blob's id
# is its content's hash) rather than by walking and reading the workspace as `hashFiles` does -- 8.8 s of a US
# convergence leg's setup with its fixture tree staged elsewhere (run 37111868620).
#
#   sbt-sources-key.sh e2e           # what ci.yml's e2e job compiles: e2e/Test, so web, worker and its fixtures
#   sbt-sources-key.sh convergence   # what a convergence leg compiles: the same less the web resources
#
# ONE PLACE FOR BOTH, because a convergence leg restores the exact entry Main's e2e job saved for the same commit: Main
# dispatches the legs only once `ci` (e2e included) is done, so that entry is this commit's whole compile, where the
# newest convergence entry is an older commit's and every leg recompiled the difference -- 20-72 s in each of 14 suite
# jobs (run 38599662543). A copy of a set in two files would drift, and a drifted key silently restores nothing.
set -euo pipefail

common=(build.sbt project/build.properties project/plugins.sbt ':(glob)**/src/main/**/*.scala' ':(glob)**/src/test/**/*.scala'
        ':(glob)**/src/fixtures/**/*.scala')
case "${1:-}" in
  e2e)         set=("${common[@]}" ':(glob)web/src/main/resources/**') ;;
  convergence) set=("${common[@]}") ;;
  *) echo "usage: $0 e2e|convergence" >&2; exit 2 ;;
esac
hash=$(git ls-files -s -- "${set[@]}" | sha256sum)
echo "${hash%% *}"
