#!/usr/bin/env bash
# pre-push's path selection: which checks a push's changed paths run, and that an unrelated push
# runs none of them. Run: bash scripts/hooks/pre-push-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m pre-push --plan\n'

# plan <path>... -> the selected checks, space-separated
plan() { printf '%s\n' "$@" | "$REPO_ROOT/scripts/hooks/pre-push" --plan | tr '\n' ' ' | sed 's/ $//'; }

check "production JS runs ESLint" "eslint" "$(plan web/src/main/assets/js/shared.js)"
check "the ESLint config runs ESLint" "eslint" "$(plan eslint.config.js)"
check "a roster source runs the generated-artefact drift check" "drift" "$(plan data/pl/venues.json)"
check "a model file runs the drift check AND the sbt lints" "drift sbt" "$(plan common/src/main/scala/models/Cinema.scala)"
check "the iOS City list runs the drift check and the iOS compile" "drift ios" "$(plan ios/Kinowo/Models/City.swift)"
check "a recorded fixture runs the snapshot stamp check" "snapshot" \
  "$(plan test/resources/fixtures/08-06-2026/www.bilety24.pl/kino/organizator/x)"
check "any Scala source runs the sbt lints" "sbt" "$(plan worker/src/test/scala/tools/FooSpec.scala)"
check "a Grafana dashboard runs the metric-coverage lint" "sbt" \
  "$(plan infra/nix/files/monitoring/grafana/dashboards/apps/kinowo-http.json)"
check "a workflow runs actionlint" "actionlint" "$(plan .github/workflows/ci.yml)"
check "a shell script runs shellcheck" "shellcheck" "$(plan scripts/ci/order-seed.sh)"
check "the hook itself runs shellcheck" "shellcheck" "$(plan scripts/hooks/pre-push)"
check "an iOS view runs the iOS compile check" "ios" "$(plan ios/Kinowo/Views/FilmCard.swift)"
check "an Android-only change runs nothing" "" "$(plan android/app/src/main/java/pl/kinowo/ui/X.kt)"
check "docs run nothing" "" "$(plan docs/readme.md README.md)"
check "checks come out once each, in run order" "eslint snapshot actionlint shellcheck sbt" \
  "$(plan web/src/main/assets/js/a.js build.sbt scripts/x.sh .github/workflows/a.yml worker/src/main/scala/A.scala test/resources/fixtures/08-06-2026/y)"

spec_summary
