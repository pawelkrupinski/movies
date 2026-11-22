#!/usr/bin/env bash
# THE ORDER INDEPENDENCE VERDICT: a leg's red says an ORDER dependence only when the tests it failed pass in DEFAULT
# order at the same commit. Of Order independence's ~66 failing jobs over runs #1-#100, ~31 were regressions (the same
# failure in default order), ~18 flakes and ~12 the environment, against ONE real order dependence (2026-11-22
# analysis), yet each read as an order red. So every leg runs through `leg`: on a failure it names the failing tests,
# runs exactly those again in default order (the CONTROL PASS), and labels the leg
#   regression  some fail in default order too: a change broke them, whatever ran before them;
#   order       all pass in default order: an order dependence (or a flake -- a control pass is one sample);
#   none        the leg failed naming no test (a build error, a timeout, the runner): no verdict;
#   pass        the leg passed.
# The control pass is a DIAGNOSIS, never a rerun to green: `leg` exits with the leg's own status whatever it found.
#
#   order-compare.sh leg <leg> <record dir> -- <command...>   run the leg's command; on a failure the control pass,
#                                                     its record in <record dir>, the verdict as an annotation (agentctl
#                                                     reads `REGRESSION, not an order dependence:`), the step summary
#                                                     and `order-verdict=` in $GITHUB_OUTPUT. <leg>: scala-reverse,
#                                                     scala-shuffled, android, android-instrumented, swift, ios.
#   order-compare.sh control-command <leg> <test>...  the default-order command `leg` runs for these tests (a dry run)
#   order-compare.sh failed-suites <sbt log>          the suites sbt names under `Failed tests:` / `Error during tests:`
#   order-compare.sh failed-junit <dir>               the classes of the failed test cases in <dir>'s JUnit XML reports
#   order-compare.sh failed-xctest <log>              the cases XCTest's log says failed, as <Module>.<Class>/<test>
#   order-compare.sh summary <dir> [<shuffled seed>]  every leg's verdict as markdown, and scala's reverse vs shuffled
#   order-compare.sh verdict <dir>                    the run's one word (regression > order > none > pass) and
#                                                     <dir>/order-verdict.json, every leg's verdict and tests
#
# A leg's record in <record dir>: <leg>.result (success|failure), <leg>.txt (the failing tests, one a line, sorted),
# <leg>.control.txt (those failing in default order too), <leg>.verdict, <leg>.log (its output). A leg script that
# runs each test alone writes its failing tests to $ORDER_FAILED_FILE (swift-test-shuffled.sh, devtest-shuffled.sh,
# xcuitest-each-class-alone.sh); sbt's are read from its log and Gradle's from the JUnit reports.
# ORDER_CONTROL_CMD, when set, runs the control pass instead as `$ORDER_CONTROL_CMD <leg> <test>...` (its test);
# ORDER_CONTROL_EXTRA: words added to the android control command (the leg's own -P settings but the order seed);
# ORDER_ANDROID_RESULTS stands in for the Gradle test-results directory.
# Runs under macOS's /bin/bash 3.2 too (the swift and ios legs). (scripts/ci/order-compare-test.sh)
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
android_results="${ORDER_ANDROID_RESULTS:-$root/android/app/build/test-results/testDebugUnitTest}"

failed_suites() { # <sbt log>
  [ -f "$1" ] || return 0 # no log: the leg never ran sbt, nothing named
  # ANSI colours off, then the indented names under each heading, until the next line that is not one
  sed $'s/\x1b\\[[0-9;]*[A-Za-z]//g' "$1" | awk '
    /^\[error\][[:space:]]+(Failed tests|Error during tests):[[:space:]]*$/ { inside = 1; next }
    inside && /^\[error\][[:space:]]+[A-Za-z_][A-Za-z0-9_.$]*[[:space:]]*$/ { sub(/^\[error\][[:space:]]+/, ""); sub(/[[:space:]]+$/, ""); print; next }
    { inside = 0 }' | LC_ALL=C sort -u
}

failed_junit() { # <dir>: a <testcase classname="C" ...> holding a <failure or <error, as C
  [ -d "$1" ] || return 0
  find "$1" -name '*.xml' -type f -exec cat {} + 2> /dev/null | tr '\n' ' ' | awk '{ gsub(/<testcase /, "\n<testcase "); print }' \
    | grep -E '^<testcase [^>]*[^/]>[^<]*<(failure|error)[ >/]' | sed -nE 's/^<testcase [^>]*classname="([^"]+)".*/\1/p' \
    | LC_ALL=C sort -u || true
}

failed_xctest() { # <log>: Darwin's `Test Case '-[Mod.Class test]' failed`
  [ -f "$1" ] || return 0
  sed -nE "s/^.*Test Case '-\[([A-Za-z0-9_]+)\.([A-Za-z0-9_]+) ([A-Za-z0-9_]+)\]' failed.*/\1.\2\/\3/p" "$1" | LC_ALL=C sort -u
}

# The default-order command for <leg>'s tests, a word a line (the dry run's output, and what control_run runs).
control_command() { # <leg> <test>...
  local leg=$1 t
  shift
  case "$leg" in
    scala-*) printf '%s\n' env -u TEST_ORDER_SEED sbt "testOnly $*" ;;
    android)
      printf '%s\n' "$root/android/gradlew" -p "$root/android" --no-daemon --offline testDebugUnitTest
      # the leg's other settings (its sync-model depth): only the order may differ from the leg's run
      for t in ${ORDER_CONTROL_EXTRA:-}; do printf '%s\n' "$t"; done
      for t in "$@"; do printf '%s\n' --tests "$t"; done ;;
    android-instrumented) for t in "$@"; do printf '%s\n' "$root/android/scripts/devtest.sh --skip-live-youtube $t"; done ;;
    swift)
      printf '%s\n' swift test
      for t in ${SWIFTPM_FLAGS:-}; do printf '%s\n' "$t"; done # as the leg ran it (swift-test-shuffled.sh)
      printf '%s\n' --package-path "$root/ios" --skip-build --filter \
        "^($(printf '%s\n' "$@" | sed 's/[.[\*^$()+?{}|]/\\&/g' | paste -sd'|' -))\$" ;;
    ios)
      printf '%s\n' xcodebuild test-without-building -project "$root/ios/Kinowo.xcodeproj" -scheme Kinowo \
        -destination "${DESTINATION:-}" -derivedDataPath "$root/build/dd" -resultBundlePath "$root/build/alone/control.xcresult" \
        -test-timeouts-enabled YES -default-test-execution-time-allowance 180 -maximum-test-execution-time-allowance 300
      for t in "$@"; do printf '%s\n' "-only-testing:KinowoUITests/$t"; done
      printf '%s\n' CODE_SIGNING_ALLOWED=NO ;;
    *) echo "order-compare.sh: unknown leg $leg" >&2; return 64 ;;
  esac
}

# Run <leg>'s tests in default order; print those failing there too (a subset of the tests), one a line.
control_run() { # <leg> <dir> <test>...
  local leg=$1 dir=$2 status=0 named w t
  shift 2
  local log="$dir/$leg.control.log"
  : > "$log"
  if [ -n "${ORDER_CONTROL_CMD:-}" ]; then
    $ORDER_CONTROL_CMD "$leg" "$@" < /dev/null > "$log" 2>&1 || status=$?
  elif [ "$leg" = android-instrumented ]; then
    for t in "$@"; do # each class alone, as the leg ran it (a fresh install each), in name order
      "$root/android/scripts/devtest.sh" --skip-live-youtube "$t" < /dev/null >> "$log" 2>&1 || echo "$t"
    done
    return 0
  else
    set --  "$leg" "$@"
    local cmd
    cmd="$(control_command "$@")"
    set --
    while IFS= read -r w; do set -- "$@" "$w"; done <<< "$cmd"
    [ "$leg" != ios ] || rm -rf "$root/build/alone/control.xcresult"
    [ "$leg" != android ] || rm -rf "$android_results"
    "$@" < /dev/null > "$log" 2>&1 || status=$?
  fi
  [ "$status" -ne 0 ] || return 0
  case "$leg" in
    android) named="$(failed_junit "$android_results")" ;;
    swift) named="$(failed_xctest "$log")" ;;
    ios) named="$(failed_xctest "$log" | sed -E 's/^[A-Za-z0-9_]+\.([A-Za-z0-9_]+)\/.*/\1/' | LC_ALL=C sort -u)" ;;
    *) named="$(failed_suites "$log")" ;;
  esac
  # only what was asked: a name the control run reports that the leg did not fail is no finding of this pass
  [ -z "$named" ] || LC_ALL=C comm -12 <(LC_ALL=C sort -u "$dir/$leg.txt") <(printf '%s\n' "$named" | LC_ALL=C sort -u)
}

names() { sed 's/.*/`&`/' | paste -sd, - | sed 's/,/, /g'; } # stdin's tests, a line each, as one list

leg() { # <leg> <dir> -- <command...>
  local leg=$1 dir=$2 status failed control="" verdict line count
  shift 2
  [ "${1:-}" = -- ] || { echo "usage: order-compare.sh leg <leg> <record dir> -- <command...>" >&2; return 64; }
  shift
  case "$leg" in scala-reverse|scala-shuffled|android|android-instrumented|swift|ios) ;;
    *) echo "order-compare.sh: unknown leg $leg" >&2; return 64 ;; esac
  mkdir -p "$dir"
  : > "$dir/$leg.txt"
  set +e
  ORDER_FAILED_FILE="$dir/$leg.txt" "$@" 2>&1 | tee "$dir/$leg.log"
  status=${PIPESTATUS[0]}
  set -e
  if [ "$status" -eq 0 ]; then
    echo success > "$dir/$leg.result"; echo pass > "$dir/$leg.verdict"; : > "$dir/$leg.txt"; : > "$dir/$leg.control.txt"
    echo "order-verdict=pass" >> "${GITHUB_OUTPUT:-/dev/null}"
    return 0
  fi
  echo failure > "$dir/$leg.result"
  case "$leg" in
    scala-*) failed="$(failed_suites "$dir/$leg.log")" ;;
    android) failed="$(failed_junit "$android_results")" ;;
    *) failed="$(grep . "$dir/$leg.txt" | LC_ALL=C sort -u || true)" ;;
  esac
  if [ -n "$failed" ]; then printf '%s\n' "$failed" > "$dir/$leg.txt"; else : > "$dir/$leg.txt"; fi
  if [ -z "$failed" ]; then
    verdict=none
    line="NO ORDER VERDICT: the $leg leg failed (exit $status) naming no test -- a build error, a timeout or the runner"
    echo "::warning title=Order verdict ($leg)::$line"
  else
    count="$(printf '%s\n' "$failed" | wc -l | tr -d ' ')"
    echo "::group::control pass: the $count failing tests again, in DEFAULT order, at this commit"
    # shellcheck disable=SC2046 # one test a word: test names hold no space
    control="$(control_run "$leg" "$dir" $(printf '%s\n' "$failed"))"
    echo "::endgroup::"
    if [ -n "$control" ]; then
      verdict=regression
      line="REGRESSION, not an order dependence: $(printf '%s\n' "$control" | names) fail in default order too -- a change broke them; find it among the commits since the last green run"
    else
      verdict=order
      line="ORDER DEPENDENCE (or a flake): $(printf '%s\n' "$failed" | names) fail in the $leg order but pass in default order${ORDER_REPLAY:+ -- replay: $ORDER_REPLAY}"
    fi
    echo "::error title=Order verdict ($leg)::$line"
  fi
  if [ -n "$control" ]; then printf '%s\n' "$control" > "$dir/$leg.control.txt"; else : > "$dir/$leg.control.txt"; fi
  echo "$verdict" > "$dir/$leg.verdict"
  echo "order-verdict=$verdict" >> "${GITHUB_OUTPUT:-/dev/null}"
  printf '### Order verdict (%s): %s\n- %s\n' "$leg" "$(echo "$verdict" | tr '[:lower:]' '[:upper:]')" "$line" >> "${GITHUB_STEP_SUMMARY:-/dev/null}"
  return "$status"
}

summary() { # <dir> [<seed>]
  local dir=$1 seed=${2:-} f l v any=""
  echo "### Order verdict, every leg"
  for f in "$dir"/*.verdict; do
    [ -f "$f" ] || continue
    any=1 l="$(basename "$f" .verdict)" v="$(cat "$f")"
    case "$v" in
      pass) echo "- \`$l\`: passed" ;;
      regression) echo "- \`$l\`: **REGRESSION, not an order dependence**: $(names < "$dir/$l.control.txt") fail in default order too" ;;
      order) echo "- \`$l\`: **order dependence (or a flake)**: $(names < "$dir/$l.txt") pass in default order" ;;
      *) echo "- \`$l\`: no order verdict (it failed naming no test)" ;;
    esac
  done
  [ -n "$any" ] || echo "- no leg recorded a verdict (none reached its tests: a preflight, its setup or the runner)"
  scala_compare "$dir" "$seed"
}

# Scala's two orders side by side: a suite failing in BOTH is a regression; one in one order alone a candidate.
scala_compare() { # <dir> [<seed>]
  local dir=$1 seed=${2:-} order both only_reverse only_shuffled lines=""
  for order in reverse shuffled; do [ -f "$dir/scala-$order.txt" ] || : > "$dir/scala-$order.txt"; done
  result() { cat "$dir/scala-$1.result" 2> /dev/null || echo "no record (the leg did not finish)"; }
  both="$(LC_ALL=C comm -12 "$dir/scala-reverse.txt" "$dir/scala-shuffled.txt")"
  only_reverse="$(LC_ALL=C comm -23 "$dir/scala-reverse.txt" "$dir/scala-shuffled.txt")"
  only_shuffled="$(LC_ALL=C comm -13 "$dir/scala-reverse.txt" "$dir/scala-shuffled.txt")"
  # a leg gives a verdict when it passed or named what failed; a suite failing in one order is compared only
  # against a leg that did, never against one cut short (a compile error, a timeout) before reaching it
  reached() { [ "$(result "$1")" = success ] || [ -s "$dir/scala-$1.txt" ]; }
  add() { lines="$lines- $1"$'\n'; }
  [ -z "$both" ] || add "**regression, not an order dependence: the same suite fails in both orders**: $(printf '%s\n' "$both" | names)"
  if [ -n "$only_reverse" ] && ! reached shuffled; then
    add "$(printf '%s\n' "$only_reverse" | names) failed in the reverse order; the shuffled leg reached no verdict to compare it with"
    only_reverse=""
  fi
  if [ -n "$only_shuffled" ] && ! reached reverse; then
    add "$(printf '%s\n' "$only_shuffled" | names) failed in the shuffled order; the reverse leg reached no verdict to compare it with"
    only_shuffled=""
  fi
  [ -z "$only_reverse" ] || add "**order dependence**: $(printf '%s\n' "$only_reverse" | names) fails only in the reverse order -- replay: \`TEST_ORDER_SEED=reverse sbt testUnitNoE2e\`"
  [ -z "$only_shuffled" ] || add "**order dependence**: $(printf '%s\n' "$only_shuffled" | names) fails only in the shuffled order -- replay: \`TEST_ORDER_SEED=${seed:-<seed>} sbt testUnitNoE2e\`"
  for order in reverse shuffled; do
    reached "$order" || add "**no order verdict** from the $order leg: it ended $(result "$order") naming no failing suite (a compile error, a timeout or the runner)"
  done
  [ -n "$lines" ] || lines="- both orders passed"$'\n'
  echo "### Scala suites, reverse vs shuffled"
  printf '%s' "$lines"
}

json_list() { grep . "$1" 2> /dev/null | sed 's/.*/"&"/' | paste -sd, - || true; }

run_verdict() { # <dir>
  local dir=$1 f l v word="" legs=""
  for f in "$dir"/*.verdict; do
    [ -f "$f" ] || continue
    l="$(basename "$f" .verdict)" v="$(cat "$f")"
    case "$v:$word" in
      regression:*) word=regression ;;
      order:regression) ;;
      order:*) word=order ;;
      none:regression|none:order) ;;
      none:*) word=none ;;
      *:) word=pass ;;
    esac
    legs="$legs${legs:+,}\"$l\":{\"verdict\":\"$v\",\"failed\":[$(json_list "$dir/$l.txt")],\"default_order_failed\":[$(json_list "$dir/$l.control.txt")]}"
  done
  word=${word:-none} # no leg recorded anything: no verdict
  printf '{"verdict":"%s","legs":{%s}}\n' "$word" "$legs" > "$dir/order-verdict.json"
  echo "$word"
}

case "${1:-}" in
  leg) shift; [ $# -ge 3 ] || { echo "usage: order-compare.sh leg <leg> <record dir> -- <command...>" >&2; exit 64; }; leg "$@" ;;
  control-command) shift; [ $# -ge 2 ] || { echo "usage: order-compare.sh control-command <leg> <test>..." >&2; exit 64; }; control_command "$@" ;;
  failed-suites) [ $# -eq 2 ] || { echo "usage: order-compare.sh failed-suites <sbt log>" >&2; exit 64; }; failed_suites "$2" ;;
  failed-junit) [ $# -eq 2 ] || { echo "usage: order-compare.sh failed-junit <dir>" >&2; exit 64; }; failed_junit "$2" ;;
  failed-xctest) [ $# -eq 2 ] || { echo "usage: order-compare.sh failed-xctest <log>" >&2; exit 64; }; failed_xctest "$2" ;;
  summary) [ $# -ge 2 ] || { echo "usage: order-compare.sh summary <dir> [<shuffled seed>]" >&2; exit 64; }; summary "$2" "${3:-}" ;;
  verdict) [ $# -eq 2 ] || { echo "usage: order-compare.sh verdict <dir>" >&2; exit 64; }; run_verdict "$2" ;;
  *) echo "usage: order-compare.sh leg|control-command|failed-suites|failed-junit|failed-xctest|summary|verdict ..." >&2; exit 64 ;;
esac
