#!/usr/bin/env bash
# The Order independence workflow's scala verdict: which suites failed in each order, and what that says. A suite that
# fails in BOTH orders (reverse and shuffled) is a regression, not an order dependence: it fails whatever ran before it.
# Of Order independence's ~66 failing jobs over runs #1-#100, ~31 were such regressions and one a real order dependence
# (2026-11-22 analysis), yet each read as an order red. Only a suite failing in one order alone is an order-dependence
# candidate; a leg that failed naming no suite (a compile error, a timeout, the runner) gives no order verdict at all.
#
#   scripts/ci/order-compare.sh failed-suites <sbt log>            the suites sbt names under `Failed tests:` /
#                                                                  `Error during tests:`, one a line, sorted
#   scripts/ci/order-compare.sh summary <dir> [<shuffled seed>]    the verdict as markdown, from <dir>'s
#                                                                  <order>.txt (failed-suites') and <order>.result (the
#                                                                  unit step's outcome) for reverse and shuffled
#
# summary prints a heading and a bullet a finding, each one of:
#   regression, not an order dependence: the same suite fails in both orders: `A`, `B`
#   order dependence: `C` fails only in the <order> order ...
#   no order verdict: ...
#   both orders passed
# (scripts/ci/order-compare-test.sh)
set -euo pipefail

case "${1:-}" in
  failed-suites)
    [ $# -eq 2 ] || { echo "usage: order-compare.sh failed-suites <sbt log>" >&2; exit 64; }
    [ -f "$2" ] || exit 0 # no log: the leg never ran sbt, nothing named
    # ANSI colours off, then the indented names under each heading, until the next line that is not one
    sed $'s/\x1b\\[[0-9;]*[A-Za-z]//g' "$2" | awk '
      /^\[error\][[:space:]]+(Failed tests|Error during tests):[[:space:]]*$/ { inside = 1; next }
      inside && /^\[error\][[:space:]]+[A-Za-z_][A-Za-z0-9_.$]*[[:space:]]*$/ { sub(/^\[error\][[:space:]]+/, ""); sub(/[[:space:]]+$/, ""); print; next }
      { inside = 0 }' | LC_ALL=C sort -u
    ;;
  summary)
    [ $# -ge 2 ] || { echo "usage: order-compare.sh summary <dir> [<shuffled seed>]" >&2; exit 64; }
    dir=$2 seed=${3:-}
    for order in reverse shuffled; do [ -f "$dir/$order.txt" ] || : > "$dir/$order.txt"; done
    result() { cat "$dir/$1.result" 2> /dev/null || echo "no record (the leg did not finish)"; }
    names() { sed 's/.*/`&`/' | paste -sd, - | sed 's/,/, /g'; } # stdin's suites, a line each, as one list
    both="$(LC_ALL=C comm -12 "$dir/reverse.txt" "$dir/shuffled.txt")"
    only_reverse="$(LC_ALL=C comm -23 "$dir/reverse.txt" "$dir/shuffled.txt")"
    only_shuffled="$(LC_ALL=C comm -13 "$dir/reverse.txt" "$dir/shuffled.txt")"
    # a leg gives a verdict when it passed or named what failed; a suite failing in one order is compared only
    # against a leg that did, never against one cut short (a compile error, a timeout) before reaching it
    verdict() { [ "$(result "$1")" = success ] || [ -s "$dir/$1.txt" ]; }
    lines=()
    [ -n "$both" ] && lines+=("**regression, not an order dependence: the same suite fails in both orders**: $(printf '%s\n' "$both" | names)")
    if [ -n "$only_reverse" ] && ! verdict shuffled; then
      lines+=("$(printf '%s\n' "$only_reverse" | names) failed in the reverse order; the shuffled leg reached no verdict to compare it with")
      only_reverse=""
    fi
    if [ -n "$only_shuffled" ] && ! verdict reverse; then
      lines+=("$(printf '%s\n' "$only_shuffled" | names) failed in the shuffled order; the reverse leg reached no verdict to compare it with")
      only_shuffled=""
    fi
    [ -n "$only_reverse" ] && lines+=("**order dependence**: $(printf '%s\n' "$only_reverse" | names) fails only in the reverse order -- replay: \`TEST_ORDER_SEED=reverse sbt testUnitNoE2e\`")
    [ -n "$only_shuffled" ] && lines+=("**order dependence**: $(printf '%s\n' "$only_shuffled" | names) fails only in the shuffled order -- replay: \`TEST_ORDER_SEED=${seed:-<seed>} sbt testUnitNoE2e\`")
    for order in reverse shuffled; do
      if ! verdict "$order"; then
        lines+=("**no order verdict** from the $order leg: it ended $(result "$order") naming no failing suite (a compile error, a timeout or the runner)")
      fi
    done
    [ ${#lines[@]} -gt 0 ] || lines=("both orders passed")
    echo "### Scala suites, reverse vs shuffled"
    printf -- '- %s\n' "${lines[@]}"
    ;;
  *) echo "usage: order-compare.sh failed-suites <sbt log> | summary <dir> [<shuffled seed>]" >&2; exit 64 ;;
esac
