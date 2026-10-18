#!/usr/bin/env bash
# The assertion harness the shell test scripts share.
#
# WHY IT EXISTS. Four test scripts — scripts/mobile-release-test.sh,
# android/scripts/store-screenshots-test.sh, android/scripts/store-graphics-test.sh
# and ios/scripts/store-screenshots-test.sh — carried a BYTE-IDENTICAL `check`,
# the same `fails=0`, and the same summary-and-exit line. Identical enough that
# their four copies hashed the same.
#
# It is deliberately NOT in store-screenshots-common.sh, which three of the four
# already reach: that file is about locating candidates, baselines and locales
# for the store screenshots. A test harness living inside it would make "the
# screenshot library" also mean "the way we assert things", and the next script
# that wants only assertions would have to drag screenshots in to get them.
#
# scripts/local-mirror/mirror-resilience-test.sh is intentionally left out — its
# harness is a genuinely different one (it reports per-case rather than
# accumulating), and rewriting a passing spec to fit a shared shape is churn, not
# deduplication.
#
# Usage:
#   . "$REPO_ROOT/scripts/shell-spec.sh"
#   check "what it does" "$expected" "$actual"
#   spec_summary            # prints the tally and exits non-zero on any failure

# The JVM gate (scripts/jvm-gate.sh) reads the real Mac's paging and takes machine-wide slots, which a spec's stubbed
# JVM starts have no business waiting on, and sizes JVMs for a share of the cores, which a spec's command lines would
# then vary by: all off in every spec unless it asks for it (jvm-gate-test.sh does).
export KINOWO_MEMORY_GATE=${KINOWO_MEMORY_GATE:-0} KINOWO_JVM_SLOTS=${KINOWO_JVM_SLOTS:-0} KINOWO_JVM_CPUS=${KINOWO_JVM_CPUS:-0}

# Failure tally. Set here rather than in each caller, which is the third line all
# four repeated.
fails=0

# check <description> <expected> <actual>
check() {
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

# Print the tally and exit with the shell's usual convention: 0 clean, 1 if
# anything failed. Callers end with this instead of repeating the ternary.
spec_summary() {
  if [ "$fails" -eq 0 ]; then printf '\033[32m✓\033[0m all passed\n'
  else printf '\033[31m✗\033[0m %s failed\n' "$fails"; fi
  exit $((fails > 0))
}
