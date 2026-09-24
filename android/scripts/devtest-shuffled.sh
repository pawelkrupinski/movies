#!/usr/bin/env bash
#
# Every instrumented (androidTest) class ALONE, in an order drawn from a seed —
# the emulator lane of the nightly order-independence workflow. The ordinary
# connectedDebugAndroidTest run always meets the classes in one order, on one
# app install whose prefs and caches carry over from class to class, so a class
# that passes only because another ran first looks healthy forever. Each class
# here gets its own connectedDebugAndroidTest run (devtest.sh — installed
# fresh and uninstalled after), in a seeded order.
# Reproduce a failure with the seed it prints:
#
#     android/scripts/devtest-shuffled.sh <seed> [--skip-live-youtube]
#
set -uo pipefail
seed="${1:?usage: devtest-shuffled.sh <seed> [--skip-live-youtube]}"
shift
android="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$android/../scripts/seeded-order.sh"
sources="$android/app/src/androidTest/java"

# Every file with a JUnit @Test, as a class name (one top-level test class per file).
classes="$(grep -rl --include='*.kt' '@Test' "$sources" | sed -e "s|^$sources/||" -e 's|\.kt$||' -e 's|/|.|g' | sort)"
[ -n "$classes" ] || { echo "devtest-shuffled: no androidTest classes under $sources" >&2; exit 1; }
ordered="$(printf '%s\n' "$classes" | seeded_order "$seed")"
count="$(printf '%s\n' "$ordered" | wc -l | tr -d ' ')"
echo "devtest-shuffled: seed $seed — $count classes, each alone — reproduce with android/scripts/devtest-shuffled.sh $seed $*"

failed=()
while IFS= read -r class; do
    echo "::group::$class"
    # </dev/null: stdin is the class list this loop reads, and adb and Gradle's client
    # both read theirs — a run would swallow every class after it.
    if ! "$android/scripts/devtest.sh" "$@" "$class" </dev/null; then failed+=("$class"); fi
    echo "::endgroup::"
done <<<"$ordered"

if [ "${#failed[@]}" -gt 0 ]; then
    echo "devtest-shuffled: ${#failed[@]} of $count classes fail alone under seed $seed:"
    printf '  %s\n' "${failed[@]}"
    exit 1
fi
echo "devtest-shuffled: all $count classes pass alone under seed $seed"
