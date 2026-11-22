#!/usr/bin/env bash
# Purpose: Every XCUITest class alone, each in its own `xcodebuild test-without-building` on the booted $DESTINATION
# Every XCUITest class ALONE, each in its own `xcodebuild test-without-building` on the booted $DESTINATION, so none
# can lean on state another class left behind (order-independence.yml's ios job, after a build-for-testing into
# build/dd). Run from the checkout:
#
#     DESTINATION=<an xcodebuild -destination> scripts/ci/xcuitest-each-class-alone.sh
#
# Every class runs even after a failure; the step then fails naming the classes. A failed class's group opens with
# an error per failed test carrying its own assertion at its file and line (xcresult_annotations.py), then the tail of
# its xcodebuild log: the first error a failed step prints is the line its red is known by. That is also why this is
# a script and not the workflow's `run:` block: GitHub echoes a `run:` block into the log, and its
# `echo "::error::..."` lines were read as the failure -- Order independence #28-#31 failed in four different ways
# and read as one red. Each test is bounded: a hung one fails at its allowance, naming itself.
# Result bundles and logs: build/alone/<class>.xcresult, build/alone/<class>.log (the runner's warm-up:
# build/alone/runner-warm-up.*). XCUITEST_WARMUP_TEST (default RepertoireLaunchUITests/testAppLaunches) is its test.
# XCUITEST_DIR (default ios/KinowoUITests) stands in for the classes' directory in its test.
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
: "${DESTINATION:?the simulator to run on, as an xcodebuild -destination}"
dir="${XCUITEST_DIR:-ios/KinowoUITests}"

# FIRST LAUNCH BEFORE THE FIRST CLASS: a fresh Simulator's first launch of the app is minutes of system work (Order
# independence #77: 148 s, and XCUITest's launch timed out; every later launch ~16 s), and it fell on whichever class
# sorts first, every run CardShareMenuUITests (#74 a press read as a tap in the same stall). The workflow boots the
# Simulator first; this installs the built app and launches it once, outside any test. A warm-up that fails or hangs
# only warns: the classes still say for themselves whether they pass.
# It launches as FixtureLaunch.intoGrid does (the UI-test flags, a pinned country, language and city, the fixture
# listing): a bare launch asked for the location (Order independence #82: "Allow “Showtimes” to use your location?" was
# still up when CardShareMenuUITests began, handled by XCUITest's "Don't Allow", and its second test's launch then
# failed to terminate the first's app). No test ever meets that prompt; the warm-up must not leave it either.
# shellcheck source=scripts/tool-conventions.sh
. "$here/../tool-conventions.sh"
udid="${DESTINATION##*id=}"
app="${XCUITEST_APP:-build/dd/Build/Products/Debug-iphonesimulator/Kinowo.app}"
bundle="${XCUITEST_BUNDLE_ID:-dev.kinowo.Kinowo}"
started=$SECONDS
# simctl hands a SIMCTL_CHILD_<name> variable to the app as <name>
launch_as_a_test() {
    SIMCTL_CHILD_KINOWO_UITEST_FIXTURE=1 SIMCTL_CHILD_KINOWO_FORCE_DETECTED_CITY=warszawa \
        bounded 300 xcrun simctl launch "$udid" "$bundle" -UITests 1 -selectedCountryCode pl -selectedLanguageCode pl \
        -AppleLanguages "(pl)" -AppleLocale pl -selectedCity warszawa
}
if bounded 300 xcrun simctl install "$udid" "$app" && launch_as_a_test; then
    echo "warmed the Simulator: $bundle installed and launched once in $((SECONDS - started))s"
    bounded 60 xcrun simctl terminate "$udid" "$bundle" || echo "::warning::could not end the warm-up launch of $bundle"
else
    echo "::warning::the Simulator warm-up launch of $bundle failed after $((SECONDS - started))s: the first class pays the first launch"
fi

mkdir -p build/alone

# AND THE TEST RUNNER'S FIRST RUN BEFORE THE FIRST CLASS (Order independence #115): the app warm, XCUITest's own first
# run on a fresh Simulator -- its runner app's install and launch, the automation session, the accessibility stack --
# still cost ~70 s, and it fell inside whichever class sorts first: CardShareMenuUITests' long-press test took 81 s
# (13.7 s warm) and its hold read as no press, labelled ORDER by the control pass though no class ran before it. One
# throwaway run of a quick test pays it; it only warns, its result counts for nothing.
warm_test="${XCUITEST_WARMUP_TEST:-RepertoireLaunchUITests/testAppLaunches}"
started=$SECONDS
if bounded 600 xcodebuild test-without-building \
    -project ios/Kinowo.xcodeproj \
    -scheme Kinowo \
    -destination "$DESTINATION" \
    -derivedDataPath build/dd \
    -only-testing:"KinowoUITests/$warm_test" \
    -resultBundlePath build/alone/runner-warm-up.xcresult \
    CODE_SIGNING_ALLOWED=NO > build/alone/runner-warm-up.log 2>&1; then
    echo "warmed the XCUITest runner: $warm_test once in $((SECONDS - started))s"
else
    echo "::warning::the XCUITest runner warm-up ($warm_test) failed after $((SECONDS - started))s: the first class pays the runner's first run"
fi
classes=$(grep -hoE 'class[[:space:]]+[A-Za-z0-9_]+[[:space:]]*:[[:space:]]*XCTestCase' "$dir"/*.swift \
    | sed -E 's/class[[:space:]]+([A-Za-z0-9_]+).*/\1/' | sort -u)
[ -n "$classes" ] || { echo "::error::no XCUITest classes found in $dir"; exit 1; }
failed=""
for class in $classes; do
    echo "::group::$class"
    started=$SECONDS
    if xcodebuild test-without-building \
        -project ios/Kinowo.xcodeproj \
        -scheme Kinowo \
        -destination "$DESTINATION" \
        -derivedDataPath build/dd \
        -only-testing:"KinowoUITests/$class" \
        -resultBundlePath "build/alone/$class.xcresult" \
        -test-timeouts-enabled YES \
        -default-test-execution-time-allowance 180 \
        -maximum-test-execution-time-allowance 300 \
        CODE_SIGNING_ALLOWED=NO > "build/alone/$class.log" 2>&1; then
        echo "$class passed alone in $((SECONDS - started))s"
    else
        echo "$class FAILED alone after $((SECONDS - started))s"
        python3 "$here/xcresult_annotations.py" --level error "build/alone/$class.xcresult"
        tail -80 "build/alone/$class.log"
        failed="$failed $class"
    fi
    echo "::endgroup::"
done
if [ -n "$failed" ]; then
    echo "XCUITest classes that fail when run alone:$failed" >> "${GITHUB_STEP_SUMMARY:-/dev/null}"
    echo "::error::XCUITest classes that fail when run alone:$failed"
    # the failing classes, one a line, for Order independence's control pass (scripts/ci/order_compare.py leg)
    [ -z "${ORDER_FAILED_FILE:-}" ] || printf '%s\n' $failed > "$ORDER_FAILED_FILE"
    exit 1
fi
