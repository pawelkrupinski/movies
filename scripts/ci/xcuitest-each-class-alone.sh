#!/usr/bin/env bash
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
# Result bundles and logs: build/alone/<class>.xcresult, build/alone/<class>.log.
# XCUITEST_DIR (default ios/KinowoUITests) stands in for the classes' directory in its test.
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
: "${DESTINATION:?the simulator to run on, as an xcodebuild -destination}"
dir="${XCUITEST_DIR:-ios/KinowoUITests}"

mkdir -p build/alone
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
    exit 1
fi
