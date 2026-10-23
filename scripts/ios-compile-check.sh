#!/usr/bin/env bash
# Compile the iOS code the ways CI does and a Mac's `swift test` does not.
#
#   1. The APP TARGET, SwiftUI views included, and its XCUITests, with `xcodebuild
#      build-for-testing`. `swift test` builds the SwiftPM view of the sources, which leaves out
#      every SwiftUI file (Views/, ContentView, KinowoApp — see ios/Package.swift), so a view
#      that does not compile passes it. That is how
#      an ambiguous SwiftUI initializer compiled locally and failed only in CI (run 35088520259).
#      ios.yml's `ui-tests` job builds the same on its macOS runner.
#
#   2. The SwiftPM package on the MAC, test targets included (`swift build --build-tests`), for
#      its concurrency diagnostics (below): the macOS-only targets (KinowoAuth, KinowoNetworking
#      and their tests) are checked nowhere else.
#
#   3. The SwiftPM package on LINUX, in the same swift:6.2 image ios.yml's `unit-integration`
#      job uses. swift-corelibs-foundation is not Apple's Foundation: `Locale.language` exists on
#      a Mac and not there, which compiled locally and failed only in CI (run 35431061855).
#      Needs Docker; the build directory lives in a named volume, so the second run is incremental.
#
# All three build in the Swift 6 language mode (SWIFT_VERSION = 6.0 in the pbxproj, tools 6.0 in
# Package.swift), so complete concurrency checking runs everywhere and a data race is an error; the
# check also FAILS on any concurrency WARNING (a `@preconcurrency` import, a Sendable note the
# mode keeps as a warning), so the count stays at zero. A file compiled clean is not recompiled by
# the next incremental run, so the check sees every file that changed, and every file whose
# dependencies' interfaces changed.
#
# Usage: scripts/ios-compile-check.sh [--app-only | --spm-only | --linux-only]
# The pre-push hook runs this when a push touches ios/**. Each part is skipped, with a warning,
# when its toolchain is missing (no full Xcode, no swift, no Docker daemon) — never silently.
set -uo pipefail

cd "$(git rev-parse --show-toplevel)" || exit 1

mode="${1:-all}"
status=0

log() { printf '\033[36m▸\033[0m %s\n' "$*"; }
warn() { printf '\033[33m!\033[0m %s\n' "$*" >&2; }

# The diagnostics complete concurrency checking emits: Sendable, actor isolation, data races, global
# mutable state, @preconcurrency. Every one of them names one of these.
CONCURRENCY_DIAGNOSTIC='(warning|error): .*([Ss]endable|isolat|[Mm]ain actor|global actor|concurren|data race|preconcurrency|sending )'

# concurrency_diagnostics <log> -> each concurrency diagnostic in a build log once (a build prints some twice),
# colours stripped
concurrency_diagnostics() {
    sed -E $'s/\x1b\\[[0-9;]*m//g; s/\x1b\\]8;;[^\x1b]*\x1b\\\\//g' "$1" \
        | grep -E "^/[^ ]+:[0-9]+:[0-9]+: $CONCURRENCY_DIAGNOSTIC" | sort -u
}

# built <what> <ok message> <error pattern> <command...> -> the command, its output kept aside: removed once it
# compiles with no concurrency diagnostic, kept (its path said) and its matching lines shown when it does not
built() {
    local what=$1 ok=$2 errors=$3 out found
    shift 3
    out="$(mktemp "${TMPDIR:-/tmp}/ios-compile-check-$what.XXXXXX")" || return 1
    if ! "$@" >"$out" 2>&1; then
        grep -E "$errors" "$out" | head -40 >&2
        warn "full log: $out"
        return 1
    fi
    found="$(concurrency_diagnostics "$out")"
    if [ -n "$found" ]; then
        printf '%s\n' "$found" | head -40 >&2
        warn "$(printf '%s\n' "$found" | wc -l | tr -d ' ') concurrency diagnostic(s) — fix them (ios/Package.swift says how the checking is set up); full log: $out"
        return 1
    fi
    rm -f "$out"
    log "$ok"
}

app_build() {
    if ! xcodebuild -version >/dev/null 2>&1; then
        warn "xcodebuild unavailable (no full Xcode selected) — SKIPPED the app-target build."
        return 0
    fi
    log "xcodebuild build-for-testing: the Kinowo app target, SwiftUI included, and its XCUITests"
    built app "app target compiles" 'error:|BUILD FAILED' xcodebuild build-for-testing \
        -project ios/Kinowo.xcodeproj \
        -scheme Kinowo \
        -destination 'generic/platform=iOS Simulator' \
        -derivedDataPath ios/.build/compile-check-derived-data \
        CODE_SIGNING_ALLOWED=NO
}

spm_build() {
    if ! swift --version >/dev/null 2>&1; then
        warn "no swift toolchain — SKIPPED the Mac SwiftPM build."
        return 0
    fi
    log "swift build --build-tests: the SwiftPM package on this Mac, test targets included"
    built spm "the package compiles on the Mac" 'error:' swift build --build-tests --package-path ios
}

linux_build() {
    if ! docker info >/dev/null 2>&1; then
        warn "no Docker daemon — SKIPPED the Linux swift build (CI's unit-integration job still runs it)."
        return 0
    fi
    log "swift build --build-tests on Linux (swift:6.2, as ios.yml)"
    # The sources read-only; the build products in a named volume, so a Linux .build never
    # lands in ios/.build next to the Mac one.
    built linux "the package compiles on Linux" 'error:' docker run --rm \
        -v "$PWD/ios:/src:ro" \
        -v kinowo-ios-linux6-build:/scratch \
        swift:6.2 \
        swift build --build-tests --package-path /src --scratch-path /scratch
}

case "$mode" in
    --app-only)   app_build || status=1 ;;
    --spm-only)   spm_build || status=1 ;;
    --linux-only) linux_build || status=1 ;;
    all)          app_build || status=1; spm_build || status=1; linux_build || status=1 ;;
    *)            echo "usage: $0 [--app-only | --spm-only | --linux-only]" >&2; exit 2 ;;
esac

exit $status
