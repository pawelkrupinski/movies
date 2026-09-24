#!/usr/bin/env bash
# Compile the iOS code the two ways CI does and a Mac's `swift test` does not.
#
#   1. The APP TARGET, SwiftUI views included, with `xcodebuild build`. `swift test` builds the
#      SwiftPM view of the sources, which leaves out every SwiftUI file (Views/, ContentView,
#      KinowoApp — see ios/Package.swift), so a view that does not compile passes it. That is how
#      an ambiguous SwiftUI initializer compiled locally and failed only in CI (run 35088520259).
#      ios.yml's `ui-tests` job runs the same build on its macOS runner.
#
#   2. The SwiftPM package on LINUX, in the same swift:5.10 image ios.yml's `unit-integration`
#      job uses. swift-corelibs-foundation is not Apple's Foundation: `Locale.language` exists on
#      a Mac and not there, which compiled locally and failed only in CI (run 35431061855).
#      Needs Docker; the build directory lives in a named volume, so the second run is incremental.
#
# Usage: scripts/ios-compile-check.sh [--app-only | --linux-only]
# The pre-push hook runs this when a push touches ios/**. Either half is skipped, with a warning,
# when its toolchain is missing (no full Xcode, no Docker daemon) — never silently.
set -uo pipefail

cd "$(git rev-parse --show-toplevel)" || exit 1

mode="${1:-both}"
status=0

log() { printf '\033[36m▸\033[0m %s\n' "$*"; }
warn() { printf '\033[33m!\033[0m %s\n' "$*" >&2; }

app_build() {
    if ! xcodebuild -version >/dev/null 2>&1; then
        warn "xcodebuild unavailable (no full Xcode selected) — SKIPPED the app-target build."
        return 0
    fi
    log "xcodebuild build: the Kinowo app target, SwiftUI included"
    local out
    out="$(mktemp)"
    if xcodebuild build \
        -project ios/Kinowo.xcodeproj \
        -scheme Kinowo \
        -destination 'generic/platform=iOS Simulator' \
        -derivedDataPath ios/.build/compile-check-derived-data \
        CODE_SIGNING_ALLOWED=NO >"$out" 2>&1; then
        log "app target compiles"
    else
        grep -E 'error:|BUILD FAILED' "$out" | head -40 >&2
        warn "full log: $out"
        return 1
    fi
}

linux_build() {
    if ! docker info >/dev/null 2>&1; then
        warn "no Docker daemon — SKIPPED the Linux swift build (CI's unit-integration job still runs it)."
        return 0
    fi
    log "swift build --build-tests on Linux (swift:5.10, as ios.yml)"
    # The sources read-only; the build products in a named volume, so a Linux .build never
    # lands in ios/.build next to the Mac one.
    local out
    out="$(mktemp)"
    if docker run --rm \
        -v "$PWD/ios:/src:ro" \
        -v kinowo-ios-linux-build:/scratch \
        swift:5.10 \
        swift build --build-tests --package-path /src --scratch-path /scratch >"$out" 2>&1; then
        log "the package compiles on Linux"
    else
        grep -E 'error:' "$out" | head -40 >&2
        warn "full log: $out"
        return 1
    fi
}

case "$mode" in
    --app-only)   app_build || status=1 ;;
    --linux-only) linux_build || status=1 ;;
    both)         app_build || status=1; linux_build || status=1 ;;
    *)            echo "usage: $0 [--app-only | --linux-only]" >&2; exit 2 ;;
esac

exit $status
