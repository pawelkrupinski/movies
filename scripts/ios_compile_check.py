#!/usr/bin/env python3
"""Compile the iOS code the ways CI does and a Mac's `swift test` does not.

Compile the iOS code the ways CI does and a Mac's `swift test` does not.

  1. The APP TARGET, SwiftUI views included, and its XCUITests, with `xcodebuild
     build-for-testing`. `swift test` builds the SwiftPM view of the sources, which leaves out
     every SwiftUI file (Views/, ContentView, KinowoApp — see ios/Package.swift), so a view
     that does not compile passes it. That is how
     an ambiguous SwiftUI initializer compiled locally and failed only in CI (run 35088520259).
     ios.yml's `ui-tests` job builds the same on its macOS runner.

  2. The SwiftPM package on the MAC, test targets included (`swift build --build-tests`), for
     its concurrency diagnostics (below): the macOS-only targets (KinowoAuth, KinowoNetworking
     and their tests) are checked nowhere else.

  3. The SwiftPM package on LINUX, in the same swift:6.2 image ios.yml's `unit-integration`
     job uses. swift-corelibs-foundation is not Apple's Foundation: `Locale.language` exists on
     a Mac and not there, which compiled locally and failed only in CI (run 35431061855).
     Needs Docker; the build directory lives in a named volume, so the second run is incremental.

All three build in the Swift 6 language mode (SWIFT_VERSION = 6.0 in the pbxproj, tools 6.0 in
Package.swift), so complete concurrency checking runs everywhere and a data race is an error; the
check also FAILS on any concurrency WARNING (a `@preconcurrency` import, a Sendable note the
mode keeps as a warning), so the count stays at zero. A file compiled clean is not recompiled by
the next incremental run, so the check sees every file that changed, and every file whose
dependencies' interfaces changed.

Every mode first checks the iOS deployment target: the Xcode project (each build configuration) and
ios/Package.swift's `.iOS(.vN)` must name one floor, or the SwiftPM builds check the sources against a
platform the app does not ship to. IOS_DIR (default ios) points the check at another tree in tests.

Usage: scripts/ios-compile-check.sh [--app-only | --spm-only | --linux-only]
The pre-push hook runs this when a push touches ios/**. Each part is skipped, with a warning,
when its toolchain is missing (no full Xcode, no swift, no Docker daemon) — never silently."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import tempfile
from typing import IO, Callable, Dict, List, Optional, Sequence

USAGE = "usage: scripts/ios-compile-check.sh [--app-only | --spm-only | --linux-only]"
SWIFT_IMAGE = "swift:6.2"
# The diagnostics complete concurrency checking emits: Sendable, actor isolation, data races, global mutable state,
# @preconcurrency. Every one of them names one of these.
CONCURRENCY_DIAGNOSTIC = re.compile(r"^/[^ ]+:[0-9]+:[0-9]+: (warning|error): .*([Ss]endable|isolat|[Mm]ain actor|"
                                    r"global actor|concurren|data race|preconcurrency|sending )")
COLOURS = re.compile(r"\x1b\[[0-9;]*m|\x1b\]8;;[^\x1b]*\x1b\\")
LINUX_BUILD = ("mkdir -p /src && tar -xf - -C /src --strip-components=1 && exec swift build --build-tests "
               "--package-path /src --scratch-path /scratch")


def log(text: str) -> None:
    print(f"\033[36m▸\033[0m {text}", flush=True)


def warn(text: str) -> None:
    print(f"\033[33m!\033[0m {text}", file=sys.stderr, flush=True)


def answers(argv: Sequence[str]) -> bool:
    """Whether a toolchain's version probe answers (its output dropped)."""
    try:
        return subprocess.run(list(argv), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0
    except OSError:
        return False


def concurrency_diagnostics(text: str) -> List[str]:
    """Each concurrency diagnostic in a build log once (a build prints some twice), colours stripped."""
    return sorted({line for line in COLOURS.sub("", text).splitlines() if CONCURRENCY_DIAGNOSTIC.match(line)})


def built(what: str, ok: str, errors: str, argv: Sequence[str], stdin: Optional[IO[bytes]] = None) -> bool:
    """The command, its output kept aside: removed once it compiles with no concurrency diagnostic, kept (its path
    said) and its matching lines shown when it does not."""
    handle, out = tempfile.mkstemp(prefix=f"ios-compile-check-{what}.", dir=os.environ.get("TMPDIR") or "/tmp")
    with os.fdopen(handle, "wb") as sink:
        try:
            code = subprocess.run(list(argv), stdin=stdin, stdout=sink, stderr=subprocess.STDOUT).returncode
        except OSError as e:
            sink.write(f"{argv[0]}: {e}\n".encode())
            code = 127
    with open(out, errors="replace") as f:
        text = f.read()
    if code != 0:
        sys.stderr.write("".join([f"{line}\n" for line in text.splitlines() if re.search(errors, line)][:40]))
        warn(f"full log: {out}")
        return False
    found = concurrency_diagnostics(text)
    if found:
        sys.stderr.write("".join(f"{line}\n" for line in found[:40]))
        warn(f"{len(found)} concurrency diagnostic(s) — fix them (ios/Package.swift says how the checking is set up);"
             f" full log: {out}")
        return False
    os.remove(out)
    log(ok)
    return True


def matches(path: str, pattern: str, value: Callable[["re.Match[str]"], str]) -> List[str]:
    """What `pattern` reads from each line of the file at `path` that it matches; none when the file is missing."""
    try:
        with open(path, errors="replace") as f:
            return [value(m) for m in map(re.compile(pattern).match, f.read().splitlines()) if m]
    except OSError:
        return []


def deployment_target() -> bool:
    """The one iOS floor the project's build configurations and the package manifest agree on, or a failure naming
    both."""
    directory = os.environ.get("IOS_DIR") or "ios"
    project = "".join(f"{v} " for v in sorted(set(matches(
        os.path.join(directory, "Kinowo.xcodeproj", "project.pbxproj"),
        r".*IPHONEOS_DEPLOYMENT_TARGET = ([0-9.]+);", lambda m: m.group(1)))))
    manifest = "\n".join(matches(os.path.join(directory, "Package.swift"), r".*\.iOS\(\.v([0-9]+)\)",
                                 lambda m: f"{m.group(1)}.0"))
    if not manifest or project != f"{manifest} ":
        warn(f"iOS deployment targets disagree: project.pbxproj says {project or 'nothing'}, Package.swift "
             f"{manifest or 'nothing'} — raise both together")
        return False
    log(f"iOS deployment target {manifest} in the project and the package")
    return True


def app_build() -> bool:
    if not answers(["xcodebuild", "-version"]):
        warn("xcodebuild unavailable (no full Xcode selected) — SKIPPED the app-target build.")
        return True
    log("xcodebuild build-for-testing: the Kinowo app target, SwiftUI included, and its XCUITests")
    return built("app", "app target compiles", "error:|BUILD FAILED", [
        "xcodebuild", "build-for-testing", "-project", "ios/Kinowo.xcodeproj", "-scheme", "Kinowo", "-destination",
        "generic/platform=iOS Simulator", "-derivedDataPath", "ios/.build/compile-check-derived-data",
        "CODE_SIGNING_ALLOWED=NO"])


def spm_build() -> bool:
    if not answers(["swift", "--version"]):
        warn("no swift toolchain — SKIPPED the Mac SwiftPM build.")
        return True
    log("swift build --build-tests: the SwiftPM package on this Mac, test targets included")
    return built("spm", "the package compiles on the Mac", "error:",
                 ["swift", "build", "--build-tests", "--package-path", "ios"])


def linux_build() -> bool:
    """The sources (tracked and untracked, never ignored: no Mac ios/.build) come in on stdin and are unpacked at
    /src, not mounted: Rancher Desktop's VM keeps a host descriptor for every file and directory a container saw
    through the shared home, the worktree deleted or not (run-script-tests' linux_tree_slot says what that cost). The
    build products in a named volume, so a Linux .build never lands in ios/.build next to the Mac one."""
    if not answers(["docker", "info"]):
        warn("no Docker daemon — SKIPPED the Linux swift build (CI's unit-integration job still runs it).")
        return True
    log(f"swift build --build-tests on Linux ({SWIFT_IMAGE}, as ios.yml)")
    with tempfile.TemporaryFile() as sources:
        files = subprocess.run(["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", "ios"],
                               stdout=subprocess.PIPE)
        tar = subprocess.run(["tar", "--no-xattrs", "--null", "-T", "-", "-cf", "-"], input=files.stdout,
                             stdout=sources, env={**os.environ, "COPYFILE_DISABLE": "1"})
        if files.returncode != 0 or tar.returncode != 0:
            return False
        sources.seek(0)
        return built("linux", "the package compiles on Linux", "error:", [
            "docker", "run", "--rm", "-i", "-v", "kinowo-ios-linux6-build:/scratch", SWIFT_IMAGE, "sh", "-c",
            LINUX_BUILD], stdin=sources)


MODES: Dict[str, List[Callable[[], bool]]] = {
    "--app-only": [app_build], "--spm-only": [spm_build], "--linux-only": [linux_build],
    "all": [app_build, spm_build, linux_build]}


def main(argv: List[str]) -> int:
    top = subprocess.run(["git", "rev-parse", "--show-toplevel"], stdout=subprocess.PIPE, text=True)
    if top.returncode != 0:
        return 1
    os.chdir(top.stdout.strip())
    mode = argv[0] if argv else "all"
    if mode not in MODES:
        print(USAGE, file=sys.stderr)
        return 2
    results = [deployment_target()] + [build() for build in MODES[mode]]
    return 0 if all(results) else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
