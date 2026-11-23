#!/usr/bin/env python3
"""Archive, export and upload the iOS app to App Store Connect.

Archive, export and upload the iOS app to App Store Connect.

  scripts/ios-release.sh              # test → archive → export → validate → upload
  scripts/ios-release.sh --no-upload  # stop after validate (dry run)

This lane had no script for three releases running: each one was rebuilt by
hand from a memory note, which is how the ExportOptions plist and the four
signing flags kept having to be rediscovered. `xcodebuild archive` needs manual
signing here — the automatic style picks a development certificate and the
export then fails on a mismatch — and `altool` lives inside the Xcode toolchain
rather than on PATH.

The version comes from `mobile-version.txt` via `mobile-release.sh`; this
script never sets it, so run the bump first and this only builds what is
already committed.

Credentials: the App Store Connect API key from the Touch ID vault — run
`secrets unlock movies` first (scripts/app-store-credentials.sh's load_app_store_credentials, which this runs).
IOS_RELEASE_BUILD_DIR (default ios/build/release) is where every log and the ipa land."""
from __future__ import annotations

import os
import re
import shutil
import signal
import subprocess
import sys
from types import FrameType
from typing import List, Mapping, Optional, Sequence, Tuple

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TEAM_ID = "CQ4YC43YDM"
BUNDLE_ID = "dev.kinowo.Kinowo"
PROFILE = "Kinowo App Store"
# The iOS SDK an app is built against decides how much of the unfolded iPhone Duo's inner display it gets: older than
# 27.0 runs letterboxed in a black frame, 27.0 stops short of the status bar, 27.1 reaches the edges. `SDKROOT =
# iphoneos` means "whatever the selected Xcode ships", so check it rather than trust it.
MIN_IOS_SDK = "27.1"
EXPORT_OPTIONS = f"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>method</key><string>app-store-connect</string>
  <key>teamID</key><string>{TEAM_ID}</string>
  <key>signingStyle</key><string>manual</string>
  <key>provisioningProfiles</key>
  <dict><key>{BUNDLE_ID}</key><string>{PROFILE}</string></dict>
  <key>uploadSymbols</key><true/>
  <key>destination</key><string>export</string>
</dict>
</plist>
"""
# The credentials, loaded by the one implementation (app-store-credentials.sh, sourced in a bash of its own): its key
# directory kept past that bash (`trap - EXIT`), this process removing it instead
LOAD_CREDENTIALS = ('. "$1/scripts/log.sh"; . "$1/scripts/app-store-credentials.sh"; load_app_store_credentials; '
                    'trap - EXIT; printf "%s\\n%s\\n%s\\n" "$APP_STORE_KEY_ID" "$APP_STORE_ISSUER_ID" '
                    '"$API_PRIVATE_KEYS_DIR"')


class Died(Exception):
    """A step failed: its message said (log.sh's die), exit 1."""


def say(text: str) -> None:
    print(f"\033[36m▸\033[0m {text}", flush=True)


def ok(text: str) -> None:
    print(f"\033[32m✓\033[0m {text}", flush=True)


def ended(signum: int, _frame: Optional[FrameType]) -> None:
    raise SystemExit(128 + signum)


def version_key(version: str) -> Tuple[int, ...]:
    """`sort -V`'s order for dotted numbers: 27.10 after 27.9."""
    return tuple(int(part) if part.isdigit() else -1 for part in version.split("."))


def run_logged(argv: Sequence[str], log: str, why: str, env: Optional[Mapping[str, str]] = None) -> None:
    """The command, stdout and stderr into `log`; Died(`<why> — see <log>`) when it fails."""
    with open(log, "wb") as sink:
        try:
            code = subprocess.run(list(argv), stdout=sink, stderr=subprocess.STDOUT,
                                  env=dict(env) if env is not None else None).returncode
        except OSError as e:
            sink.write(f"{argv[0]}: {e}\n".encode())
            code = 127
    if code != 0:
        raise Died(f"{why} — see {log}")


def check_sdk() -> None:
    try:
        found = subprocess.run(["xcrun", "--sdk", "iphoneos", "--show-sdk-version"], capture_output=True, text=True)
    except OSError:
        raise Died("no iOS SDK found — is full Xcode selected?") from None
    sdk = found.stdout.strip()
    if found.returncode != 0:
        raise Died("no iOS SDK found — is full Xcode selected?")
    if version_key(sdk) < version_key(MIN_IOS_SDK):
        raise Died(f"iOS SDK {sdk} is older than {MIN_IOS_SDK} — the build would not fill the iPhone Duo screen; select "
                   f"Xcode {MIN_IOS_SDK} or newer")


def credentials() -> Tuple[str, str, str]:
    """APP_STORE_KEY_ID, APP_STORE_ISSUER_ID and the key's directory (API_PRIVATE_KEYS_DIR); exit 1 (said by the
    shell's die) on a locked vault."""
    done = subprocess.run(["bash", "-c", LOAD_CREDENTIALS, "credentials", REPO_ROOT], stdout=subprocess.PIPE,
                          text=True)
    values = done.stdout.split("\n")
    if done.returncode != 0 or len(values) < 3:
        raise SystemExit(done.returncode or 1)
    return values[0], values[1], values[2]


def require_clean_tree(version: str) -> None:
    """The tag this pushes after upload (tag-mobile-release.sh) names HEAD as the commit that produced the build. A
    dirty tree makes that a lie: xcodebuild compiles whatever's on disk (log.sh's require_clean_tree)."""
    status = subprocess.run(["git", "-C", REPO_ROOT, "status", "--porcelain"], capture_output=True, text=True)
    if status.stdout.strip():
        raise Died(f"working tree has uncommitted changes — commit or stash them first, or the pushed mobile-ios-{version}"
                   " tag won't match what this build actually contains")


def archive_and_export(build: str) -> str:
    """The unit tests, the archive and the export: the ipa's path."""
    # The SPM target excludes every SwiftUI file, so this proves the MODEL/parser layer only -- the archive below is
    # what type-checks Views/.
    say("unit tests")
    run_logged(["swift", "test", "--package-path", f"{REPO_ROOT}/ios"], f"{build}/test.log", "swift test failed")
    ok("tests green")
    with open(f"{build}/ExportOptions.plist", "w") as f:
        f.write(EXPORT_OPTIONS)
    say("archive")
    run_logged(["xcodebuild", "archive",
                "-project", f"{REPO_ROOT}/ios/Kinowo.xcodeproj", "-scheme", "Kinowo", "-configuration", "Release",
                "-destination", "generic/platform=iOS", "-archivePath", f"{build}/Kinowo.xcarchive",
                "CODE_SIGN_STYLE=Manual", f"PROVISIONING_PROFILE_SPECIFIER={PROFILE}",
                "CODE_SIGN_IDENTITY=Apple Distribution", f"DEVELOPMENT_TEAM={TEAM_ID}"],
               f"{build}/archive.log", "archive failed")
    ok("archived")
    say("export")
    # Xcode's IPA packaging spawns `rsync` off PATH and expects the system one: a Homebrew rsync 3.x ahead of /usr/bin
    # dies mid-copy, which the export reports only as "exportArchive Copy failed" (the rsync error is in the
    # .xcdistributionlogs). Pin just rsync, so every other tool still resolves as usual.
    system_rsync = f"{build}/system-rsync"
    os.makedirs(system_rsync, exist_ok=True)
    os.symlink("/usr/bin/rsync", f"{system_rsync}/rsync")
    env = {**os.environ, "PATH": f"{system_rsync}:{os.environ.get('PATH', '')}"}
    run_logged(["xcodebuild", "-exportArchive", "-archivePath", f"{build}/Kinowo.xcarchive", "-exportOptionsPlist",
                f"{build}/ExportOptions.plist", "-exportPath", f"{build}/export"],
               f"{build}/export.log", "export failed", env=env)
    ipa = f"{build}/export/Kinowo.ipa"
    if not os.path.isfile(ipa):
        raise Died(f"no ipa at {ipa}")
    size = subprocess.run(["du", "-h", ipa], capture_output=True, text=True).stdout.split("\t")[0]
    ok(f"exported {size}")
    return ipa


def build_number() -> str:
    try:
        with open(f"{REPO_ROOT}/ios/Kinowo.xcodeproj/project.pbxproj", errors="replace") as f:
            found = re.search(r"CURRENT_PROJECT_VERSION = ([0-9]*);", f.read())
    except OSError:
        return ""
    return found.group(1) if found else ""


def release(upload: bool, build: str) -> None:
    check_sdk()
    # Before anything expensive: a locked vault should fail now, not after the archive.
    key, issuer, keys = credentials()
    os.environ["API_PRIVATE_KEYS_DIR"] = keys  # every command after reads its key from there, as the shell exported it
    try:
        with open(f"{REPO_ROOT}/mobile-version.txt") as f:
            version = f.read().replace("\n", "")
        say(f"iOS release {version}")
        require_clean_tree(version)
        shutil.rmtree(build, ignore_errors=True)
        os.makedirs(build)
        ipa = archive_and_export(build)
        # altool ships inside the Xcode toolchain and is not on PATH.
        developer = subprocess.run(["xcode-select", "-p"], capture_output=True, text=True).stdout.strip()
        altool = [f"{developer}/usr/bin/altool"]
        credential = ["-f", ipa, "-t", "ios", "--apiKey", key, "--apiIssuer", issuer]
        say("validate")
        run_logged([*altool, "--validate-app", *credential], f"{build}/validate.log", "validate failed")
        ok("validated")
        if not upload:
            ok("--no-upload: stopping before upload")
            return
        say("upload")
        run_logged([*altool, "--upload-app", *credential], f"{build}/upload.log", "upload failed")
        ok("uploaded")
        # Marks the commit this specific upload was actually built from: force-moved, a later successful upload of
        # the same version superseding whichever commit the tag last pointed to.
        subprocess.run([f"{REPO_ROOT}/scripts/tag-mobile-release.sh", "ios", version])
        # Build processing has taken anywhere from ~1 to ~10 minutes; the build is not in /v1/builds at all until it
        # finishes, so an absent build is not a failure.
        print(f"\nBuild {build_number()} is processing. It appears in App Store Connect within ~1-10 min;\n"
              "poll /v1/builds rather than assuming the upload failed.")
    finally:
        shutil.rmtree(keys, ignore_errors=True)


def main(argv: List[str]) -> int:
    signal.signal(signal.SIGINT, ended)
    signal.signal(signal.SIGTERM, ended)
    build = os.environ.get("IOS_RELEASE_BUILD_DIR") or f"{REPO_ROOT}/ios/build/release"
    try:
        release(argv[:1] != ["--no-upload"], build)
    except Died as e:
        sys.stdout.flush()
        print(f"\033[31m✗\033[0m {e}", file=sys.stderr, flush=True)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
