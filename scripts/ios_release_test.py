#!/usr/bin/env python3
"""scripts/ios-release.sh, black-box, in a throwaway copy of the repository with fakes of everything it drives --
swift, xcodebuild, xcrun, xcode-select, altool, the vault and tag-mobile-release.sh -- so nothing is ever archived,
uploaded or tagged for real. What it pins: the lane's order (SDK floor, credentials, clean tree, test, archive, export,
validate, upload, tag) and its lines; the export runs the SYSTEM rsync (a Homebrew rsync 3.x ahead of /usr/bin made
the 2.0.12 export die with nothing but "exportArchive Copy failed"); an iOS SDK older than 27.1 is refused before the
archive (27.10 newer than 27.9); the App Store key reaches altool through API_PRIVATE_KEYS_DIR, removed afterwards; each
failing step stops the lane naming its log. Ported from ios-release-test.sh case for case, plus every step's argv and
failure; the same cases hold the shell tool and its Python rewrite (the copy takes ios_release.py and scripts/lib when
they exist)."""
from __future__ import annotations

import json
import os
import re
import shutil
import sys
import unittest
from typing import Any, Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
OWN = ("ios-release.sh", "ios_release.py", "log.sh", "app-store-credentials.sh")
SAY, OK, DIE = "\033[36m▸\033[0m ", "\033[32m✓\033[0m ", "\033[31m✗\033[0m "
# xcodebuild: archive lays the archive (fails on $ARCHIVE_FAILS); the export notes which rsync it would spawn, then lays
# the ipa unless $NO_IPA
XCODEBUILD = '''
if [ "$1" = archive ]; then [ -z "${ARCHIVE_FAILS:-}" ] || { echo "archive: no"; exit 65; }; fi
if [ "$1" = -exportArchive ]; then
  r=$(command -v rsync); printf '%s\\n' "$(readlink "$r" || echo "$r")" > "$WORK/export-rsync"
  [ -n "${NO_IPA:-}" ] || while [ $# -gt 0 ]; do [ "$1" = -exportPath ] && { mkdir -p "$2"; echo ipa > "$2/Kinowo.ipa"; }; shift; done
fi
exit 0
'''
# altool: notes the key directory's .p8 (name, content, mode); fails the step named in $ALTOOL_FAILS
ALTOOL = '''
import stat
keys = env.get("API_PRIVATE_KEYS_DIR", "")
found = sorted(os.listdir(keys)) if keys and os.path.isdir(keys) else []
p8 = [(n, open(os.path.join(keys, n)).read(), oct(stat.S_IMODE(os.stat(os.path.join(keys, n)).st_mode))) for n in found]
with open(env["WORK"] + "/altool.jsonl", "a") as f:
    f.write(json.dumps({"keys": keys, "p8": p8}) + "\\n")
if env.get("ALTOOL_FAILS") and env["ALTOOL_FAILS"] in argv:
    out("altool: no")
    exit(1)
'''
# the vault: each secret's value; a locked one ($VAULT_LOCKED) answers nothing
SECRETS = '''
if env.get("VAULT_LOCKED"):
    exit(1)
out({"APP_STORE_KEY_ID": "K1", "APP_STORE_ISSUER_ID": "I1"}.get(argv[2], "p8"), end="")
'''


class IosReleaseTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo", "-b", "main")
        os.makedirs(os.path.join(self.checkout, "scripts"))
        for name in OWN:
            if os.path.exists(os.path.join(HERE, name)):
                shutil.copy2(os.path.join(HERE, name), os.path.join(self.checkout, "scripts", name))
        if os.path.exists(os.path.join(HERE, "ios_release.py")):
            shutil.copytree(os.path.join(HERE, "lib"), os.path.join(self.checkout, "scripts", "lib"),
                            ignore=shutil.ignore_patterns("__pycache__", "*_test.py"))
        tag = self.fakes.script("tag-mobile-release.sh", "exit 0", shell="/usr/bin/env bash")
        shutil.move(tag, os.path.join(self.checkout, "scripts", "tag-mobile-release.sh"))
        self.write("repo/mobile-version.txt", "2.0.99\n")
        self.write("repo/ios/Kinowo.xcodeproj/project.pbxproj", "\t\tCURRENT_PROJECT_VERSION = 42;\n"
                                                                "\t\tCURRENT_PROJECT_VERSION = 42;\n")
        self.write("repo/.gitignore", "ios/build/\n")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "init")
        self.fakes.script("swift", '[ -z "${SWIFT_FAILS:-}" ] || { echo "swift: red"; exit 1; }')
        self.fakes.script("xcodebuild", XCODEBUILD, shell="/usr/bin/env bash")
        self.fakes.script("xcrun", 'echo "${SDK:-27.1}"; [ -z "${NO_SDK:-}" ]')
        self.fakes.script("xcode-select", f"echo {self.path('xcode')}")
        os.makedirs(self.path("xcode", "usr", "bin"))
        shutil.move(self.fakes.program("altool", ALTOOL), self.path("xcode", "usr", "bin", "altool"))
        self.secrets = self.fakes.program("secrets", SECRETS)
        os.makedirs(self.path("brew"))
        shutil.move(self.fakes.script("rsync", "exit 1"), self.path("brew", "rsync"))
        self.build = self.path("build")

    def release(self, *args: str, **extra: Optional[str]) -> cli_harness.Done:
        env: Dict[str, Optional[str]] = {"PATH": f"{self.path('brew')}:{self.fakes.bin}:{os.environ['PATH']}", "SECRETS_BIN": self.secrets,
               "IOS_RELEASE_BUILD_DIR": self.build, "WORK": self.work, "APP_STORE_KEY_ID": None,
               "APP_STORE_ISSUER_ID": None, "APP_STORE_KEY_P8": None, **extra}
        return self.run_tool(*args, env=env, cwd=self.checkout,
                             tool=os.path.join(self.checkout, "scripts", "ios-release.sh"))

    def called(self, name: str) -> List[List[str]]:
        return self.fakes.calls(name)

    def altool_seen(self) -> List[Dict[str, Any]]:
        with open(self.path("altool.jsonl")) as f:
            return [json.loads(line) for line in f]

    def test_a_dry_run_goes_through_to_the_no_upload_stop(self) -> None:
        done = self.release("--no-upload")
        self.assertEqual(0, done.code, done.output)
        lines = done.stdout.splitlines()
        self.assertEqual([f"{SAY}iOS release 2.0.99", f"{SAY}unit tests", f"{OK}tests green", f"{SAY}archive",
                          f"{OK}archived", f"{SAY}export"], lines[:6])
        self.assertRegex(lines[6], "^" + re.escape(OK) + r"exported \S+$")
        self.assertEqual([f"{SAY}validate", f"{OK}validated", f"{OK}--no-upload: stopping before upload"], lines[7:])
        self.assertEqual("", done.stderr)
        self.assertEqual([["--validate-app", "-f", f"{self.build}/export/Kinowo.ipa", "-t", "ios", "--apiKey", "K1",
                           "--apiIssuer", "I1"]], self.called("altool"))
        self.assertEqual([], self.called("tag-mobile-release.sh"))

    def test_the_export_runs_the_system_rsync(self) -> None:
        self.release("--no-upload")
        self.assertEqual("/usr/bin/rsync\n", self.read("export-rsync"))

    def test_the_builds_argv_and_the_export_options(self) -> None:
        self.release("--no-upload")
        self.assertEqual([["test", "--package-path", f"{self.checkout}/ios"]], self.called("swift"))
        self.assertEqual([
            ["archive", "-project", f"{self.checkout}/ios/Kinowo.xcodeproj", "-scheme", "Kinowo", "-configuration",
             "Release", "-destination", "generic/platform=iOS", "-archivePath", f"{self.build}/Kinowo.xcarchive",
             "CODE_SIGN_STYLE=Manual", "PROVISIONING_PROFILE_SPECIFIER=Kinowo App Store",
             "CODE_SIGN_IDENTITY=Apple Distribution", "DEVELOPMENT_TEAM=CQ4YC43YDM"],
            ["-exportArchive", "-archivePath", f"{self.build}/Kinowo.xcarchive", "-exportOptionsPlist",
             f"{self.build}/ExportOptions.plist", "-exportPath", f"{self.build}/export"]], self.called("xcodebuild"))
        with open(os.path.join(self.build, "ExportOptions.plist")) as f:
            self.assertEqual('''<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>method</key><string>app-store-connect</string>
  <key>teamID</key><string>CQ4YC43YDM</string>
  <key>signingStyle</key><string>manual</string>
  <key>provisioningProfiles</key>
  <dict><key>dev.kinowo.Kinowo</key><string>Kinowo App Store</string></dict>
  <key>uploadSymbols</key><true/>
  <key>destination</key><string>export</string>
</dict>
</plist>
''', f.read())
        self.assertEqual(["ExportOptions.plist", "archive.log", "export", "export.log", "system-rsync", "test.log",
                          "validate.log"], sorted(os.listdir(self.build)))

    def test_an_upload_hands_altool_the_key_then_tags_and_says_the_build(self) -> None:
        done = self.release()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["--upload-app", "-f", f"{self.build}/export/Kinowo.ipa", "-t", "ios", "--apiKey", "K1",
                          "--apiIssuer", "I1"], self.called("altool")[-1])
        for seen in self.altool_seen():
            self.assertEqual([["AuthKey_K1.p8", "p8", "0o600"]], [list(p) for p in seen["p8"]])
        self.assertFalse(os.path.exists(self.altool_seen()[0]["keys"]), "the key directory is removed")
        self.assertEqual([["ios", "2.0.99"]], self.called("tag-mobile-release.sh"))
        self.assertTrue(done.stdout.endswith(
            f"{SAY}upload\n{OK}uploaded\n\nBuild 42 is processing. It appears in App Store Connect within ~1-10 min;\n"
            "poll /v1/builds rather than assuming the upload failed.\n"), done.stdout)

    def test_an_sdk_older_than_27_1_is_refused_before_anything(self) -> None:
        for sdk in ("26.4", "27.0"):
            self.fakes.clear()
            done = self.release("--no-upload", SDK=sdk)
            self.assertEqual((1, f"{DIE}iOS SDK {sdk} is older than 27.1 — the build would not fill the iPhone Duo "
                                 "screen; select Xcode 27.1 or newer\n"), (done.code, done.stderr), sdk)
            self.assertEqual([], self.called("xcodebuild") + self.called("secrets"))

    def test_sdk_27_10_passes_and_archives(self) -> None:
        done = self.release("--no-upload", SDK="27.10")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("archive", self.called("xcodebuild")[0][0])

    def test_no_sdk_is_refused(self) -> None:
        done = self.release("--no-upload", NO_SDK="1")
        self.assertEqual((1, f"{DIE}no iOS SDK found — is full Xcode selected?\n"), (done.code, done.stderr))

    def test_a_locked_vault_fails_before_anything_expensive(self) -> None:
        done = self.release("--no-upload", VAULT_LOCKED="1")
        self.assertEqual((1, f"{DIE}can't read APP_STORE_KEY_ID from the vault — run `secrets unlock movies` (Touch ID)"
                             " and retry\n"), (done.code, done.stderr))
        self.assertEqual("", done.stdout)
        self.assertEqual([], self.called("swift"))

    def test_credentials_handed_in_skip_the_vault(self) -> None:
        done = self.release("--no-upload", APP_STORE_KEY_ID="K2", APP_STORE_ISSUER_ID="I2", APP_STORE_KEY_P8="key")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.called("secrets"))
        self.assertEqual(["--apiKey", "K2", "--apiIssuer", "I2"], self.called("altool")[0][5:])
        self.assertEqual([["AuthKey_K2.p8", "key", "0o600"]], [list(p) for p in self.altool_seen()[0]["p8"]])

    def test_a_dirty_tree_is_refused_before_the_build(self) -> None:
        self.write("repo/ios/Stray.swift", "\n")
        done = self.release("--no-upload")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(f"{DIE}working tree has uncommitted changes — commit or stash them first, or the pushed "
                         "mobile-ios-2.0.99 tag won't match what this build actually contains\n", done.stderr)
        self.assertEqual(f"{SAY}iOS release 2.0.99\n", done.stdout)
        self.assertEqual([], self.called("swift"))

    def test_the_default_build_directory_is_under_ios(self) -> None:
        done = self.release("--no-upload", IOS_RELEASE_BUILD_DIR=None)
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(os.path.isfile(os.path.join(self.checkout, "ios", "build", "release", "validate.log")))

    def test_each_failing_step_stops_the_lane_naming_its_log(self) -> None:
        for extra, said in (({"SWIFT_FAILS": "1"}, f"swift test failed — see {self.build}/test.log"),
                            ({"ARCHIVE_FAILS": "1"}, f"archive failed — see {self.build}/archive.log"),
                            ({"NO_IPA": "1"}, f"no ipa at {self.build}/export/Kinowo.ipa"),
                            ({"ALTOOL_FAILS": "--validate-app"}, f"validate failed — see {self.build}/validate.log"),
                            ({"ALTOOL_FAILS": "--upload-app"}, f"upload failed — see {self.build}/upload.log")):
            self.fakes.clear()
            done = self.release(**extra)
            self.assertEqual((1, f"{DIE}{said}\n"), (done.code, done.stderr), extra)
            self.assertEqual([], self.called("tag-mobile-release.sh"), extra)
        with open(os.path.join(self.build, "upload.log")) as f:
            self.assertEqual("altool: no\n", f.read())


if __name__ == "__main__":
    unittest.main()
