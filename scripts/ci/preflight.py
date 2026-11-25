#!/usr/bin/env python3
"""PREFLIGHT: fail a workflow in seconds, at its first job, when it cannot succeed for want of configuration rather than
code -- a secret that is unset or empty, or an Android SDK licence the runner has not accepted. Without this those
surface minutes in, as a signing step producing an unsigned APK, a `flyctl deploy` refusing an empty token, or Gradle
declining to install a platform, and read like build failures.

  preflight.sh [--android-sdk-licence] NAME...

  NAME...                 environment variables that must be set and non-empty. The caller maps each secret into the
                          step's `env:`; this prints only the NAMES, never a value, not even its length.
  --android-sdk-licence   also require the SDK licence Gradle needs (android-sdk-license) to be accepted, as
                          `sdkmanager --licenses` reports it. Run it after the same android-actions/setup-android step
                          the build uses.

The workflows run it by the name scripts/ci/preflight.sh; stdlib only (a plan job's sparse checkout runs it); tested by
preflight_test.py against a stub sdkmanager."""
from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
from typing import List, Mapping, Optional

NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
# The licence Gradle's SDK auto-install needs for platforms, build-tools and the NDK. Everything else sdkmanager lists
# (Google TV, XR, MIPS, and the PREVIEW licence, which only preview packages carry) is for packages this project never
# installs; a healthy runner lists six of them.
REQUIRED_LICENCES = ("android-sdk-license",)
LICENCE = re.compile(r"[0-9]/[0-9]*: License ([A-Za-z0-9._-]*):")
VERDICT = re.compile(r"All SDK package licenses accepted|licenses not accepted")


class Preflight:
    def __init__(self, env: Mapping[str, str]) -> None:
        self.env = env
        self.problems = 0

    def error(self, text: str) -> None:
        print(f"::error::{text}", flush=True)
        self.problems += 1

    def secret(self, name: str) -> None:
        if not NAME.fullmatch(name):
            self.error(f"'{name}' is not an environment variable name")
        elif not self.env.get(name):
            self.error(f"secret {name} is unset or empty — add it under Settings > Secrets and variables > Actions")
        else:
            print(f"secret {name}: present", flush=True)

    def sdkmanager(self) -> Optional[str]:
        found = shutil.which("sdkmanager", path=self.env.get("PATH"))
        sdk = self.env.get("ANDROID_SDK_ROOT") or self.env.get("ANDROID_HOME") or ""
        beside = os.path.join(sdk, "cmdline-tools", "latest", "bin", "sdkmanager")
        if not found and sdk and os.access(beside, os.X_OK):
            return beside
        return found

    def licences(self) -> None:
        sdkmanager = self.sdkmanager()
        if not sdkmanager:
            self.error("no sdkmanager on PATH or under $ANDROID_HOME — cannot check the SDK licences")
            return
        # "y" to reviewing the unaccepted licences and "n" to each, so every one is NAMED ("1/6: License
        # android-googletv-license:") and none is accepted by this check.
        said = subprocess.run([sdkmanager, "--licenses"], input="y\n" + "n\n" * 100, env=dict(self.env), text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout
        unaccepted = sorted(set(LICENCE.findall(said)))
        if not VERDICT.search(said):
            self.error("sdkmanager --licenses said neither 'all accepted' nor 'N not accepted' — cannot tell:")
            print("\n".join(said.rstrip("\n").split("\n")[-20:]), flush=True)
        for licence in REQUIRED_LICENCES:
            if licence in unaccepted:
                self.error(f"Android SDK licence {licence} is not accepted — Gradle cannot install the platform or "
                           "build-tools it needs")
        if unaccepted:
            unneeded = " ".join(licence for licence in unaccepted if licence not in REQUIRED_LICENCES)
            print(f"unaccepted licences this build does not need: {unneeded} ", flush=True)
        print(f"Android SDK licence checked: {' '.join(REQUIRED_LICENCES)}", flush=True)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    for arg in argv:
        if arg.startswith("-") and arg != "--android-sdk-licence":
            print(f"unknown option {arg}", file=sys.stderr)
            return 2
    preflight = Preflight(env)
    for name in (arg for arg in argv if not arg.startswith("-")):
        preflight.secret(name)
    if "--android-sdk-licence" in argv:
        preflight.licences()
    if preflight.problems:
        print(f"preflight: {preflight.problems} problem(s) — nothing after this job can succeed until they are fixed.")
        return 1
    print("preflight: ok")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
