#!/usr/bin/env python3
"""Record the WHOLE country's fixture corpus into test/resources/fixtures/<dir> (default `today`) by running the real
cinema scrape + the full enrichment cascade (TMDB -> IMDb -> Metacritic -> RT -> Filmweb) against live upstreams, then
verify the capture is non-empty.

Shared by two callers so a green local run is exactly what CI does:
  - .github/workflows/country-fixture-artifact.yml -- the daily artifact job, which passes a zip target so the corpus
    can be uploaded;
  - a local machine -- just run `.github/scripts/record_country_fixture.py` to drop a fresh capture under
    test/resources/fixtures/today/.

Env:
  KINOWO_COUNTRY      which country to record: pl (default) | de | uk | us | es. The recorder walks `country.cities`,
                      so WITHOUT this it silently records Poland whatever you meant -- which is why the corpus was
                      Poland-only for as long as it existed and Germany and the UK had no offline replay at all.
  KINOWO_FIXTURE_DIR  fixture subdir under test/resources/fixtures. Defaults to `today` for Poland (the name the local
                      sync agent pulls) and `today-<code>` otherwise, so recording one country never overwrites
                      another's corpus.
  TMDB_API_KEY        REQUIRED -- real TMDB key; the whole enrichment cascade 401s and captures nothing without it.
  DECODO_PROXY_USER   REQUIRED -- the Decodo residential-proxy credentials the Multikino / biletyna scrapes egress
  DECODO_PROXY_PASS   through (their WAF blocks a datacenter IP, a GitHub runner's included).
CI sets them as secrets. Locally, a missing one is read from the Touch ID vault: the tool re-runs itself under
`secrets run movies <the missing keys>` (one Touch ID, scripts/vault_env.py). It exits 1 if a required one is still
missing rather than recording a silently-partial corpus.

Args:
  $1  optional path for a zip of the corpus. When given, the recorded tree is wrapped in one colon-free zip there (the
      artifact job uploads it): the recorder names per-URL fixture files after the URL, so some contain a colon
      (`...,ts:1781388000`), which upload-artifact rejects as an NTFS-unsafe character; inside a zip they are bytes.
      Omit for a plain on-disk capture -- the normal local case. (record_country_fixture_test.py)"""
from __future__ import annotations

import os
import sys
from typing import Dict, List, Mapping, Optional, TextIO

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path[:0] = [os.path.join(ROOT, "scripts"), os.path.join(ROOT, "scripts", "lib")]
import tool_process  # noqa: E402
import vault_env  # noqa: E402

# Without TMDB_API_KEY the whole enrichment cascade 401s and captures nothing; without the proxy credentials the
# Multikino / biletyna scrapes never reach their sites. A partial capture is worse than no capture -- it looks complete
# but silently omits whole cinemas/ratings -- so a missing one fails loudly rather than records a sparse one.
REQUIRED = ("TMDB_API_KEY", "DECODO_PROXY_USER", "DECODO_PROXY_PASS")
COUNTRIES = ("pl", "de", "uk", "us", "es")
RECORDER = "worker/Test/runMain clients.tools.RecordAllDataToFixture"


class Recording:
    def __init__(self, root: str, env: Mapping[str, str], runner: tool_process.Runner, out: TextIO,
                 err: TextIO) -> None:
        self.root = root
        self.env: Dict[str, str] = dict(env)
        self.runner = runner
        self.out = out
        self.err = err

    def error(self, message: str) -> int:
        self.err.write(f"::error::{message}\n")
        return 1

    def say(self, line: str) -> None:
        self.out.write(line + "\n")
        self.out.flush()

    def country(self) -> Optional[str]:
        """WHICH country. `Country.fromEnv` reads the singular KINOWO_COUNTRY while the worker wiring reads the plural
        KINOWO_COUNTRIES, and anything that consults only one falls back to Poland -- so both are set."""
        country = self.env.get("KINOWO_COUNTRY") or "pl"
        if country not in COUNTRIES:
            return None
        directory = self.env.get("KINOWO_FIXTURE_DIR") or ("today" if country == "pl" else f"today-{country}")
        self.env.update(KINOWO_COUNTRY=country, KINOWO_COUNTRIES=country, KINOWO_FIXTURE_DIR=directory)
        return country

    def run(self, zip_target: str) -> int:
        missing = [key for key in REQUIRED if not self.env.get(key)]
        if missing:
            return self.error(f"Missing required key(s): {' '.join(missing)} — set them in the environment (or the Touch"
                              " ID vault, namespace movies) before recording.")
        country = self.country()
        if country is None:
            return self.error(f"KINOWO_COUNTRY='{self.env.get('KINOWO_COUNTRY')}' is not one of pl|de|uk|us|es.")
        name = self.env["KINOWO_FIXTURE_DIR"]
        relative = f"test/resources/fixtures/{name}"
        self.say(f"Recording the {country} fixture corpus into {relative} …")
        status = self.runner.run(tool_process.Command(["sbt", RECORDER], self.env, cwd=self.root))
        if status != 0:
            return status
        count = self.verified(relative)
        if count is None:
            return 1
        return self.zipped(zip_target, name, count) if zip_target and count else (0 if count else 1)

    def verified(self, relative: str) -> Optional[int]:
        """How many files the capture holds, said; None (failed) when it holds none or there is no capture at all."""
        directory = os.path.join(self.root, relative)
        if not os.path.isdir(directory):
            self.err.write(f"find: {relative}: No such file or directory\n")
            return None
        count = sum(len(files) for _, _, files in os.walk(directory))
        self.say(f"Captured {count} fixture files into {relative} ({capture_date(directory)})")
        if count == 0:
            self.error("Recorder produced no fixture files — refusing to treat this as a successful capture.")
            return None
        return count

    def zipped(self, target: str, name: str, count: int) -> int:
        fixtures = os.path.join(self.root, "test/resources/fixtures")
        status = self.runner.run(tool_process.Command(["zip", "-q", "-r", "-X", target, name], self.env, cwd=fixtures))
        if status != 0:
            return status
        size = self.runner.capture(tool_process.Command(["du", "-h", target], self.env, cwd=self.root))
        if size.code != 0:
            self.err.write(size.stderr)
            return size.code
        self.say(f"Wrote {size.stdout.split(chr(9))[0].strip()} zip with {count} fixtures to {target}")
        return 0


def capture_date(directory: str) -> str:
    """The capture's CAPTURE_DATE, its lines joined by spaces (each newline one), "" when there is none."""
    try:
        with open(os.path.join(directory, "CAPTURE_DATE")) as f:
            return f.read().replace("\n", " ")
    except OSError:
        return ""


def main(argv: List[str]) -> int:
    """The composition point. Locally a missing key comes from the Touch ID vault: this tool again under `secrets
    run`, naming only the missing keys. A CI runner has every key set (and no `secrets` CLI), so nothing happens there;
    anything still missing is refused by the check in `run` either way."""
    vault_env.exec_under_vault(REQUIRED, [sys.executable, os.path.abspath(__file__), *argv])
    recording = Recording(ROOT, os.environ, tool_process.SubprocessRunner(), sys.stdout, sys.stderr)
    return recording.run(argv[0] if argv else "")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
