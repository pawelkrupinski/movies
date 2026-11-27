#!/usr/bin/env python3
"""An evidence card for every UNJUDGED take of the unmatched-cluster capture — the listing's title, venues, year, runtime,
directors, catalogue ids and synopsis from the recording beside the taken record's title, original title, year,
runtime and directors — with a verdict SUGGESTED only where the evidence decides it (a year and a director shared →
right; a record running >30% longer than the venue's slot, a short billed as a feature, the year and the directors
both disagreeing → wrong; scripts.JudgeEvidence.suggest). Everything else stays open. Writes drafts for a human or an agent to confirm; it NEVER edits identity-judgements.tsv.

  scripts/identity-judge-cards [--fixtures SRC] [--country cc[,cc]]... [--out DIR] [--all] [--live|--offline]
                               (--countries a,b alike)

  --fixtures SRC  the capture (default: this checkout's): a directory, an "Identity capture" run id, or a git ref
                  (origin/chore/identity-capture — the weekly capture's branch, read without checking it out). Its
                  judgements come with it (a run's: this checkout's).
  --out DIR       default target/identity-judge: cards.md (one card per title × film, decisive ones first),
                  labels-draft.tsv (the decisive suggestions as identity-judgements.tsv rows, noted "— CONFIRM"), open.tsv
  --all           card the judged takes too, and write suggestions.tsv beside each label's verdict: how far the
                  suggestions can be trusted
  --live          read TMDB_API_KEY from the Touch ID vault when it is not set, for the records the capture never
                  read (default: TMDB_API_KEY if set, else those records stay unknown); --offline: never ask TMDB

The capture, the judgements and the classpath come from identity-tools.sh (identity_capture, identity_judgements,
identity_classpath, IDENTITY_JAVA) through scripts/lib/identity_ref_trees.py until that library moves. Tested by
scripts/identity_judge_cards_test.py."""
from __future__ import annotations

import os
import shutil
import sys
from dataclasses import dataclass, field
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import machine  # noqa: E402
import identity_ref_trees  # noqa: E402
import tool_cli  # noqa: E402
import tool_countries  # noqa: E402
import tool_process  # noqa: E402

TOOL = "identity-judge-cards"
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)


def usage() -> str:
    return (__doc__ or "").split("\n\nThe capture, the judgements")[0] + "\n"


@dataclass
class Options:
    fixtures: str = ""
    countries: List[str] = field(default_factory=lambda: [])
    passed: List[str] = field(default_factory=lambda: [])
    offline: bool = False

    def word(self, argv: List[str], i: int) -> int:
        flag = argv[i]
        if flag.split("=", 1)[0] in ("--country", "--countries"):
            self.countries, i = tool_countries.flag_at(argv, i, self.countries)
        elif flag in ("--fixtures", "--out"):
            value = tool_cli.value_at(argv, i)
            if flag == "--fixtures":
                self.fixtures = value
            else:
                os.makedirs(value, exist_ok=True)
                self.passed += [flag, os.path.realpath(value)]
            i += 1
        elif flag in ("--all", "--offline"):
            self.offline = self.offline or flag == "--offline"
            self.passed.append(flag)
        elif flag != "--live":
            raise tool_cli.Refused(f"unknown argument {flag}", with_usage=True)
        return i


def with_vault_key(argv: List[str], env: Mapping[str, str]) -> Optional[int]:
    """--live with no TMDB_API_KEY: this tool again under `secrets run` (vault-env.sh's ensure_vault_env); 1 when there
    is no secrets CLI to read it with. None: go on."""
    if "--live" not in argv or env.get("TMDB_API_KEY"):
        return None
    if shutil.which("secrets", path=env.get("PATH")) is None:
        print("[vault] TMDB_API_KEY not set, and no `secrets` CLI on PATH to read it from the vault", file=sys.stderr)
        return 1
    os.execvpe("secrets", ["secrets", "run", "movies", "TMDB_API_KEY", "--", os.path.join(HERE, TOOL), *argv],
               dict(env))


def cards(o: Options, env: Mapping[str, str]) -> int:
    refs = identity_ref_trees.RefTrees(env, tool_process.SubprocessRunner(), TOOL)
    if not o.offline and not env.get("TMDB_API_KEY"):
        refs.log("no TMDB_API_KEY: a record the capture never read stays unknown (--live reads the key from the vault)")
    ccs = o.countries or list(tool_countries.CODES)
    captured = refs.call("identity_capture", [o.fixtures, *ccs])
    if captured.code != 0:
        return captured.code
    capture = captured.stdout.rstrip("\n")
    judgements = refs.function("identity_judgements", [capture]) or ""
    country = ["--country", ",".join(o.countries)] if o.countries else []
    return refs.run_java(REPO, ["scripts.IdentityJudgeCards", "--fixtures", capture, "--labels", judgements, *country,
                                *o.passed])


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if argv[:1] in (["-h"], ["--help"]):
        sys.stdout.write(usage())
        return 0
    vault = with_vault_key(argv, env)
    if vault is not None:
        return vault
    code, o = tool_cli.parse_call(TOOL, usage(), lambda: tool_cli.read_options(Options(), argv, usage(), Options.word))
    return cards(o, env) if o is not None else code or 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
