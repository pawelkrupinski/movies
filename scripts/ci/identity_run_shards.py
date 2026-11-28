#!/usr/bin/env python3
"""IDENTITY-RUN'S JOBS (.github/workflows/identity-run.yml's plan): which sbt commands run side by side, each in a
`movies-measure` job of its own, instead of one after the other in one job (run 42370669779: two countries' benches
took 28 min in a row).

  * a line naming {cc} runs once a country of `--countries`;
  * every non-blank line of the command is a job of its own (scripts/identity-ci-run's `--command`, repeated);
  * one line naming no country runs once, as the shard `all` -- the one-job run's own layout.

Each shard says where its output goes under the artifact root (`out`, empty for the root itself) and its sbt log's
name: `<cc>/` and `sbt-<cc>.log` for a one-line country command, `sbt.log` for `all` -- exactly the layout the
one-job run uploaded (scripts/identity-landing-check reads `sbt.log`); with several lines each shard's name prefixes
its own, so no two shards upload the same file.

  identity_run_shards.py --command '<command, one a line>' --countries pl,de   -> the matrix, one JSON line
"""
import argparse
import json
import sys


def _country_shards(number: int, prefix: str, line: str, ccs: list[str]) -> list[dict[str, str]]:
    """One shard per country for a command line naming `{cc}`."""
    if not ccs:
        raise ValueError(f"line {number} names {{cc}} but no country is given")
    return [{"name": prefix + cc, "cc": cc, "out": prefix + cc, "log": f"sbt-{prefix}{cc}.log", "command": line}
            for cc in ccs]


def _single_shard(number: int, several: bool, line: str) -> dict[str, str]:
    """The one shard of a command line that names no country."""
    name = str(number) if several else "all"
    return {"name": name, "cc": "", "out": "", "log": f"sbt-{name}.log" if several else "sbt.log", "command": line}


def plan(command: str, countries: str) -> list[dict[str, str]]:
    lines = [line.strip() for line in command.splitlines() if line.strip()]
    if not lines:
        raise ValueError("no command")
    ccs = [cc.strip() for cc in countries.split(",") if cc.strip()]
    several = len(lines) > 1
    shards = []
    for number, line in enumerate(lines, 1):
        if "{cc}" in line:
            shards += _country_shards(number, f"{number}-" if several else "", line, ccs)
        else:
            shards.append(_single_shard(number, several, line))
    return shards


DOCSTRING = __doc__ or ""  # None under python -OO


def main(argv: list[str]) -> str:
    parser = argparse.ArgumentParser(description=DOCSTRING.splitlines()[0])
    parser.add_argument("--command", required=True)
    parser.add_argument("--countries", default="")
    args = parser.parse_args(argv)
    return json.dumps(plan(args.command, args.countries), separators=(",", ":"))


if __name__ == "__main__":
    try:
        print(main(sys.argv[1:]))
    except ValueError as e:
        print(f"identity_run_shards: {e}", file=sys.stderr)
        sys.exit(2)
