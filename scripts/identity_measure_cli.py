#!/usr/bin/env python3
"""WHOLE-CORPUS MEASURE OF A RESOLVER CHANGE, before landing it: every take a commit moves, in every country's whole
pinned recording, split by what the judgements (test/resources/fixtures/identity-judgements.tsv) make of it.

A selector-only `identity-slice --batch` measures the listings it is handed: twice it missed regressions the whole
corpus showed (26e48897d lost 9 PL "Lalka" and 9 UK takes; branch a083 lost UK "Fallen Angels" x8 and DE
"Schwanensee"), and a local whole-corpus slice is ~192k listings, >10 hours. CI's identity takes check
(scripts/ci/identity-takes-check.sh) resolves the whole recording at a base and a head in ~15 minutes a country.
This dispatches that check, and only that check (`takes-only`), for one commit, then classifies its moves
(scripts/identity_measure.py) and prints LANDABLE or NOT LANDABLE.

THE DEFAULT WAY TO MEASURE (2026-10-21): it runs on CI, not on the laptop. scripts/identity-measure-local
is the fallback, run only for what CI cannot do (it takes the JVM slots itself, scripts/jvm-gate.sh). Typical wall time: ~25-40 minutes, most of it the takes rows.

usage: scripts/identity-measure [<commit-or-branch> | --head <commit>] [--base <sha>] [--recording <run>]
                                [--countries pl,de,uk,us,es] [--keep-branch] [--no-wait | --wait] [--full] [--fresh] [--json]
                                [--timeout <minutes>] [--cancel-on-timeout] [--all-countries] [--all-recordings | --one-recording] [--plan]
                                [--footprint [--traces DIR] | --footprint-only [--traces DIR]] [--no-cache]
       scripts/identity-measure --run <run id> [--countries ...] [--keep-branch] [--json] [--no-wait | --wait] [--timeout <minutes>] [--cancel-on-timeout]
  <commit>       default HEAD (uncommitted changes are NOT measured: commit them first); --head names it too
  --base         default the commit's merge-base with <remote>/main; the commit's parent when it is on main already
  --recording    a `Record scrape fixtures` run (e.g. 528), or prod-latest (the freshest complete nightly), every leg replays -- its corpus and tree as released, no
                 fills, the bare pair identity-measure-local lays -- in place of the pinned pair. Never reused. Name
                 the countries it holds with --countries (528: pl,de)
  --countries    the countries measured and judged: only their legs run (the workflow's `takes-countries`); also a
                 repeatable --country cc[,cc], as the other identity tools take it. Default: THE FOOTPRINT (below)
  --all-countries  every country, whatever the footprint
  --all-recordings measure over every recording in IDENTITY_MEASURE_RECORDINGS at once, in parallel (THE RECORDINGS)
  --one-recording  never more than the one recording, whatever the change moved
  --no-store     the takes check resolves both sides afresh, reusing and storing nothing (implies --fresh): the
                 control that proves a stored base changes no verdict
  --plan         print the footprint and the (recording, countries) runs it would dispatch, and exit
  --footprint    a TITLE-RULE change measured on the laptop in minutes, no dispatch, when it can be proved to move no take
                 (THE FOOTPRINT MEASURE below); otherwise says why and measures in full, as without it
  --footprint-only  the same, but exit 3 where --footprint would measure in full (nothing dispatched)
  --traces DIR   the base's traces from DIR (<cc>.jsonl.gz: a resolve's and a ratchet's --trace files, concatenated)
                 in place of the release's: a base no takes check stored one for, traced by scripts/identity-ci-run
  --keep-branch  leave the temporary branch on the remote
  --no-wait      dispatch, print the run, keep the branch, and exit: collect later with --run <id> (which deletes it)
  --wait         say the run's id and an ETA (the median wall time of the workflow's recent measure/ runs), then wait
                 with ~/bin/agentctl wait's backoff -- 60 s, x1.5, at most 5 min (IDENTITY_MEASURE_WAIT_FIRST seconds
                 first) -- not a read every 30 s. An agent runs it in the background and ends its turn. With --run too
  --timeout      minutes to WAIT (default max(120, 1.5 x the ETA said below)). It only stops the waiting: the run and its
                 branch stay, the tool says `scripts/identity-measure --run <id>` and exits 3 (STILL RUNNING -- not a verdict)
  --cancel-on-timeout  the old behaviour: at --timeout cancel the run, delete its branch, exit 2
  --full         run the whole convergence suite, not only the takes rows (never reused, never stored)
  --fresh        dispatch even when an earlier measurement of the same change answers it (below; the result cache too)
  --no-cache     neither answer from THE RESULT CACHE (below) nor store in it: measure afresh
  --take-paths   print the paths a reuse treats as able to move a take (matches.sh dialect) and exit
  --json         machine output (scripts/identity_measure.py --json)
  --no-what-if WHY  dispatch without a what-if verdict for this head and base (THE WHAT-IF GATE below); WHY is said in
                 the output and must be named in your report: the projection, the gate, live asks, drift, or anything
                 else scripts/identity-what-if cannot see
  --run          collect an existing run instead (its identity-takes-<cc> artifacts); deletes its measure/ branch.
                 A run still in progress is waited for (at most --timeout, then exit 3); with --no-wait it exits 3 at once

THE WHAT-IF GATE (the owner's "what-if first", 2026-11-05; review 2026-11-06 P3: 40% of measure runs failed or were
cancelled). Nothing is dispatched for a head with no scripts/identity-what-if verdict for the same head and base:
what-if writes one line a country it finished (identity-tools.sh's what_if_record_verdict, under the head and base),
and this looks it up (a file read, no run). Without it the tool prints the what-if command and exits 2;
`--no-what-if "<why>"` dispatches anyway and says so. A cached, reused or footprint answer dispatches nothing, so needs none.

THE FOOTPRINT (2026-10-22). Without --countries, only the countries whose takes the change CAN move are measured: each
changed take-moving file's countries as scripts/ci/identity_impact.py reads them (the identity area's test-impact
selection, 2026-10-27): */services/cinemas/<cc>/ or a capture identity-unmatched/<cc>.* reaches <cc>; a title-rules.tsv
change the countries whose loaded rows differ (a row's `countries`, a venue row's venues); title-words.tsv EVERY country
(TitleWords.tokens folds every language into one set: a German word reads in a Polish title too -- until 2026-10-27 a
row reached only its language's countries, which was unsound); any other take-moving file -- the resolver, the shared
rules -- every country. A change touching none measures all, as before (the takes rows then say "nothing can have
moved" in seconds). A country the run held no takes row of (a --run collected over every country) reads "not measured
(outside the footprint)" and leaves the verdict to the countries measured (identity_measure.py's UNMEASURED).

THE FOOTPRINT MEASURE (--footprint, 2026-10-22). A change to the title rules alone -- title-rules.tsv, the
services/titles/rules/ code (not TitleRulePlaceholders.scala, which FormatTags reads directly) and TitleNormalizer.scala:
the code the resolver reads titles through and nothing else does -- changes what the resolve READS, never how it
decides. Every takes check stores, beside the base it resolved and each main head, every title reading its resolve and
its ratchet made (scripts/ci/identity-takes-check.sh's THE NORMALIZER TRACE: `takes-trace-<cc>-<identity code>-<p|n><run>`
on the release). This replays the base's under the head's rules (services.movies.NormalizerTraceCheck, compiled in the
head's tree: ~1-3 minutes, no runner). When every reading of every footprint country reads alike, the head's resolve
would make exactly the base's readings in the same order and decide exactly as it did: NO TAKE CAN MOVE, printed as
the measure's table (`measure: footprint`) and LANDABLE, exit 0. That is sound for the recording the trace is over (the
newest pinned pair's, or --recording's named one), its fills aside. Anything else falls back to the full measure, said
first, its table `measure: full`:
  - a changed take-moving file outside the title rules (matcher, scoring, a capture, BillingParse, title-words.tsv: no
    trace says what they decide);
  - no stored trace of the base's identity code over the recording (a base main stored none for yet);
  - a reading that reads otherwise -- takes may move, and which only a resolve can say (they are listed, up to 20 a
    country) -- or one the head cannot make again (a method gone, a venue renamed).
The full measure stays the gate a change LANDS on: a footprint 0 is for iterating, and its own measure states so.

THE RECORDINGS (2026-10-22). One recording by default: the pinned pair (or --recording), over the footprint's
countries. It is also the cheap pre-diff for the rest: when it is LANDABLE and the change moved takes in two or more
countries (CROSS-COUNTRY), the same change is measured again over the other recordings in IDENTITY_MEASURE_RECORDINGS
(default "528": <run>[:<the countries it holds>], else read from its artifacts), in parallel, for the countries that moved; the
verdict is the worst of them. A change that moved nothing, or one country's takes, is answered by the one recording.
--all-recordings dispatches every recording at once, without the pre-diff; --one-recording never more than one.
With --no-wait only the first is dispatched; collecting it with --run says which recordings a cross-country change
still needs.

THE REMOTE. IDENTITY_MEASURE_REMOTE, else origin.

THE BRANCH. The workflow runs a commit only from a ref, so the commit is pushed as `measure/<user>/<short sha>-<random>`
-- the one kind of branch push this repository allows besides main, ONLY to run a build. The pre-push hook checks
nothing on a measure/ ref (it never lands), so the push needs no --no-verify. It is deleted at the end:
success, failure, Ctrl-C (which also cancels the run) alike. A commit
whose workflows predate the takes-only dispatch gets main's .github/ and scripts/ci/ laid over it in a commit of
its own (no identity path: the check diffs only identity code, so the measured code is the commit's).

REUSE. A measurement is the takes rows' base and head resolves over the pinned recording, classified HERE with the
judgements. An earlier run of (B0, H0) answers (B1, H1) -- collected again, no push, no dispatch -- when
  - the change is the same: `git patch-id --stable` of B..H over the files that can move a take -- the paths under
    .github/convergence-paths.txt less the CI plumbing (.github/, scripts/ but the takes check's own), plus what the
    takes check reads beyond them (its resolve dump under worker/src/it, the ratchet spec, the checked-in captures)
    -- is equal (the key it is stored under), AND
  - main moved only outside them: `git diff B0 B1` touches none (matched by .github/actions/changed-paths/matches.sh,
    as kick-convergence.sh matches them), so the base resolves alike, AND `git diff H0 H1` touches none either: the
    patch-id ignores whitespace and line numbers, so it finds a candidate but never proves the head the same.
identity-judgements.tsv is left out of all three: it moves no take, and collecting classifies with the head's own. A
run is reused for at most IDENTITY_MEASURE_REUSE_HOURS (default 24) after it was DISPATCHED (a --run's from its
createdAt, not from when it was collected): the pinned recording is renewed nightly, and a fresh dispatch measures
over the newest. Stored per change under <cache>/measure/reuse/<patch-id>/<base>, written whole (temp file + mv),
only for a run whose takes artifacts are downloaded in the cache -- by a measurement or a --run.
`--take-paths` prints the take-moving patterns (at HEAD) and exits.

THE RESULT CACHE (2026-10-23). Every verdict a measurement prints (LANDABLE or NOT LANDABLE, its table, the countries
whose takes moved) is stored as an asset of the convergence fixtures' release (FIXTURE_RELEASE_TAG, convergence-fixtures; the takes
check's resolves are on their own, TAKES_RELEASE_TAG, identity-takes): `measure-result-<key>.json`, the key a hash of the head and base shas, the
recording (`pinned` or the run), the countries measured, the measure mode (takes, full, footprint) and THE TOOLING HASH
-- the blobs of this script, identity_measure.py, the takes check and the workflows that run it, in this checkout. A
repeat request with the same key prints the stored verdict in seconds (`CACHED verdict of run <id>`, exit as stored):
no push, no dispatch, no download, from any clone. A pinned-pair verdict is answered for IDENTITY_MEASURE_CACHE_HOURS
(default 24, as REUSE: the pinned pair is renewed), a named recording's for good. --no-cache, --fresh and --no-store
never read it (only --no-store stores nothing either); --json neither reads nor stores (the cache holds the table).
A --no-wait dispatch leaves its key beside the run's cache (<cache>/measure/<run>/result-key), so its --run stores
the verdict -- when the checkout classifies with the head's own judgements. A cache that cannot be read or written
is said and skipped, never a failed measurement. The head's judgements are part of the head, so a judgement made
since is a new head and a new key.

THE LANE. A run from any branch but main has a lane of its own (identity-takes.yml's concurrency, and identity-model-
convergence.yml's for --full): it replaces no one's waiting run and waits behind no other measurement, only for runners. A run cancelled anyway
(by hand) is dispatched again until the deadline.

Exit: 0 LANDABLE, 1 NOT LANDABLE, 2 the measurement could not be made, 3 --footprint-only could not answer, OR the run is
still going at --timeout (no verdict; the run and branch are kept: collect with --run <id>).
"""
from __future__ import annotations

import os
import signal
import sys
import tempfile
import time
from types import FrameType
from typing import Dict, List, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import machine  # noqa: E402
import identity_measure_cli_options as options_  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402
from identity_measure_cli_context import TOOL, Context, Die, Exit  # noqa: E402
from identity_measure_cli_flow import Measure  # noqa: E402


class Interrupted(BaseException):
    """INT or TERM: the run cancelled and the branch deleted on the way out, exit 130."""


def interrupted(_signum: int, _frame: Optional[FrameType]) -> None:
    raise Interrupted()


def measured(o: options_.Options, env: Dict[str, str]) -> int:
    with tempfile.TemporaryDirectory(prefix="identity-measure.", dir=env.get("TMPDIR") or None) as scratch:
        tool = Measure(Context(o, env, tool_process.SubprocessRunner(), scratch, time.monotonic, time.time))
        try:
            for sig in (signal.SIGINT, signal.SIGTERM):
                signal.signal(sig, interrupted)
            return tool.main()
        except Die as e:
            print(f"{TOOL}: {e}", file=sys.stderr)
            return 2
        except Exit as e:
            return e.code
        except Interrupted:
            return 130
        finally:
            tool.clean_up()


def main(argv: List[str], env: Dict[str, str]) -> int:
    sys.stdout.reconfigure(line_buffering=True)  # type: ignore[attr-defined] # in order with stderr, as the shell tool wrote
    try:
        o = options_.parse(argv, __doc__ or "")
        return 0 if o is None else measured(o, env)
    except options_.UsageError as e:
        return tool_cli.usage_failed(TOOL, e)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
