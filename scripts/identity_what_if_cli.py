#!/usr/bin/env python3
"""A LOCAL WHAT-IF OF A RESOLVER CHANGE over prod's current corpus, in minutes: the base's and the head's model resolve
the LOCAL prod mirror's snapshot of a moment (`--at`; :28017, read-only), the head re-resolving only the families the
change can reach, and every take that moved is bucketed by the judgements (test/resources/fixtures/identity-judgements
.tsv) as scripts/identity-measure buckets a CI measurement -- moved, gained, lost, right->wrong, new-wrong -- with prod's
served take beside each.

  scripts/identity-what-if [--base REF] [--head REF|.] [--at ISO] [--max-age HOURS] [--all] [--verify]
                           [--offline] [--model-only] [--answers-dir DIR] [--no-serve]
  scripts/identity-what-if --bisect GOOD BAD [--listing 'cc|venue|raw title'] [--at ISO] [--since ISO] cc
  scripts/identity-what-if --recording RUN --base REF --head REF cc...
  scripts/identity-what-if --stop-served
                           [--threads N] [--out DIR] [--judgements F] [--json] cc...

  --base REF     the code the change is measured against (default origin/main)
  --head REF|.   the change: a commit, or `.` (default) for this checkout's code with its uncommitted edits
  --at ISO       the mirror moment both sides resolve (default: the newest snapshot the base already resolved within
                 --max-age hours, 6 by default, else now)
  --all          re-resolve every family at the head, whatever the change reaches: the DEFAULT for a change reaching a
                 take (2026-11-20, the stamp carry: only a whole verdict carries a stamp -- scripts/identity-stamp-carry --
                 and the head costs about what the base side does; most resolver changes select ALL anyway)
  --selection    re-resolve only the families the change can reach (the default before 2026-11-20): faster on a narrow
                 change, but its verdict carries no stamp
  --verify       also resolve every family at the head and FAIL (exit 1) if any take differs from the what-if's: the
                 proof that the selection missed nothing
  --threads N    head families resolved at once (default 6)
  --out DIR      where the per-country diff TSVs go (default target/identity-what-if/<head>)
  --judgements F the judgements the moves are bucketed by (default the head's own, as identity-measure classifies)
  --offline      ask nothing: a question the mirror's store lacks stays a gap, its listing counted as awaiting an
                 answer (the tool before 2026-11-05). Without it, every such question -- TMDB's and IMDb's for the
                 model, the agreement families' (IMDb, Wikidata, Metacritic, Rotten Tomatoes, Filmweb where it votes),
                 posters and catalogue pages for the stage -- is asked of the REAL service once (direct requests over
                 production's paced wire, each host at most 8 at once, halved on a 429/503; never a proxy or paid
                 service; TMDB's key from the vault: `secrets run movies TMDB_API_KEY`) and filed into the LOCAL
                 what-if store, keyed as production's store keys it, so a repeat run asks nothing
  --answers-dir  that store (default $KINOWO_IDENTITY_CACHE/what-if-answers; one directory a country)
  --no-serve     a JVM per side; by default the head's tree runs in a KEPT JVM (one per tree and code; stops after
                 $KINOWO_IDENTITY_WHAT_IF_IDLE_MINUTES, 10, idle minutes; --stop-served stops them all) holding the
                 corpus and its context of the last two (country, snapshot, answers): a warm rerun resolves only
  --bisect G B   the commit of G..B (first parent) whose code moved a take on one mirror snapshot: the listing's model
                 and final take with --listing (its venue and raw title as takes-diff rows name them), else every take;
                 each step a whole side, kept as a base is (about a minute a step cold, seconds kept). When G and B take
                 alike, no commit moved it: B is resolved at --since (24 h before the snapshot by default) to say
                 whether the CORPUS did
  --recording RUN  replay that cached "Record scrape fixtures" run instead of the mirror, through CI's OWN two tools
                 (scripts/identity-measure-local: the model's IdentityResolveDump and the ratchet's final takes): the
                 CI measure's numbers exactly (its recording, its capture), at a whole resolve's cost, not the what-if's
  --model-only   compare the model's takes only; by default the FINAL take too: a fresh agreement stage settled over
                 each side's model takes (the base's every family, the head's re-resolved ones), diffed as the measure
                 diffs its final stage

HOW. Each ref's sources are laid out once under the shared cache (scripts/identity-tools.sh's identity_tree, this
checkout's WhatIfResolve.scala and MemoLookups.scala dropped in, so a ref from before the tool works) and compiled once.
The base resolves every family (services.identity.WhatIfResolve: takes, rule ids, block keys, prod's served take) in a
JVM of its own, and the side is KEPT on disk, keyed by the base's sha, the tool files and the snapshot moment: a repeat
run resolves only the head. While a base side resolves, the head compiles beside it (its own JVM slot).
scripts/identity_what_if.py selects: the changed code walked up its callers to the rule roots
(scripts/identity-what-if-roots.tsv), the families whose base trace names one of their rules re-resolved, widened while a
re-resolved family blocks under another's key; a change it cannot attribute (shared resolver code, scoring, lookups,
normalizer) re-resolves ALL, and says why. A change reaching no take (docs, tests, scripts, comments: `reaches`) or a
selection naming no family starts no head JVM and compiles no head: its takes are the base's. Then it diffs.
Every line it logs carries the seconds since it started (+Ns): the run's phase timings.

A change to the agreement stage (identity/.../agreement/, or a definition its code calls) selects ALL: every family's
final take settled again at the head, and it says why.

WHAT IT IS NOT. prod's current corpus, not a recording: CI's scripts/identity-measure replays a pinned recording
and stays the verdict of record; the two differ by the corpus (docs: identity-investigation skill).

Each country it finishes is recorded for scripts/identity-measure's what-if gate (THE VERDICT RECORD, below).

A PURE MOVE (scripts/pure-move-check: files, packages and classes renamed, nothing else) is not resolved at all: it
prints "PURE MOVE: what-if skipped (scripts/pure-move-check)", records a `pure-move` verdict a country and exits 0.

A MIRROR CHANGING UNDER THE RUN IS A HARNESS ERROR, never takes moved (2026-11-19: lane C's US what-if reported 87k
takes moved off a base read while the mirror re-seeded, 16k of its live asks failed; a rerun moved none). Both sides
read the mirror's CURRENT listings (the snapshot moment replays identity history over them), so both must read ONE
copy. The run FENCES the mirror's tailer (scripts/local-mirror/fence-rule.js: a file under $KINOWO_MIRROR_FENCE_DIR,
~/.cache/kinowo/mirror-fence by default, that tail.js holds every change for; mirror.sh holds its re-seed audit), and
its fence LINGERS $KINOWO_WHAT_IF_FENCE_LINGER_MINUTES (20) after it, so a rerun finds the same copy and the base side
it kept. It is pinned to the mirror's generation (scripts/local-mirror/freshness --generation): its seed generation and
its tail position (the resume token of the last change tailed: 2026-11-20, lane A3's DE what-if moved a take a write
tailed between its sides moved, its base-vs-base control the same one). Refused while a seed is in flight or due, and
exit 2 when either is not the same before the base, between the sides and after the head. Kept base sides and the kept
JVM's corpora are keyed by it, so nothing resolved off one copy is served against another. A side whose live asks
failed past $KINOWO_WHAT_IF_MAX_FAILED_ASKS (25) is exit 2 too, and is not kept: an unanswered question is a gap the
side takes as pending, and the diff would read every one as a move.

Exit 0 done (CLEAN or BLOCKING is printed per country), 1 --verify found a take the selection missed, 2 usage / a
side failed / HARNESS ERROR (mirror not ready, re-seeded during the run, answers failed, or a head of the base's very
tree -- a control -- moving any take: 2026-11-19, lane E2's control moved the same 462 US takes as its change). Every JVM takes a slot of
scripts/jvm-gate.sh."""
from __future__ import annotations

import os
import subprocess
import sys
import time
import traceback
from typing import Dict, List, NoReturn

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import machine  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402
from identity_what_if_cli_context import IDENTITY_TOOLS, TOOL, Context, Exit  # noqa: E402
from identity_what_if_cli_flow import WhatIf  # noqa: E402
from identity_what_if_cli_options import Help, Options, Usage, parse  # noqa: E402
from identity_what_if_cli_resolve import KEPT_PATTERN, KeptJvms  # noqa: E402

ME = os.path.realpath(__file__)


def keep_served(pid: str, env: Dict[str, str]) -> NoReturn:
    """THE KEEPER of a kept JVM: this process becomes identity-tools.sh's identity_kept_jvm_keep, holding the JVM slot
    it was handed on descriptor 7 (jvm-gate.sh's jvm_slot_keeper_start) for the JVM's whole life."""
    command = tool_shell.shell_command(env, IDENTITY_TOOLS, "identity_kept_jvm_keep", [pid, KEPT_PATTERN])
    os.execvpe(command.argv[0], list(command.argv), dict(command.env))


def start(command: tool_process.Command) -> "subprocess.Popen[bytes]":
    """A command in the background (the head's compile), its stdout dropped, its stderr this tool's."""
    return subprocess.Popen(list(command.argv), env=dict(command.env), stdout=subprocess.DEVNULL)


def unchecked(e: Exception) -> int:
    """An exception nothing expected: said, never a bare traceback-less status."""
    frame = traceback.extract_tb(e.__traceback__)[-1]
    where = f"{os.path.basename(frame.filename)}:{frame.lineno}: {type(e).__name__}: {e}"
    print(f"HARNESS ERROR: {TOOL} stopped at an unchecked failure, {where}", file=sys.stderr, flush=True)
    traceback.print_exception(type(e), e, e.__traceback__)
    return 2


def run(ctx: Context) -> int:
    tool = WhatIf(ctx, ME, start)
    try:
        return tool.main()
    finally:
        tool.clean_up()


def guarded(ctx: Context) -> int:
    tool_cli.raise_on_signals()
    try:
        return run(ctx)
    except Exit as e:
        return e.code
    except tool_cli.Interrupted as e:
        return 128 + e.signum
    except Exception as e:  # noqa: BLE001 -- the one place an unexpected failure is said
        return unchecked(e)


def main(argv: List[str], env: Dict[str, str]) -> int:
    sys.stdout.reconfigure(line_buffering=True)  # type: ignore[attr-defined] # in order with stderr, as the shell's
    if argv[:1] == ["--keep-served"] and len(argv) > 1:
        keep_served(argv[1], env)
    if argv[:1] == ["--stop-served"]:
        KeptJvms(Context(Options(argv=argv), env, tool_process.SubprocessRunner(), time.time, time.sleep)).stop_all()
        return 0
    try:
        o = parse(argv, env)
    except Help:
        print(__doc__ or "")
        return 0
    except Usage as e:
        print(e, file=sys.stderr)
        if e.usage:
            print(__doc__ or "", file=sys.stderr)
        return 2
    return guarded(Context(o, env, tool_process.SubprocessRunner(), time.time, time.sleep))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
