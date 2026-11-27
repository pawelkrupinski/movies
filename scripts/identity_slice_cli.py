#!/usr/bin/env python3
r"""Reproduce ONE listing's identity take — matched or not, model and agreement — from a recording, in seconds, at one or
two git refs (docs/design/identity-slice.md). The listing's families and their corpus context are cut out of a "Record
scrape fixtures" recording with every answer they read (TMDB, IMDb, the families, posters, catalogue links, venue pages)
into a hermetic slice; each ref's code decides the slice again.

  scripts/identity-slice <cc> SELECTOR... [--ref A [--ref B]] [--recording RUN [--fills LIST]] [--widen N] [--emit-case]
  scripts/identity-slice <cc> --batch FILE --ref BASE --ref HEAD [--recording RUN]
  scripts/identity-slice <cc> SELECTOR... --bisect A..B [--recording RUN]
  scripts/identity-slice --log '<take-change log line (JSON)>'
  scripts/identity-slice --compiles REF
  scripts/identity-slice <cc> --faithfulness CONVERGENCE_RUN [--sample N] [--seed S]
  scripts/identity-slice <cc> --explain [--venue S] [--title S] [--ref A [--ref B]] [--fixture SRC] [--limit N] [--no-resolve]

  SELECTOR      --venue S --title S (substrings, case and accents aside; either alone), or --key 'venue|raw title' (exact)
  --ref         decide under that ref's code (each laid out and compiled once into the cache); two refs print their takes
                side by side, then every listing that moved. Without --ref: this checkout's code.
  --recording   the recording run, or `prod-latest` (the freshest complete nightly recording: prod's listings as of
                last night; scripts/identity-repro), default: the newest successful "Record scrape fixtures" run holding the
                country's corpus; older ones are tried in turn, each skip said. Every ref replays the
                SAME recording: the comparison is the code's, never the input's.
  --fills LIST  lay these fill assets over the recording's tree, as a hermetic convergence leg did: its log's
                "hermetic leg: corpus from recording run N ..., fills: fill-<cc>-N-... ..." names them. Without them a
                slice cut for that leg's red lacks the answers its flip read, and replays clean (UK "Rebuilding",
                "Dracula": 2026-11-01). The filled pair is cached apart from the bare one.
  --widen N     N more rings of context: the context listings' own families joined to the slice
  --orders N    also feed the slice to the INCREMENTAL model in N shuffled arrival orders (the worker's path) and name
                every listing whose take any order moves from the whole resolve's — nondeterminism, a mode flip
  --batch FILE  one selector a line (`venue<TAB>raw title`, `*` for every venue, or `--venue S --title S`): every slice
                at both refs in ONE JVM per ref, and the moves tallied against the judgements — cured, lost, new-wrong
  --bisect A..B the first commit of A..B whose take of the selected listings differs from A's (one ref compiled at a time)
  --emit-drift-slice  pin the listing as a DRIFT SLICE (Tier 1 of the drift gate): the slice copied whole to
                test/resources/fixtures/drift-slices/<cc>/<selector>/, then replayed through IdentityDrift's incremental
                sequence (--slice --widen, $KINOWO_DRIFT_SLICE_SEEDS seeds, default 12) with every answer it lacks read from
                the recording and kept in it, round after round until the slice is CLOSED: replayed alone it lacks no
                request and finds what its listings' replay with the recording behind it finds -- else the cut fails
                (exit 1, CUT NOT CLOSED). Then the slice's ARRIVAL WALK (PopulationMonotonicitySpec) with this
                checkout's code: each A -> B -> A take no `population` row of identity-flip-backlog.tsv names is
                appended to it, so the slice and every row main's spec demands of it land together (Main #2515); a
                walk that fails or breaches the rule contract files nothing (exit 6). A replay that crashes, runs out of memory, is killed or prints no "## DRIFT VERDICT:" line is "NO VERDICT: <cause>", exit 5, never RED or CLEAN. IdentityDriftSlicesSpec replays every such slice. A drift or flip red is not
                closed until its listing's slice is committed (docs/landing-process.md). This checkout's code only.
  --emit-case   pin the listing as a FAILING TEST: the slice copied to test/resources/fixtures/identity-slices/, its expected
                film taken from the judgements (never from the take), each shown with its own evidence; refused when
                they do not judge the listing. IdentitySliceCasesSpec replays every such slice.
  --modes N     also take the slice through N CHANGED projections — the periodic one's own agreement stage again, over
                EVERY listing with those at a random half of the venues republished, as tickChanged reads them (the
                scraped venues re-read, every other venue's held) — and name every listing whose take a changed one flips
  --drift       the WHOLE recording through production's incremental sequence instead of a slice (scripts.IdentityDrift):
                the model taken up with a share of the answers missing, ticks filing them, new venues' scrapes,
                re-scrapes, listings leaving and returning, two deploys (one changing the rules) through the stores' own
                BSON — and every listing whose take at a checkpoint differs from a whole resolve of the same listings
                named, beside the judgements; exit 1 on any test/resources/fixtures/identity-drift-allow.tsv does not
                name. No selector. At the first --ref (default this checkout), which must hold the tool. `--out` keeps
                the divergences' TSV. CI runs it in every hermetic leg's takes row (scripts/ci/identity-drift-check.sh)
  --families P  prod's filed family answers, which no recording holds: an unmatched-cluster capture (default this
                checkout's test/resources/fixtures/identity-unmatched/<cc>.json.gz), or `none`. Without them the
                families agree on nothing — as in a hermetic leg. The slice keeps those it read, named as prod's.
  --faithfulness capture  the agreement-taken listings (Agreed, Filled, Poster, …) of that capture as this code's agreement
                takes them, each sliced alone on the capture's recording and compared
  --threads N   decide N slices at once in the one JVM (default: half the cores, at most 8); the output keeps the batch's order
  --fresh       decide every slice again: by default a slice's result at a ref is kept (per recording, ref, tool files and
                filed answers, and the labels about its listings) and read back by every checkout asking it again
  --serve       keep a JVM per (ref, recording, country) with the recording opened and the ref's index loaded, and send
                the batch to it: no JVM start or corpus load per call. Each kept JVM holds one of the machine's JVM
                slots (scripts/jvm-gate.sh) for its whole life, through a keeper process beside it; a call reaching a
                live one takes none. It stops after $KINOWO_IDENTITY_SLICE_IDLE_MINUTES (default 10) minutes without
                a request -- never during one -- freeing the slot; `scripts/identity-slice --stop-served` stops
                every kept one now
  --out TSV     keep the takes (of the last --ref) as a TSV: selector key venue rawTitle film title basis … verdict
  --labels TSV  judge the takes by this identity-judgements.tsv (default this checkout's) — the same at both refs
  --compiles REF  lay REF out with this checkout's tool files dropped in and compile it, nothing else: the tool must
                compile into every ref it may be pointed at — a deployed worker's build, a bisect's oldest commit
                (scripts/identity-slice-old-ref-test.sh)
  --log LINE    a prod `identity take change:` line from VictoriaLogs: its country, venue and listing, at its build
  --explain     WHY a listing the unmatched-cluster capture holds took (or did not take) its film: the whole trace —
                billing and venue page, the captured model decision, the cluster re-resolved (keys, candidates, signals,
                refusals), the agreement stage's basis, fill rule and guards, the take and its label. Replays the capture,
                not a recording; a listing the MODEL took is not in it (slice it instead). scripts/identity-explain is an
                alias kept for the `repro` field prod's take-change lines print.
    --fixture SRC  the capture to explain from (default: this checkout's): a directory holding <cc>.json.gz, an
                "Identity capture" run id, or a git ref holding one. Its judgements judge it (a run's: this checkout's).
    --limit N, --no-resolve  at most N clusters; skip the re-resolve (the captured decision and the agreement only)
  --faithfulness RUN  the slice's take of a sample of listings against what that "Identity model convergence" run's leg
                took (its convergence-diagnostics `<cc>-takes`), at the run's commit and its pinned recording
  --prod-mirror [cc]  read prod's own state from the LOCAL prod mirror (scripts/local-mirror, :28017 — never prod): its
                filed family answers in preference to the capture's, the agreement stage's stored verdicts (the stage
                starts from them, as prod's does) and the operator pins; and print beside each take what prod's stored
                state takes now and why (model decision, trace, agreement verdict, correction, stored titles). With
                --faithfulness it also tallies the slice against prod's take. Checks the mirror's freshness first
                (scripts/local-mirror/freshness) and WARNs when it is older than the recording or more than
                $KINOWO_MIRROR_MAX_LAG_HOURS (default 6) hours behind — the slice would mix moments. [cc] must be the slice's.

Everything downloaded or compiled is kept in $KINOWO_IDENTITY_CACHE (default ~/.cache/kinowo-identity), shared by every
worktree: recordings/<run>/, slice/<run>/{index-<cc>-<code key>.json.gz, results/, slices/<selector>/}, refs/<sha>/. The
first call on a recording resolves its corpus whole once per CODE (~25-50 s) -- keyed by content (code_key), so a commit
or an edit changing no code reads the index another made -- later ones decide the slice alone (~1 s). A new ref's
compile is seeded from the nearest compiled ref (identity_nearest_compiled); one with no source changed keeps its
classpath. --emit-case goes through a kept JVM (--serve), so pinning cases one after another costs ~2 s each.
"""
from __future__ import annotations

import os
import sys
import traceback
from typing import Dict, List, NoReturn

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import machine  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402
from identity_slice_cli_context import IDENTITY_TOOLS, REPO, TOOL_FILES, Context, Exit, say  # noqa: E402
from identity_slice_cli_flow import Flow  # noqa: E402
from identity_slice_cli_keys import Keys  # noqa: E402
from identity_slice_cli_options import TOOL, Options, Parser, Usage  # noqa: E402
from identity_slice_cli_run import KEPT_PATTERN, Slicer  # noqa: E402

ME = os.path.realpath(__file__)
HELP = __doc__ or ""
# THE KEEPER's first and last lines in its log: which JVM it keeps, and whether it started holding a JVM slot
KEEPER_SAYS = ('IDENTITY_TOOL=identity-slice; shift; identity_log "keeper of kept JVM $1: pid $$, $(jvm_slot_riding && '
               'echo "holding JVM slot ${KINOWO_JVM_SLOT_HELD##*/}" || echo "no JVM slot yet")"')


def keep_served(pid: str, env: Dict[str, str]) -> NoReturn:
    """THE KEEPER of a kept JVM (`--keep-served <pid>`, started with the JVM slot's descriptor open: jvm-gate.sh's
    jvm_slot_keeper_start): this process becomes identity-tools.sh's identity_kept_jvm_keep, holding that slot for the
    JVM's whole life, idle or not -- taking one as soon as one is free when it started without -- and ending once the
    JVM has (its idle exit, --stop-served, a crash), which frees the slot. Its command line keeps `--keep-served <pid>`."""
    command = tool_shell.shell_command(env, IDENTITY_TOOLS, "identity_kept_jvm_keep", ["--keep-served", pid, KEPT_PATTERN],
                                       prelude=KEEPER_SAYS, then='identity_log "kept JVM $1 gone: done"')
    os.execvpe(command.argv[0], list(command.argv), dict(command.env))


def compiles(argv: List[str], ctx: Context) -> int:
    """--compiles REF: the ref laid out with this checkout's tool files dropped in and compiled, nothing else."""
    if len(argv) < 2 or not argv[1]:
        say(f"{TOOL}: 2: --compiles REF")
        return 1
    if not ctx.need_ref(argv[1]):
        return 2
    tree = ctx.shell("identity_tree", [argv[1], *TOOL_FILES])
    if tree.code != 0 or ctx.classpath(tree.stdout.rstrip("\n")) is None:
        return 1
    ctx.log(f"{argv[1]} compiles with this checkout's tool files")
    return 0


def unchecked(e: Exception) -> int:
    """An exception nothing expected: said, never a bare status."""
    frame = traceback.extract_tb(e.__traceback__)[-1]
    say(f"HARNESS ERROR: {TOOL} stopped at an unchecked failure, {os.path.basename(frame.filename)}:{frame.lineno}: "
        f"{type(e).__name__}: {e}")
    traceback.print_exception(type(e), e, e.__traceback__)
    return 1


def usage(e: Usage) -> int:
    if str(e):
        say(str(e))
    if e.usage:
        sys.stderr.write(HELP)
    return 2


def guarded(o: Options, env: Dict[str, str]) -> int:
    flow = Flow(Context(o, env, tool_process.SubprocessRunner()), ME)
    try:
        return flow.main()
    except Exit as e:
        return e.code
    except Usage as e:
        return usage(e)
    except tool_cli.Interrupted as e:
        return 128 + e.signum
    except Exception as e:  # noqa: BLE001 -- the one place an unexpected failure is said
        return unchecked(e)
    finally:
        flow.clean_up()


def helped(first: str) -> int:
    """--help: the header on stdout; no argument: on stderr, a usage error."""
    if first:
        sys.stdout.write(HELP)
        return 0
    sys.stderr.write(HELP)
    return 2


def verb(argv: List[str], env: Dict[str, str]) -> int:
    """--compiles REF and --stop-served: no country, no slice."""
    ctx = Context(Options(), env, tool_process.SubprocessRunner())
    if argv[0] == "--stop-served":
        Slicer(ctx, Keys(ctx), ME).stop_all()
        return 0
    return compiles(argv, ctx)


def main(argv: List[str], env: Dict[str, str]) -> int:
    sys.stdout.reconfigure(line_buffering=True)  # type: ignore[attr-defined] # in order with stderr, as the shell's
    first = argv[0] if argv else ""
    if first in ("-h", "--help", ""):
        return helped(first)
    if first == "--keep-served":
        keep_served(argv[1] if len(argv) > 1 else "", env)
    tool_cli.raise_on_signals()  # before the runner, whose handler passes a signal on to this one
    if first in ("--compiles", "--stop-served"):
        return verb(argv, env)
    try:
        o = Parser(env, REPO).parse(argv)
    except Usage as e:
        return usage(e)
    return guarded(o, env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
