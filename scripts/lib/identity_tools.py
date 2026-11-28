"""WHAT THE LOCAL IDENTITY TOOLS SHARE, THE FUNCTIONS OF scripts/identity-tools.sh THAT NEED NO CALLING SHELL (scripts/lib;
identity-slice and its --explain, identity-judge-cards, venue-replay, title-trace, recording-diff, hard-clusters.sh and
listing-history, through the shim's functions of the same names): the shared cache, a recording or a capture downloaded
once into it, a ref's sources laid out once, a tree's worker test classpath compiled once.

  python3 -I -B scripts/lib/identity_tools.py <command> <args...>      (the shim passes IDENTITY_REPO, IDENTITY_TOOL,
        IDENTITY_FILLS and KINOWO_IDENTITY_CACHE / KINOWO_GH / KINOWO_GH_REPO)

  capture <src> <cc>...           the directory holding <cc>.json.gz for each cc, and its judgements (identity_capture)
  judgements <dir>                the judgements that judge the capture in <dir> (identity_judgements)
  tree <ref> <file>...            the ref's sources laid out once under the cache, the named files this checkout's
  tree-judgements <sha> <dir>     the ref's judgements in <dir>
  tools-hash <file>...            the hash of this checkout's files
  classpath <tree>                the tree's worker test classpath, sbt run only when a source is newer
  classpath-fresh <tree>          0 when the kept classpath is newer than every source
  classpath-line <sbt log>        the last line of it that IS a classpath
  recording <run> <cc> [corpus]   the fixtures directory holding the run's corpus/ and enrichment-<cc>/
  recording-cached <run> <cc>     that directory when the cache already holds it, else fails
  recording-run <run|prod-latest> <cc>    the run id a --recording names
  recorded <fixtures> <cc> [corpus]       0 when the recording is laid out
  newest-recording [cc]           the newest successful "Record scrape fixtures" run (holding that country's corpus)
  takes-download <run> <cc> <dir> a convergence run's <cc>-takes, as <dir>/<cc>-takes
  main-ci-over <remote> <commit> <prefix> / measure-branch <commit> [tag]
  push-measure-branch <error file> <remote> <commit> <branch>   its failure's reason into <error file>
  what-if-verdicts-file|what-if-record-verdict|what-if-verdicts <head> <base> ...   the what-if verdict record

Each fails (non-zero, said on stderr) when what it needs cannot be read -- gh unreachable, a download cut short -- and never
hands back a stale or partial copy instead. Every gh call is bounded; a wait for another agent's download, layout or compile
of the same thing has a deadline, and a lock is free the moment its holder has gone. What a child writes to stdout is the
caller's stderr (a function's stdout is its answer)."""
from __future__ import annotations

import os
import signal
import sys
from typing import Callable, Dict, List, MutableMapping, Sequence, TextIO

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))  # run as `python3 -I`: its own directory is not there
import identity_tools_cache as cache  # noqa: E402
import identity_tools_refs as refs  # noqa: E402
import machine  # noqa: E402
from identity_context import Context, Failed  # noqa: E402

Handler = Callable[[cache.Tools, List[str]], int]


COMMANDS: Dict[str, Handler] = {
    "capture": cache.capture, "judgements": cache.judgements, "tree": cache.tree,
    "tree-judgements": cache.tree_judgements, "tools-hash": cache.hash_of, "classpath": cache.classpath,
    "classpath-fresh": cache.classpath_fresh, "classpath-line": cache.line_of, "recording": cache.recording,
    "recording-cached": cache.recording_cached, "recording-run": cache.recording_run, "recorded": cache.recorded,
    "newest-recording": cache.newest_recording, "takes-download": refs.takes_download,
    "main-ci-over": refs.main_ci_over, "measure-branch": refs.measure_branch,
    "push-measure-branch": refs.push_measure_branch, "what-if-verdicts-file": refs.verdicts_file,
    "what-if-record-verdict": refs.record_verdict, "what-if-verdicts": refs.verdicts,
}


def ended_by(signum: int, _frame: object) -> None:
    """A signal ends this process the way an exit does, so what it holds (a lock, a download directory) is given back."""
    raise SystemExit(128 + signum)


def run(command: str, args: Sequence[str], env: MutableMapping[str, str], answer: TextIO) -> int:
    handler = COMMANDS.get(command)
    if handler is None:
        sys.stderr.write(f"identity_tools.py: no command {command}\n")
        return 64
    repo = env.get("IDENTITY_REPO") or os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
    tools = cache.Tools(Context.from_environment(env, repo), answer)
    try:
        return handler(tools, list(args))
    except Failed as failed:
        return failed.code
    except (OSError, IndexError) as error:
        tools.ctx.log(f"{command} failed: {error}")
        return 1


def main(argv: List[str]) -> int:
    if not argv:
        sys.stderr.write("usage: identity_tools.py <command> <args...> (see the header)\n")
        return 64
    for signum in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(signum, ended_by)
    answer = os.fdopen(os.dup(1), "w")
    os.dup2(2, 1)  # what a child writes to stdout is stderr, as the shell's `>&2` was: stdout is the answer only
    return run(argv[0], argv[1:], machine.environment(), answer)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
