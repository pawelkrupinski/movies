#!/usr/bin/env python3
"""THE IDENTITY TAKES CHECK, for one country inside its hermetic convergence row: resolve the pinned recording the row
just replayed with the BASE commit and with the HEAD, and fail on any listing the code moved to another film (lost or
changed) that nobody accepted, or onto a suspect take nobody judged (scripts/ci/identity_takes_diff.py); and run the
corpus-shape specs over the same recording at both, failing on a finding only the head has (corpus_shape_diff.py).

Base vs head on ONE recording is what makes a red verdict mean "the code did it": a new nightly recording changes both
sides at once and so moves nothing here. What the recording itself brings -- a suspect take, a marker left in a query --
stands at base and head alike: listed on the summary, never failed, triaged from the Identity findings report.

  identity_takes_check.py <cc> <out dir>     (scripts/ci/identity-takes-check.sh: the workflows' name for it)
    env: GH_TOKEN, GITHUB_REPOSITORY, GITHUB_SHA, MONGODB_URI, HEAP (sbt -Xmx, default 6g), TAKES_BASE (optional),
         STATUS_CONTEXT (default "identity takes (<cc>)"), GITHUB_STEP_SUMMARY (optional), TAKES_STORE (the release its
         resolves are stored on and reused from; empty stores nothing), GITHUB_REF, the pinned pair's
         KINOWO_CONVERGENCE_PIN_CORPUS_RUN / _TREE_ASSET / _FILL_ASSETS, RUNNER_TEMP/laid-fill, SHAPE_COMMAND (the
         country's `corpusShape*` sbt alias; empty checks no corpus shape), TAKES_SIDE (both, head or base: THE BASE ROW),
         TAKES_BASE_JOB (the base row's job name), GITHUB_RUN_ID, TAKES_BASE_WAIT / TAKES_BASE_POLL (seconds: how long,
         and how often, the head waits for the base row), TAKES_AA (true: THE A/A SELF-TEST), TAKES_SKIP_MODE

Its parts: scripts/ci/identity_takes_check_base.py scripts/ci/identity_takes_check_store.py
scripts/ci/identity_takes_check_sides.py scripts/ci/identity_takes_check_head.py scripts/ci/identity_takes_check_runs.py
scripts/ci/identity_takes_check_verdict.py scripts/ci/identity_takes_check_context.py scripts/ci/identity_takes_check_paths.py
THE BASE: identity_takes_check_base.py. THE STORE (a side's results keyed by identity code and inputs, reused):
identity_takes_check_store.py. The sides: _sides.py (the base's, and THE BASE ROW: TAKES_SIDE=base stores the base's side
on a runner of its own while TAKES_SIDE=head runs the head's and waits for it), _head.py (THE HEAD'S OWN RESULTS, its
session), _runs.py (the sbt runs). THE SKIP GATE, THE SHADOW and the verdict: _verdict.py. THE A/A SELF-TEST
(TAKES_AA=true, the `takes-aa` row): this check's own harness against ITSELF -- the base is the head, nothing reused,
stored or gated -- so any take differing between two resolves is the HARNESS's fault (harness_aa.py takes: exit 1).
Writes <out>/{base,head}/decisions-<cc>.jsonl and ratchet-takes.tsv, <out>/{base,head}/shape/, <out>/base.txt,
<out>/recording.txt, <out>/timings.tsv and <out>/verdict.txt.
Exit: 0 passed, 1 an unaccepted move or a new suspect or corpus-shape finding, 2 the check could not run -- no verdict,
which the leg posts as an `error` status and fails on: a check that cannot compute its diff never passes."""
from __future__ import annotations

import os
import shutil
import sys
import tempfile
from typing import List, Mapping

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(os.path.dirname(HERE), "lib")]
import identity_takes_check_base as base_module  # noqa: E402
import identity_takes_check_context as ctx_module  # noqa: E402
import identity_takes_check_head as head_module  # noqa: E402
import identity_takes_check_sides as sides  # noqa: E402
import identity_takes_check_store as store_module  # noqa: E402
import identity_takes_check_verdict as verdict_module  # noqa: E402
import ci_machine  # noqa: E402

Context = ctx_module.Context


def start(ctx: Context, aa: bool) -> str:
    """The base, named (base.txt, recording.txt) and fetched."""
    base = base_module.resolve(ctx, aa)
    ctx_module.write(ctx.path("base.txt"), base + "\n")
    named = ctx.env.get("KINOWO_CONVERGENCE_NAMED_RECORDING", "")
    # which recording the takes are over: scripts/identity-measure reuses only a pinned one's run for a later measurement
    recording = f"named {named}" if named else f"pinned {ctx.env.get('KINOWO_CONVERGENCE_PIN_CORPUS_RUN', '')}"
    ctx_module.write(ctx.path("recording.txt"), recording + "\n")
    ctx.say(f"{ctx.label}: base {base}, head {ctx.head}")
    base_module.fetch(ctx, base)
    return base


def state_of(ctx: Context, base: str, aa: bool, side: str, takes: bool, shape: bool) -> sides.State:
    store, laid = store_module.store_named(ctx, aa)
    keys = store_module.Keys(ctx, laid)
    tree = os.path.join(ctx.temp, "identity-takes-base")
    state = sides.State(base=base, side=side, aa=aa, takes_moves=takes, shape_moves=shape,
                        store=store_module.Store(ctx, store, keys, side), keys=keys,
                        traces=os.path.join(ctx.temp, "identity-trace"), tree=tree,
                        final_tree=tree if side == "both" else tree + "-final")
    # an earlier run that ended before removing its base checkouts left them registered: the directories go, and so
    # does their registration (a missing but registered worktree refuses the next `git worktree add`)
    for directory in (state.tree, state.final_tree, state.traces):
        shutil.rmtree(directory, ignore_errors=True)
    os.makedirs(state.traces)
    ctx.git.ok("worktree", "prune")
    ctx_module.write(ctx.path("verdict.txt"), "")
    ctx_module.write(ctx.path("timings.tsv"), "phase\tseconds\thow\n")
    return state


def nothing_moved(ctx: Context, base: str) -> int:
    line = (f"{ctx.label}: no identity code changed since {base}, nor a corpus-shape check or allowlist -- nothing can "
            "have moved")
    ctx.say(line)
    ctx_module.write(ctx.path("verdict.txt"), line + "\n")
    return 0


def check(ctx: Context, aa: bool, side: str) -> int:
    base = start(ctx, aa)
    takes, shape = store_module.moves(ctx, base, aa)
    if not aa and not takes and not shape:
        return nothing_moved(ctx, base)
    state = state_of(ctx, base, aa, side, takes, shape)
    base_side = sides.Base(ctx, state)
    if side == "base":
        return base_side.row()
    gate = verdict_module.Gate(ctx, state)
    gate.ask()
    if state.takes_moves:
        for name in ("head", "base"):
            os.makedirs(ctx.path(name), exist_ok=True)
        if side != "head":  # beside a base row, its final takes are that row's (awaited after the session)
            base_side.final_reused_or_started()
    head = head_module.Head(ctx, state, base_side)
    head.stored()
    head.final_start()
    head.run_session()
    if state.takes_moves:
        head.takes()
    elif not gate.skipped:
        ctx.verdict(f"{ctx.label}: no identity code changed since {base} -- no take can have moved")
    shapes(ctx, state, head)
    sides.remove_worktrees(ctx, state.tree, state.final_tree, head.final_tree)
    return judged(ctx, state, gate)


def shapes(ctx: Context, state: sides.State, head: head_module.Head) -> None:
    if state.shape_moves:
        head.shape()
    elif ctx.env.get("SHAPE_COMMAND"):
        ctx.verdict(f"{ctx.label}: no corpus-shape check, allowlist or identity code changed since {state.base} -- no "
                    "finding can be new")
    else:
        ctx.verdict(f"{ctx.label}: corpus shape not checked (no SHAPE_COMMAND)")


def judged(ctx: Context, state: sides.State, gate: verdict_module.Gate) -> int:
    verdict = verdict_module.Verdict(ctx, state, gate)
    if state.aa:
        return verdict.aa()
    verdict.judge(lambda: verdict_module.refresh_marker(ctx))
    return verdict.shadow() or verdict.code()


def side_of(env: Mapping[str, str], cc: str) -> str:
    side = env.get("TAKES_SIDE") or "both"
    if side not in ("both", "head", "base"):
        print(f"::error::identity takes ({cc}): TAKES_SIDE is both, head or base, not {side}")
        raise ctx_module.Stop(2)
    if side == "head" and not env.get("TAKES_BASE_JOB"):
        return "both"  # no base row to wait for
    return "both" if aa_of(env) else side


def aa_of(env: Mapping[str, str]) -> bool:
    return env.get("TAKES_AA", "") in ("true", "1")


def main(argv: List[str]) -> int:
    """The composition point: the environment, the working checkout, the real runner, gh and git."""
    if len(argv) < 2:
        print("usage: identity_takes_check.py <cc> <out dir>", file=sys.stderr)
        return 1
    cc, env = argv[0], dict(ci_machine.environment())
    os.makedirs(argv[1], exist_ok=True)
    own = "" if env.get("RUNNER_TEMP") else tempfile.mkdtemp()  # outside Actions: one of this run's own, removed at exit
    ctx = ctx_module.live(cc, os.path.abspath(argv[1]), env, own)
    git = ctx.git
    try:
        return check(ctx, aa_of(env), side_of(env, cc))
    except ctx_module.Stop as stop:
        return stop.code
    finally:
        if own:
            shutil.rmtree(own, ignore_errors=True)
            git.ok("worktree", "prune", quiet=True)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
