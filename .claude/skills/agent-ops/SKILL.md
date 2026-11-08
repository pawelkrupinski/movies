---
name: agent-ops
description: How an agent works in this many-agent repository — its own worktree, the landing flow (ff-merge while GitHub is suspended, `<sha>:main` push when it is back), batching pushes, the red-build gate, process hygiene (agent-procs, never pkill by pattern), waiting on long runs and CI (wait-for, ci-wait), where time went (agent-time), the kept sbt server (sbt-warm, `--serve`, JVM slots), and the worker/web downtime policy. Use when starting a task, landing or pushing, waiting on anything, running sbt, killing a process, or restarting a deployment.
---

# Agent operations

The rules themselves are in `CLAUDE.md`; this skill holds the mechanics. Dated incidents behind them:
`docs/agent-workflow-history.md`.

## Worktree and landing flow

- **Work in your own worktree, never the root checkout** (`/Users/pawel/projects/movies`, branch `main`, shared,
  must end every session clean). A worktree may be a sibling directory or under `.claude/worktrees/` (gitignored):
  `git worktree add -b <branch> ../movies-<branch> origin/main` (from local `main` while GitHub is unreachable).
  Tool calls start in the root checkout: `cd` into your worktree in **every** command.
- Enforced: `.claude/settings.json` runs `scripts/claude-hooks/root-checkout-guard`, which denies an Edit/Write to
  any non-ignored root path and reports a Bash command that dirties the root or switches its branch;
  `scripts/hooks/pre-commit` refuses a commit in the root (`ALLOW_ROOT_COMMIT=1` for a person doing it on purpose).
  `scripts/root-checkout-check` (run by `agent-procs` and every push) warns when the root is off `main` or dirty.
- **The only ref that reaches origin is `main`.** Never push a feature branch, never open a PR (the bot PRs
  CLAUDE.md lists, the `chore/identity-capture` verdict push and `scripts/mobile-ship.sh`'s own release commit are the
  exceptions — see `CLAUDE.md`). Before typing `git push` or `gh pr create`, run `git rev-parse --abbrev-ref HEAD` and name the
  ref you push (`<sha>:main`); a push of anything but `main` violates the rule.
- **Landing:** rebase onto main → every relevant layer green locally → land → `git worktree remove <path>` and
  `git branch -d <branch>` (your own, merged: `-d` refuses an unmerged one; never `-D`). Deleting a branch you didn't
  create, or an unmerged one, needs the user's OK first.
  - *While GitHub is suspended* (fetch/push 403): land with `git merge --ff-only <branch>` in the clean root
    checkout on `main`; nothing is pushed.
  - *When GitHub is back:* `git fetch && git rebase origin/main`, then push the named commit from your own
    worktree: `git push origin <sha>:main`. Never `HEAD` while a rebase is stopped (a stopped rebase landed 7 commits
    at once), never `git push origin main` from the shared root (others detach it).
- **Subagents** commit on their own branch, rebase, and hand back to the agent that spawned them. A subagent lands
  only when every gate is already green locally; it never waits on CI — the lead (one landing agent) does.
- **Never yield with a dirty tree you own:** commit (preferred) or `git stash push -u -m "<what + why>"`. An
  interrupted task still owes a checkpoint: a `wip: <state>` commit on your own branch or a labelled stash. Note
  `git stash` is shared by every worktree of the clone.
- **Don't strand work on a stale base:** behind main → rebase before piling on more.
- **Touch only what's yours:** dirty files you did not create are a co-agent's WIP — surface them; never
  `git add -A`, commit, stash or discard them.

## Pushing to main (when GitHub is reachable)

A push to `main` starts ~20 jobs. Since 2026-10-22 the run in flight is NEVER cancelled: it finishes and publishes,
a newer push queues behind it, and an even newer push replaces that queued run, which shows as cancelled (github.com's
one running, one pending). Until then a newer push cancelled
the run in flight, and on 2026-10-21 39 of 51 runs died that way. Still batch, because every run that does start
takes ~20 min of runners:

- Push when a coherent change is locally green, not after every commit; several commits in one push are one run.
- If the run for your previous push is under ~10 minutes old, keep committing and push once.
- Judge `main` by the newest COMPLETED run, never a superseded (skipped) one.
- Always fetch + rebase immediately before pushing. If a push's CI fails, fix forward; don't undo the push.

## The pre-push hook and the red gate

Install once per clone: `scripts/install-hooks.sh` (shared, relative `core.hooksPath=scripts/hooks`, so every
worktree runs its own copy). The full check inventory, what selects each check, and the red-gate design are in the
header of `scripts/hooks/pre-push`. Run it without pushing: `scripts/hooks/pre-push --range <base>..HEAD`.

- Checks are selected by the paths the push changes (eslint, roster drift, snapshot stamp, one warm sbt run of the
  lint specs — every repo-reading lint spec too on a change under `scripts/`, `.github/`, `.claude/`, `CLAUDE.md`,
  `build.sbt`, `project/` or test trees — dashboards, scripttests incl. the Linux leg when Docker is up, actionlint,
  shellcheck on added warnings, oldrefs, ios compile, judgements lint).
- **Red gate:** before the path checks, the hook reads the newest COMPLETED run on main of Main, Identity model
  convergence and Identity takes -- exactly those three (the owner, 2026-10-27; Identity takes, the takes rows, since
  2026-11-08; one bounded `gh run list` each; gh unreachable → warned, not blocked).
  While one is red, every pushed commit touching a production path (`*/src/main/`, `conf/`, `build.sbt`,
  `project/`, `infra/nix/`, `Dockerfile`) carries `Fixes-Red: <run id>` naming a red run. Tools-only commits
  (scripts, docs, skills, tests, fixtures, `test/resources/`, `.github/`) land without one. The Identity findings
  report (a recording's data findings, one issue per country) is not gating.
- **Check first, then push.** git opens the push's ssh connection before it runs the hook, and a connection held
  through a 20–76 min hook can die (a network blip): the hook prints "all clear" and the push dies ("closed by remote
  host", "Broken pipe", "unexpected disconnect while reading sideband packet"). So run
  `scripts/hooks/pre-push --range origin/main..HEAD` first, then `git push origin <sha>:main`: the hook reuses the stored
  pass and the connection lives seconds. A push that died after "all clear" is re-pushed as is — the same reuse. The
  hook says so itself when it ran over `PREPUSH_LONG_SECONDS` (300).
- Bypasses — always say so in your report: `PREPUSH_SKIP=redgate`, `PREPUSH_SKIP="<checks>"` (names: `redgate`,
  `lintspecs`, `linux`, `sbt`, `ios`, … — every check name in the header), `git push --no-verify` (all).

## Processes

- **Never `pkill -f` / `killall` by pattern** — other agents' processes match it. `scripts/agent-procs kill --mine`
  (SIGTERM what runs under the current worktree; `--worktree P`, `--dry-run`, `--force`).
- A call to `scripts/sbt-warm` whose server vanished mid-call ends within ~30 s (server liveness, exit 75 `STALLED ...
  its server vanished` after one retry in a fresh server), and its client is KILLed if it ignores TERM.
- `scripts/agent-procs` audits every worktree (dirty, unmerged vs main, lock owner, its processes);
  `scripts/agent-procs prune [--yes]` removes the removable ones and their merged branches.
- `scripts/agent-time [--longest N] [--agent <id> --top N]` shows where a session's subagents spent their hours
  (polls and background waits by what they waited on, idle gaps, sbt runs by shape, the pre-push hook).

## Waiting

- **Your own long run** (slice batch, local measure, sbt suite, capture): `scripts/wait-for <log> <pattern>
  [--pid P] [--timeout S]` with `run_in_background`, then act on its notification. Never a foreground
  `until grep; sleep` loop.
- **CI:** `scripts/ci-wait`, never a hand-written `gh run view`/`sleep` loop. `--run <id>`; `--workflow <w> --sha
  <commit>` (waits for the run to appear, then to finish); several `--workflow`s for a set; `--or-later` (a green run
  at that commit or later), `--until-green` (wait through reds), `--any-completed` (each workflow's newest verdict).
  A cancelled run is followed to the run that superseded it (`--no-follow`: exit 3). Exit 0 green, 1 red,
  2 timeout/unreadable, 3 cancelled. Run it in the background.

## sbt: `scripts/sbt-warm`

- Run sbt through `scripts/sbt-warm "<command>"`: one kept server per worktree (extras stopped on the next call;
  stopped by its keeper after 10 idle minutes, `KINOWO_SBT_WARM_IDLE_MINUTES`, never under a running command), a fresh worktree's targets seeded from the newest local main build (a cold 232 s spec
  run becomes ~50 s). Don't mix with a plain `sbt` (or a bare `sbt --client`) in the same checkout while the server
  lives: only a call through the script counts as in use. Details: `test-layers`.
- **A server gone missing?** `~/.cache/kinowo-sbt-warm/stops.log` has a line for every server and keeper sbt-warm
  stopped (when, why, which process asked) and a `vanished` line for one that ended otherwise, naming the JVM reaper's
  kill (`agentctl reap`'s `~/.cache/agentctl/reap.log`) or a crash file when one exists; each keeper's own log is `<key>.log` beside it.
  `--stop` stops the checkout's keeper with its server and returns once the keeper is gone and its JVM slot free
  (waiting up to 10 s for a keeper still starting, SIGKILL past 10 s of SIGTERM). The reaper's hog rule spares a
  server whose keeper lives.
- **Long-lived servers** (the fixture server, any `runMain` that serves until stopped):
  `scripts/sbt-warm --serve '<command>'` — it keeps the checkout's server (and so its slot) in use until stopped, so
  stop it when done.
- **One JVM, one slot:** sbt-warm, identity-slice and identity-measure-local share the machine-wide JVM slots
  (`KINOWO_JVM_SLOTS`, default 4; `scripts/jvm-gate.sh`) and size their JVMs for 4 cores. A kept sbt server and an
  `identity-slice --serve` JVM each hold a slot for their whole life, idle or not (held by a keeper process beside
  them, `jvm_slot_keeper_start`); commands run in them take none, and each stops after 10 idle minutes. So an idle one
  of yours can queue another agent: `scripts/sbt-warm --stop` when you are done with a worktree
  (`identity-slice --stop-served` stops every agent's kept slice JVMs, not only yours). Never lower
  `KINOWO_JVM_SLOTS` to get ahead — wait for a slot.
  This is the "heavy-run" the older docs named: there is no `heavy-run` command. `identity-measure-local`,
  `identity-measure --footprint` and `identity-landing-check --local` (via sbt-warm / it-local.sh) take their slot
  themselves, so run them directly; never route around the gate.
- **The memory gate** (`KINOWO_MEMORY_GATE_GB` 20 paged AND `KINOWO_MEMORY_GATE_FREE_GB` 6 free) waits as long as
  memory is short, with a status line every `KINOWO_MEMORY_GATE_STATUS_SECONDS` (180); the slot wait goes ahead after
  `KINOWO_JVM_SLOT_MAX_WAIT` (1800 s) only while memory is fine. Escape hatch: `KINOWO_MEMORY_GATE_MAX_WAIT=<s>` goes
  ahead after that long; `KINOWO_MEMORY_GATE=0` turns the gate off. Say so in your report if you use either.

## Speed habits

- Dispatch every variant of a CI experiment at once (refit and measure dispatches each get their own lane).
- A CI speed-up counts only when a CI run proves it (a Mac is not the 2-core runner).
- Run a changed script's self-test on Linux (`scripts/run-script-tests --linux`) before landing it.
- Start local checks beside a dispatch, not after it; wait in the background, never in foreground sleep loops.

## Downtime policy

- **Workers** (`worker-pl`, `worker-uk`, `worker-de`, `worker-us`, `worker-es` Deployments on k3s) may go fully down
  without ceremony: scale to zero and back or delete the pod, watch Flux roll out, sanity-check, move on. Up to ~15
  minutes is in the "just do it" bucket — the read model keeps serving what is projected.
- **The web / read tier (`kinowo.net`, `showtimes.cc`) must not go fully down if there is any way not to.** Prefer a
  rolling deploy that keeps one pod serving; when moving or resizing a node, bring the replacement up first. A brief
  5xx window during a rolling deploy is fine, and so is a genuinely unavoidable short full outage (a host move) — keep
  it short and say out loud afterwards that the tier was fully down. No elaborate zero-downtime orchestration.
- This is about lifecycle only. Destructive data ops (dropping a collection, truncating, deleting a branch you
  didn't create or one not merged) still need explicit confirmation.
- Ops entry points: Grafana <https://grafana.kinowo.net> (dashboards in
  `infra/nix/files/monitoring/grafana/dashboards/apps/`); secrets via the Touch ID vault —
  `secrets run movies <KEY> -- <command>`, never a `.env.local`.
