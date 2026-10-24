# Agent workflow: the incidents behind the rules

`CLAUDE.md` states the rules; `.claude/skills/agent-ops/SKILL.md` holds the mechanics. This file keeps the dated
incidents that produced them, so the rules can stay short. Append here when a new incident changes a rule.

## Uncommitted work and the root checkout

- One session found **92 files of uncommitted WIP sitting directly on the `main` checkout** — a coherent "drop two
  cinemas" task that was never committed — plus half a dozen abandoned worktrees each carrying dirty,
  never-committed feature work (mobile day-carousel, tuning screens), all at risk of being pruned away. Uncommitted
  WIP strands work on a stale base, blocks the next agent's `git rebase`/`pull`, and gets silently swept or lost.
  Hence: own worktree, never yield dirty, labelled stash or `wip:` commit, and the root-checkout guard hooks
  (`scripts/claude-hooks/root-checkout-guard`, `scripts/hooks/pre-commit`).

## No PRs, never push a feature branch

- Breached twice — 2026-07-02 and 2026-07-19 — each costing a close-PR + delete-remote-branch cleanup. The bot
  workflows that open PRs (decoration-discovery, identity-refit, identity-capture, discover-cinemas,
  retire-venues; since 2026-10-22/24 nix-update's two, here and in movies-gitops) are the deliberate exceptions.
- 2026-10-16: a push of `HEAD` while a rebase was stopped landed 7 commits at once — hence push a named
  `<sha>:main`, never mid-rebase `HEAD`, and never from the shared root (others detach it).

## Killing processes by pattern

- One `pkill -f identity-slice` killed other agents' batches. Hence `scripts/agent-procs kill --mine`; the tool has
  deliberately no pattern-wide kill.

## Waiting on CI and long runs

- A hand-written `gh run view` / `sleep` loop read jq's `null null null` as a verdict. Hence `scripts/ci-wait`.
- 2026-10-18: foreground `until grep -q …; sleep` loops held agents in 10-minute polls they could not leave (the
  `agent-time` buckets "poll: slice" / "poll: measure"). Hence `scripts/wait-for` in the background.
- 2026-10-15: a 2h22m agent was ~45 minutes of work behind four serial CI runs and a broken push. Hence the speed
  habits (dispatch variants at once, prove CI gains on CI, Linux-test scripts, work while CI runs).

## Batching pushes to main

- 38% of Main runs in the four weeks to 2026-09-24 ended cancelled by a newer push (`main.yml`'s
  `cancel-in-progress: true`). Cancelling is kept on purpose: the superseding run tests a superset of the cancelled
  commit, Actions minutes cost nothing on this public repo, and letting runs finish would either make every push
  wait behind the previous one's ~12 minutes or, side by side, break the 20-runner budget (`CiRunnerBudgetSpec`).
  The cost is signal — a cancelled run says nothing about its commit — so pushes are batched instead.

## The red-build gate (user decision 2026-10-14)

- On 2026-10-13/14, 50 of 96 identity commits landed while the Identity model convergence suite was red;
  3f11e2cc3's wrong switches hid behind it for six hours, and three gates and label batches each turned it red again.
  Hence: while Main, Identity model convergence or Record scrape fixtures is red, only commits carrying
  `Fixes-Red: <run id>` may touch production paths; tools-only commits land freely.
- The gates were then changed to judge only what a commit's CODE changed: the identity takes check resolves one
  recording at the base and at the head and fails on a take moved, a suspect take new at the head, or a corpus-shape
  finding only the head has. What a recording brings (a suspect it carries, a marker in a query) stands at both, so
  it never turns these red; it goes to the **Identity findings report** (one issue per country, red after 3 days
  untriaged), which is not a gating workflow, because no push makes or mends a recording's data.
- `infra/kubernetes/` is still in the hook's production-path pattern but is gitignored since the manifests moved to
  `movies-gitops`, so no commit can touch it; `CLAUDE.md` no longer lists it.

## Test reruns

- 2026-10-19 (user): "remove all test rerunning on red, always investigate and fix flakes." A red is investigated to
  its root cause; a flaky test is fixed, never rerun to green. CI's rerun mechanisms are being removed accordingly.
