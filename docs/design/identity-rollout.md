# Observability-first rollout of prod-behaviour identity changes (2026-11-06)

The owner approved P4 of the identity-and-convergence review of 2026-11-06. On 11-06 the identity model's input gate
deployed at 07:50Z and its gauges and alert at ~12:20Z, so listings held for up to 2.8 h went unseen for ~4.5 h, and
three deploys followed in five hours (gate hidden 18ea612c2af1, off d5320aa67d71, safe 5ec9cf7fc1a9). Three pieces
stop that from happening again. The goal is to detect a prod regression within 15 minutes of its deploy.

## 1. Gauges before behaviour (pre-push `driftguards`, a commit rule)

A commit that changes what the worker serves over time (the gate, holds, settle, asks) must do one of three things:

- change a gauge, alert or dashboard file in the same commit;
- name the EARLIER push's commit that did: `Gauges-Shipped: <sha>`. That commit must be an ancestor that is not in
  this push. A commit in the same push lands in the same deploy, so it does not count;
- say `Gauges: none -- <which series already see it, why>`.

These are the files each term covers:

- **Serving files** (`SERVING_FILES` in `scripts/ci/commit_rules.py`): `IdentityModelService`, `ModelInputGate`, `AgreementReadiness`,
  `PendingListings`, `AgreementHandlers`, `QuestionMemory`, `QuestionPolicies`, `QuestionRenewals`,
  `OutstandingQuestions`, `VenuePageWait`, `RetainedListings`, `ProjectionTriggers`, `ListingReadiness`, `PageWait`
  and `SettleReaper`.
- **Gauge files**: `worker/src/main/scala/services/metrics/`, `infra/nix/files/monitoring/{rules,grafana}/` and
  `infra/test/app-metrics-catalogue/`.

The review argued for a rule, "never later". So this is a commit rule, not a land-time warning, because a warning is
read once and forgotten. It runs inside the existing `driftguards` check. It uses git only, makes no network call, and
takes seconds. A new file that gates or holds belongs in `SERVING_FILES`, added in the commit that creates it.

## 2. `scripts/deploy-watch`: the roll, then the checks

```
scripts/deploy-watch --image <tag> [--checks identity-gate|FILE] [--countries pl,de,uk,us,es]
                     [--every 5m] [--window 2h] [--poll 1m] [--rollout-timeout 90m]
```

The agent that landed a serving change starts the watch in the background (`run_in_background`) once Main publishes
the image, and acts on its verdict. It runs in two phases:

1. It waits until kube-state-metrics shows every worker pod of each country on the tag.
2. It reads each check every `--every` interval until `--window` is up. A failing reading prints `RED <check>: ...`
   on stderr as soon as it is read. At the end it prints one `PASS|FAIL` line per check and a
   `DEPLOY-WATCH: PASS|FAIL` verdict line.

Every query goes to Prometheus over `ssh monitoring-1 curl`, bounded at 120 s. The `identity-gate` checks file is
`scripts/deploy-watch-checks/identity-gate.tsv`. It checks:

- held listings, at most 1,000;
- the oldest held listing, at most 15 minutes;
- a listing held over 10 minutes while nothing was promoted for 30 minutes;
- take flips and take reversals per hour, against twice the same hour yesterday.

For a different change, write a checks TSV with one series per `country`. The format is in the tool's `--help`.

## 3. One country first: the canary (proposal; the gitops part is the owner's to apply)

**How a worker image rolls today.** Main's `publish-worker` job tags the image `main-<utc14>-<sha7>`. Flux's
ImagePolicy `movies-worker` in movies-gitops `image-automation/automation.yaml` picks the newest tag. The
ImageUpdateAutomation then rewrites the one marked line, `worker/base/all.yaml`'s
`# {"$imagepolicy": "flux-system:movies-worker"}`. All five `worker-<cc>` overlays share that line, and the rollout is
staggered in waves (`flux/gotk-sync.yaml`).

**The smallest canary.** It needs no feature flag, no app change and no change to Main. Spain gets a second scan of
the same registry repository with a policy of its own, `movies-worker-canary`, and its overlay follows that policy
through a kustomize `images:` line marked `flux-system:movies-worker-canary:tag`:

- **An ordinary deploy is unchanged.** Both scans see the new tag within 5 minutes and all five workers roll as today.
- **A canary deploy holds the fleet's scan before the push lands.** The fleet's policy keeps its last tag, so only
  Spain's line moves.

Spain goes first because it is the smallest country, and its fix-1 baseline was 2 reversals a week.

**Preview.** The diff (39 lines) is `/Users/pawel/Desktop/claude docs/gitops-worker-canary-2026-11-06.patch`. With the
diff applied, `kubectl kustomize worker/overlays/es` builds, and this repo's FluxImageAutomationSpec,
K8sTierPathGatingSpec, Node{Cpu,Memory}BudgetSpec and WorkerRolloutStaggerSpec pass against the patched manifests.
To apply it (the owner):

```
cd ~/projects/movies-gitops && git pull --ff-only && git apply "$HOME/Desktop/claude docs/gitops-worker-canary-2026-11-06.patch" \
  && git commit -am "Worker canary: Spain follows its own image scan, so holding the fleet's rolls Spain alone" && git push
```

**A canary deploy, step by step** (the flux commands change the live fleet, so the owner runs them):

1. Before landing: `flux suspend image repository movies-worker`.
2. Land the change. Once Main publishes, Spain rolls:
   `scripts/deploy-watch --image <tag> --countries es --window 1h`.
3. If it passes: `flux resume image repository movies-worker`. The other four roll; then run
   `scripts/deploy-watch --image <tag> --window 2h`.
4. If it fails: leave the fleet suspended and revert or fix forward. Only Spain served the regression.

**Follow-up after the patch is applied.** Add a FluxImageAutomationSpec case holding Spain's line to the canary
policy, and every other worker to the base line.
