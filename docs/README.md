# Documentation index

Every document in the repository, one line each. Working conventions are in [`CLAUDE.md`](../CLAUDE.md); task
mechanics in the skills under [`.claude/skills/`](../.claude/skills) (agent-ops, test-layers, regenerate-snapshots,
identity-investigation, record-client-fixtures, writing-fakes, external-api-rate-limits).

## Runbooks

- [adding-a-country.md](adding-a-country.md) — bringing up a new country end to end (model, roster harvest, worker,
  web, localization, deep links, observability, ship); appendix: restoring a stopped country.
- [domain-cutover.md](domain-cutover.md) — the 2026-08-29 move to `kinowo.net` / `showtimes.cc` and k3s: the host
  dimension and its ordering (mostly a historical record).
- [credential-rotation.md](credential-rotation.md) — what breaks when each credential is revoked, which alert says so,
  how to put a new one in place.
- [fleet-database-migration.md](fleet-database-migration.md) — moving the fleet's shared collections out of Poland's
  `kinowo` into `kinowo_fleet`: what moves, what was run (steps 0–6, 2026-10-25), the soak, the default flip and
  `scripts/fleet-db-finish` for the drop.
- [prod-ops-scripts.md](prod-ops-scripts.md) — writing a prod-ops script: the `scripts/prod-ops.sh` harness, `--check` /
  `--apply`, refusals, the shared fake-mongosh test. A runbook is a script, never a doc snippet.
- [heap-dumps.md](heap-dumps.md) — where JVM heap dumps land, what bounds them, how to fetch one.
- [headlamp-identity-and-gate.md](headlamp-identity-and-gate.md) — decision record (2026-09-07): Headlamp sign-in
  through the fleet's oauth2-proxy and where the gate actually is.
- [landing-process.md](landing-process.md) — commit to main: `agentctl land`, the merge queue, every pre-push hook
  step, `identity-landing-check`, the red gate, each refusal and what to do, the rules.
- [agent-workflow-history.md](agent-workflow-history.md) — the dated incidents behind the agent rules in `CLAUDE.md`
  (stranded WIP, PR breaches, pkill, CI waits, push batching, the red gate, no reruns).

## Design and rules

- [read-model-convergence.md](read-model-convergence.md) — "the prune is a no-op": every update path must leave the
  read model as a settle would.
- [design/identity-resolver.md](design/identity-resolver.md) — the identity resolver: §1–18 historical design and
  rollout record; §14, §19–21 current reference.
- [design/identity-resolver-rules-inventory.md](design/identity-resolver-rules-inventory.md) — HISTORICAL rules
  inventory at `984f4857b` (2026-10-06), before consolidation.
- [design/identity-slice.md](design/identity-slice.md) — `scripts/identity-slice`: one listing's identity reproduced
  in seconds; design and limits.
- [design/identity-cluster-identity.md](design/identity-cluster-identity.md) — a stable cluster identity for the
  agreement stage's verdicts, waits and staging rows: design and the owner's decisions (implemented 2026-10-25; keys name only since identity-no-drift.md).
- [design/identity-no-drift.md](design/identity-no-drift.md) — no drift by construction: the owner's principle (every
  change re-evaluates everything as if all data had been there from the start), the audit of every carried state, the
  equivalence harness, and what still blocks retiring the drift checks.
- [design/convergence-fixture-fill.md](design/convergence-fixture-fill.md) — how a hermetic convergence leg's
  missing fixtures get recorded, and what that does to a verdict.
- [design/identity-known-wrongs-and-todos.md](design/identity-known-wrongs-and-todos.md) — SUPERSEDED snapshot of
  2026-10-15; current: `identity-judgements.tsv`, `scripts/label-lookup`.
- `design/identity-seeding/seeding-<cc>.txt` — per-country seeding output kept with the design record.
- [identity-experiments.md](identity-experiments.md) — resolver (and resolver-performance) experiments tried,
  measured and not landed, with their `experiment/<name>` tags; read before a resolver change.

## Investigation logs (append-only)

- [white-cinema-investigations.md](white-cinema-investigations.md) — per-run log of venues whose scrapes parse empty
  (white) or fail (red), with each venue's verdict; summary and run index at the top.
- [misresolution-sweep.md](misresolution-sweep.md) — log of the sweep for films resolved to the wrong TMDB record
  (from the old pipeline era; its method predates the identity cutover of 2026-10-03/04).
- [investigations/2026-10-20-de-corpus-decline-oct12-19.md](investigations/2026-10-20-de-corpus-decline-oct12-19.md) — why
  Germany's corpus and TMDB-id counts fell 10-12 → 10-19: post-expansion identity settling, the weekly listing
  horizon, and the 10-14/15 identity rule tightening; cinemas and upstreams ruled out.
- [proposals/2026-10-23-dependency-and-flag-audit.md](proposals/2026-10-23-dependency-and-flag-audit.md) — what
  our libraries, SDKs, servers and tools offer that we don't use yet (APIs, JVM/compiler/mongod/k3s parameters),
  ranked, with file:line and verified/unverified marks; MongoDB is already on 8.3.11 (FCV unconfirmed).
- [proposals/2026-10-21-matching-fundamentals.md](proposals/2026-10-21-matching-fundamentals.md) — where the
  identity error budget is (recall, mostly relays), what was measured and rejected (title-word event gate, hard
  runtime/date constraints), and the five approved changes; scripts in `scripts/experiments/matching-fundamentals/`.
- [investigations/2026-10-22-rule-consolidation.md](investigations/2026-10-22-rule-consolidation.md) — the title
  rules moved into one table (`title-rules.tsv`): the inventory by effect, prod firings and BillingParse overlap, the
  byte-identity checks, which row merges fold alike (2) and which change stacked titles (23, for review), never-fired rows.
- [investigations/2026-10-26-us-chunk-scrape-hole.md](investigations/2026-10-26-us-chunk-scrape-hole.md) — OPEN:
  US chunked scrapes go idle ~3 h every ~3 days (all venues fresh, no due work); the hypothesis that a phase replan
  shifts a block of phases under nearest-boundary counting, how to prove it, the damping options, and the missing
  due-set gauge.
- [investigations/2026-10-22-footprint-measure.md](investigations/2026-10-22-footprint-measure.md) — the footprint
  measure (`identity-measure --footprint`): why a title-rule change can be proved to move no take by replaying the base's
  title readings, the ideas rejected as unsound, and its validation against eight full measures.
- [synopsis-resolution/investigation.md](synopsis-resolution/investigation.md) — June 2026 investigation of
  synopsis-based resolution; frozen inputs in [synopsis-resolution/archive/](synopsis-resolution/archive/README.md).
- `venue-maps/*.tsv` — venue-map data read by `data/us/scripts/generate_amc_map.py` and the UK clients (data, not prose).

## Product

- [promotion.md](promotion.md) — promotion and growth (SEO) playbook for kinowo.net and the apps.

## Module READMEs

- [ios/README.md](../ios/README.md) — the SwiftUI iOS client.
- [android/PLAY_PUBLISHING.md](../android/PLAY_PUBLISHING.md) — publishing the Android app to Google Play.
- [tools/devpanel/README.md](../tools/devpanel/README.md) — the macOS developer palette.
- [scripts/local-mirror/README.md](../scripts/local-mirror/README.md) — the local prod mirror (`:28017`): seeding,
  tailing, freshness, the prod tunnel.
- [scripts/discover-cinemas/README.md](../scripts/discover-cinemas/README.md) — the UK cinema discovery job.
- Roster data pipelines: [data/pl](../data/pl/README.md), [data/uk](../data/uk/README.md),
  [data/germany](../data/germany/README.md), [data/us](../data/us/README.md), [data/spain](../data/spain/README.md).
- [infra/nix/files/monitoring/README.md](../infra/nix/files/monitoring/README.md) — Prometheus + Grafana
  (<https://grafana.kinowo.net>; dashboards in `infra/nix/files/monitoring/grafana/dashboards/apps/`).
- [infra/version-dashboard/README.md](../infra/version-dashboard/README.md) — the local fleet dashboard (NixOS host state, unshipped mobile changes) and the mobile release lane.
- [web/src/main/resources/wellknown/README.md](../web/src/main/resources/wellknown/README.md) — the `/.well-known`
  files served for app links.
