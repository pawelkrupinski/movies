# Alerting rules: how they are checked

Three layers, each asking a question the one before cannot:

| Layer | Asks | Runs |
|---|---|---|
| `infra/test/test_alert_rules.sh` (promtool) | Does each rule fire on the series its author wrote, and stay quiet on the healthy one? | `infra/bin/check`, CI |
| `infra/test/test_alert_rule_coverage.py`, `test_label_shapes.py` | Does every alert have both cases? Do its label matchers name values the fleet really emits? | `infra/bin/check`, CI |
| `infra/bin/backtest-alerts` | What would each rule, **as written in this tree**, have done over the last 14 days of **real** Prometheus data? | by hand, weekly, and before merging a rule change |

## Why the backtest exists

Every rule below passed its promtool cases and still meant the wrong thing on real data:
`MongodNoPrimary` paged continuously against a healthy replica set; `FluxReconciliationStopped`
fired 46% of the time on ImagePolicy objects that reconcile only when an image is built; three Flux
rules selected a gauge this fleet has never published and could never fire. A synthetic series
only contains what its author thought of.

## Running it

```sh
ssh -f -N kinowo-fleet                          # SOCKS tunnel to 10.20.0.0/24 on 127.0.0.1:1080
infra/bin/backtest-alerts                       # all alerts, writes infra/test/alert-backtest.json
infra/bin/backtest-alerts --alert HostOomKilled # a subset; does not touch the baseline
INFRA_CHECK_BACKTEST=1 infra/bin/check          # as the optional last step of the infra gate
```

A full run takes about ten minutes. It is **not** a CI job: GitHub's runners cannot reach the
private fleet network, and opening Prometheus to them is not worth a weekly report. Run it weekly
by hand and commit the refreshed `infra/test/alert-backtest.json`; its diff is how a rule's
behaviour on real data moved.

It covers every `alert:` in `rules/*.rules` and every Grafana-managed rule in
`grafana/alerting/alert-rules.yaml` (translated to the equivalent `(query) <op> <threshold>`), so a
new rule file is picked up with no change to the script.

## What it does

For each alert it evaluates `expr` with `query_range` at 60s steps, simulates the `for:` hold per
output series (a gap resets it, as it does in the rule evaluator), and reports the fraction of the
window any series was firing, the number of distinct episodes, the peak number of firing series,
and -- for Prometheus rules -- the fraction Prometheus itself had it firing (`ALERTS`), which
differs wherever the deployed rule differs from this tree's.

A metric **rolled out** inside the window (absent, then present for the rest of it) clips that
alert's window to the rollout, so an `absent`-style rule is not blamed for the days before its
metric existed.

## When it fails, and what to do

| Failure | Meaning | Fix |
|---|---|---|
| noisy | Fired more than 5% of the window, or more than 7 episodes a week, outside catalogued incidents | Fix the rule with a promtool case; or, if the firing was a real incident, catalogue it (below); or allowlist it in `infra/test/alert-backtest-allowlist.yml` with a reason |
| dead | Returned nothing for the whole window **and** reads a metric with no series in it | Point it at a metric that exists; or, if the metric is published only in an exceptional state, list it under `conditional_metrics` with a reason |
| incident | An alert in `infra/test/alert-incidents.yml` did not fire in its incident window, or one expected quiet fired | The rule no longer catches (or newly misfires on) a real past event |
| error | The query failed | Usually a timeout on a heavy expression; rerun once |

An alert Prometheus has not loaded yet (a rule on a branch) whose metrics do not exist yet is
reported `not-deployed`, not dead.

## The incident catalogue

`infra/test/alert-incidents.yml` lists real incidents since retention began (~2026-08-29) with a
UTC window, the alerts that must fire in it, and the alerts that must stay quiet. Firing inside an
`expect_firing` window does not count against an alert's noise budget. Add an incident when one
happens -- it is the only regression test a rule has against the event it was written for.
