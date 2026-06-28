# kinowo alerting — self-hosted Grafana on Fly

Host/runtime monitoring + alerting for `kinowo`, `kinowo-worker`, and
`kinowo-mongo`, complementing the in-app `/uptime` page (which watches the
external cinema scrape targets, not our own VMs).

## Why this exists / why not X

- **Why not just fly-metrics.net?** Fly's hosted Grafana shows the metrics but
  **cannot run alert rules** (confirmed, even on paid plans). Fly's own
  recommendation for alerting is a self-hosted Grafana in your org — that's this.
- **Why not Datadog?** Per-host pricing (~$15/host/mo × 3 apps) for a hobby app
  buys APM/tracing we don't need. Fly already emits the host metrics free.
- **Why not Grafana Cloud?** Viable, but keeps config in their UI and depends on
  an external account + series caps. Self-hosting keeps everything as code here.

Everything is **config-as-code** under `provisioning/` — a fresh machine comes up
fully wired (datasource, 13 alert rules, Telegram contact point, overview
dashboard). No click-ops.

Most rules read Fly's host metrics. The **app-uptime** group is different: it
reads the in-app `/uptime` health (the Mongo `uptimeBuckets` data the public
page shows), which Fly's host metrics can't see. The `kinowo` web app exposes it
as Prometheus gauges (`kinowo_uptime_recent_*`) from its `MetricsController`
(`GET /metrics`).

There are **two datasources**:

- `fly-prometheus` — Fly's managed Prometheus. The ONLY source of host metrics
  (`fly_instance_*` / `fly_edge_*` — CPU credit/throttle, memory, 5xx, p95);
  Fly's host agent produces those and we can't scrape them ourselves. Its
  downside: ingestion runs **15-25 min behind real time and occasionally
  stalls** (a Fly-side limitation), so any `to=now` panel has a blank tail.
- `app-metrics-live` — a **VictoriaMetrics sidecar** (baked into this machine,
  see `Dockerfile` + `start.sh` + `victoria/scrape.yml`) that scrapes the apps'
  `/metrics` endpoints **directly over Fly 6PN every 15s**. The app-emitted
  `kinowo_*` / `kinowo_worker_*` series are real-time here instead of riding
  Fly's lag. The overview dashboard's app panels query this; host panels stay
  on `fly-prometheus`. (`<app>.internal` is IPv6-only, hence `-enableTCP6`.)

Alert rules still read `fly-prometheus` — a 15-25 min-late page beats a flaky
one, and the host-metric rules have no alternative anyway.

## What it watches (each rule maps to a real past incident)

| Rule | Fires when | Incident it would have caught |
|------|-----------|-------------------------------|
| CPU credit exhausted (serving app) | `fly_instance_cpu_balance < 500` (web/mongo) 10m | 2026-06-07 web 93% steal |
| CPU throttled (serving app) | `rate(fly_instance_cpu_throttle) > 0.2` 10m | 2026-06-07 web 93% steal |
| CPU steal high | steal share `> 25%` 10m | host contention |
| Worker CPU starved (sustained throttle) | worker throttled long-term | continuous-scrape credit drain |
| Memory pressure (pre-OOM) | `mem_available/total < 8%` 15m | 2026-06-06 Mongo OOM cascade (pre-OOM) |
| Disk space low (persistent volume) | volume `> 80%` full | 2026-06-10 Mongo disk-full crash |
| Disk projected to fill within 7 days | linear fill-rate projection | early disk warning |
| Memory pressure building (PSI) | PSI stall rising | early OOM runway |
| Mongo down | Mongo instance absent 3m | Mongo crash cascading to web outage |
| HTTP 5xx rate high | `> 5%` 5xx 5m | 502/503 boot-crash-loop outage |
| HTTP p95 latency high | p95 `> 2s` 10m | "percentiles suddenly increasing" |
| Serving app down | `fly_instance_up{app=kinowo}` 0/absent 3m | web outage |
| Residential proxy failing | recent failure ratio `> 0.5` 15m (app-uptime) | 2026-06-16 Decodo "Limit: 3" rolled all scrapes to Zyte for hours, unnoticed |

Notes grounded in the live metrics (verified against `api.fly.io/prometheus`):
- `fly_instance_exit_oom` from the docs **does not exist** in this org — the OOM
  *kill* is covered by Fly's free OOM email; the memory-pressure rule catches the
  *runway* to it.
- `kinowo-worker` runs credit-starved (balance ≈ 1) and CPU-throttled **by
  design** (continuous scrape), so the CPU-credit/throttle rules scope to
  `kinowo`/`kinowo-mongo` to avoid permanent false alarms.
- Latency/5xx rules use `noDataState: OK` — a quiet hobby app legitimately has no
  HTTP series for minutes. Only the down-detector treats missing data as the alarm.

## One-time setup

### 1. Telegram bot
All alerts go to the **"Kinowo Monitoring"** Telegram group
(chat id `-1003950886618`) — the same channel the worker posts its app alerts to.
Use the bot that is already a member of that group (its token is the
`TELEGRAM_BOT_TOKEN` secret); Grafana posts to the group's General topic.
To use a different bot, add it to the group first, then set its token as the
secret. (Look up a chat id by messaging the bot and reading `message.chat.id`
from `https://api.telegram.org/bot<TOKEN>/getUpdates`.)

### 2. Fly read token for the Prometheus datasource
```sh
fly tokens create readonly --org personal --name grafana-prometheus-read --expiry 8760h
```
Copy the whole `FlyV1 …` value **without** the `FlyV1 ` prefix (the datasource
config already prepends it — note: the header is `FlyV1 <token>`, **not**
`Bearer`).

### 3. Telegram chat id (not a secret)
`provisioning/alerting/contact-points.yaml` already pins `chatid:
"-1003950886618"` (the "Kinowo Monitoring" group). It lives in the file (not
`fly secrets`) on purpose: a chat id is useless without the bot token, and
Grafana's `${VAR}` provisioning expansion mis-types a numeric chatid as a
number, which fails — a quoted literal is the reliable form. **Keep the quotes**
if you ever change it.

### 4. Create the app, volume, and secrets
```sh
fly apps create kinowo-grafana --org personal
fly volumes create grafana_data --app kinowo-grafana --region arn --size 1
fly secrets set --app kinowo-grafana \
  GF_SECURITY_ADMIN_PASSWORD='<pick-a-strong-password>' \
  FLY_PROMETHEUS_TOKEN='<token-from-step-2-without-FlyV1-prefix>' \
  TELEGRAM_BOT_TOKEN='<bot-token>'
```

### 5. Deploy
```sh
fly deploy --remote-only -c fly/grafana/fly.toml -a kinowo-grafana
```
Or push any change under `fly/grafana/**` to `main` — `.github/workflows/deploy-grafana.yml`
deploys it. Log in at `https://kinowo-grafana.fly.dev` (admin + the password above).
Confirm under **Alerting → Alert rules** that all 13 rules are `Normal`, and open each
rule's query in **Explore** to confirm it returns data.

### 6. Deploy markers (commit hash on the timeline)
`.github/workflows/deploy.yml` posts a Grafana annotation with the commit
SHA + subject after every app deploy. Give it a path:
1. In Grafana: **Administration → Service accounts → Add** (`ci-annotations`,
   role *Editor*) → **Add token** → copy it.
2. Add two GitHub Actions secrets:
   - `GRAFANA_URL` = `https://kinowo-grafana.fly.dev`
   - `GRAFANA_ANNOTATION_TOKEN` = the service-account token
The markers render as vertical purple lines on the **kinowo — Fly health**
dashboard (the `Deploys` annotation query). Until the secrets exist, the
`annotate` job no-ops — deploys are never blocked.

## Local validation

```sh
fly/grafana/smoke-test.sh      # needs Docker
```
Boots the real Grafana image with this provisioning and asserts the datasource,
all 13 alert rules, the Telegram contact point and the dashboard load cleanly. No
network to Fly needed (dummy token). Break any provisioning file and it fails —
this is the test gate for changes here.

## Tuning

Thresholds live in `provisioning/alerting/alert-rules.yaml`. After a week of real
data, adjust any rule that's too quiet/noisy (likely the memory 8% floor on the
worker, which already runs hot). Re-run the smoke test, push, done.
