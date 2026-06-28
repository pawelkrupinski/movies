#!/bin/sh
# Entrypoint for the kinowo-grafana machine. Starts the VictoriaMetrics sidecar
# (a single static binary) in the background, then hands PID 1 to Grafana's own
# entrypoint via exec so signals/shutdown still work.
#
# Why the sidecar: the app-emitted kinowo_* / kinowo_worker_* series otherwise
# ride Fly's managed-Prometheus ingestion lag (15-25 min, occasionally stalled).
# VM scrapes those endpoints DIRECTLY over Fly 6PN every 15s and serves a
# Prometheus-compatible query API on :8428, wired as the `app-metrics-live`
# Grafana datasource. Host metrics (fly_instance_*) still come from Fly — only
# its agent produces them — so this does not replace the `fly-prometheus`
# datasource, it sits alongside it.
set -eu

# Lives on the grafana volume so the recent window survives a redeploy. Short
# retention: this is a real-time cache, not the system of record (Fly's
# Prometheus keeps the long history).
mkdir -p /var/lib/grafana/vmdata

/usr/local/bin/victoria-metrics-prod \
  -promscrape.config=/etc/victoria/scrape.yml \
  -storageDataPath=/var/lib/grafana/vmdata \
  -retentionPeriod=7d \
  -httpListenAddr=:8428 \
  -memory.allowedBytes=128MB \
  -loggerLevel=WARN &

exec /run.sh
