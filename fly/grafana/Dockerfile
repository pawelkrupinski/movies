# Self-hosted Grafana for kinowo alerting. Everything is config-as-code under
# provisioning/ — datasources, alert rules, contact point, notification policy
# and the overview dashboard are baked in, so a fresh machine comes up fully
# wired with no click-ops. Secrets (Fly read token, Telegram creds, admin
# password) are injected as env via `fly secrets`, referenced as ${VAR} in the
# provisioning YAML.
#
# A VictoriaMetrics single-binary sidecar (copied from the official VM image)
# scrapes the apps' /metrics endpoints DIRECTLY over Fly 6PN every 15s and is
# wired as the `app-metrics-live` datasource — so the app-emitted kinowo_* /
# kinowo_worker_* series are real-time instead of riding Fly's managed-Prometheus
# ingestion lag (15-25 min, occasionally stalled). See start.sh + victoria/
# scrape.yml. Host metrics (fly_instance_*) still come from the `fly-prometheus`
# datasource — only Fly's host agent emits those.
FROM victoriametrics/victoria-metrics:v1.146.0 AS vm

FROM grafana/grafana:11.4.0

USER root
COPY --from=vm /victoria-metrics-prod /usr/local/bin/victoria-metrics-prod
COPY provisioning /etc/grafana/provisioning
COPY victoria/scrape.yml /etc/victoria/scrape.yml
COPY start.sh /usr/local/bin/kinowo-start.sh
RUN chmod +x /usr/local/bin/kinowo-start.sh /usr/local/bin/victoria-metrics-prod
USER grafana

ENTRYPOINT ["/usr/local/bin/kinowo-start.sh"]
