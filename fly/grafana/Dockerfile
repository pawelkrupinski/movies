# Self-hosted Grafana for kinowo alerting. Everything is config-as-code under
# provisioning/ — datasource, alert rules, contact point, notification policy
# and the overview dashboard are baked in, so a fresh machine comes up fully
# wired with no click-ops. Secrets (Fly read token, Telegram creds, admin
# password) are injected as env via `fly secrets`, referenced as ${VAR} in the
# provisioning YAML.
FROM grafana/grafana:11.4.0

COPY provisioning /etc/grafana/provisioning
