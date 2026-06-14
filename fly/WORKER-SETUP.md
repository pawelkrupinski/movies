# One-time setup for the `kinowo-worker` Fly app

The read/write split deploys two Fly apps from one Dockerfile (see
`.github/workflows/deploy.yml`): `kinowo` (serving) and `kinowo-worker`
(scrape/enrich). CI already deploys both on every push to `main`, **but the
worker app and its secrets must be created once, manually, before that first
deploy** — `flyctl deploy` does not create an app or copy secrets.

Run these from the repo root with a Fly token that can create apps:

```sh
# 1. Create the app (no public IP — the worker serves no user traffic; the
#    only inbound HTTP is the private health check in fly.worker.toml).
flyctl apps create kinowo-worker --org <your-org>

# 2. Secrets. The worker is the WRITER + enricher, so it needs Mongo write
#    access + the scrape/enrich API keys. It does NOT need APPLICATION_SECRET
#    or the OAuth secrets (those are serving-only — they stay on `kinowo`).
#    Pull the values from `kinowo`'s existing secrets / `.env.local`:
flyctl secrets set -a kinowo-worker \
  MONGODB_URI="…"   \
  MONGODB_DB="…"    \
  TMDB_API_KEY="…"  \
  ZYTE_API_KEY="…"  \
  SENTRY_DSN="…"

# 3. (Optional) confirm the machine size matches fly.worker.toml (1 vCPU/1 GB).
#    `flyctl deploy` applies the toml, so this is just a sanity check.
flyctl status -a kinowo-worker
```

After this, the normal CI pipeline (`flyctl deploy -c fly.worker.toml
--build-arg BIN=worker`) takes over on every `main` push.

