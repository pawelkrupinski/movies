# Self-hosted Mongo for the `kinowo` app. Plain official mongo image, no
# customisation — auth is enabled via the MONGO_INITDB_ROOT_* env vars wired
# in by `flyctl secrets`, and the only persistent state lives on the mounted
# volume at `/data/db` (configured in fly.toml).
FROM mongo:7.0
