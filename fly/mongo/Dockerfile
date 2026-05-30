# Self-hosted Mongo for the `kinowo` app. Plain official mongo image, no
# customisation — auth is enabled via the MONGO_INITDB_ROOT_* env vars wired
# in by `flyctl secrets`, and the only persistent state lives on the mounted
# volume at `/data/db` (configured in fly.toml).
FROM mongo:7.0

# Fly's `.internal` network is 6PN (IPv6 only). Mongo 7 binds IPv4 by
# default; `--ipv6` (combined with the official entrypoint's
# `--bind_ip_all`) makes it accept IPv6 too, so cross-app traffic from
# kinowo lands here.
CMD ["mongod", "--ipv6", "--bind_ip_all"]

