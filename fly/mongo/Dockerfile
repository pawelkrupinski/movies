# Self-hosted Mongo for the `kinowo` app. Plain official mongo image, no
# customisation — auth is enabled via the MONGO_INITDB_ROOT_* env vars wired
# in by `flyctl secrets`, and the only persistent state lives on the mounted
# volume at `/data/db` (configured in fly.toml).
FROM mongo:7.0

# Internal-auth keyfile. Mongo requires keyfile (or x509) internal auth as soon
# as authentication AND replication are both enabled — even for a SINGLE-member
# replica set, which has no peer to authenticate against. Generated at build
# time: for a one-node set there's no other member that must share it, so a
# fresh keyfile per build is fine. Must be mode 0400 and owned by the `mongodb`
# user the entrypoint runs mongod as.
RUN openssl rand -base64 756 > /etc/mongo-keyfile \
 && chmod 400 /etc/mongo-keyfile \
 && chown mongodb:mongodb /etc/mongo-keyfile

# Run as a single-node replica set (`rs0`) so MongoDB exposes an oplog and
# change streams — the kinowo app refreshes its cache incrementally off the
# change stream instead of re-reading the whole `movies` collection on a timer
# (a standalone mongod rejects `$changeStream` with error 40573).
#
# ONE-TIME init after the FIRST deploy with this CMD (or after recreating the
# data volume) — the set config then lives on the volume and survives redeploys.
# Tunnel in (`flyctl proxy 27017:27017 --app kinowo-mongo`) and, authenticated
# as the root user, run with the *app-reachable* member host:
#   rs.initiate({_id: "rs0",
#                members: [{_id: 0, host: "kinowo-mongo.internal:27017"}]})
#
# `--ipv6 --bind_ip_all` keeps it reachable over Fly's 6PN `.internal` network;
# `--keyFile` implies `--auth`.
#
# `--wiredTigerCacheSizeGB 0.25` pins the WiredTiger cache. WiredTiger auto-sizes
# its cache from the *host's* reported RAM, not the container's cgroup limit, so
# in a Fly VM it can wildly over-allocate; an unbounded cache + mongod baseline +
# live connections is what OOM-killed the 512mb box on 2026-06-06. Pinning it
# bounds steady-state RSS far under the (now 1gb) ceiling — the structural fix
# for the OOM. The dataset is tiny (hundreds of docs), so 256mb of cache is ample.
#
# `--setParameter diagnosticDataCollectionDirectorySizeMB=300` bounds FTDC (full-
# time diagnostic data capture) at 300mb. FTDC's inability to write its interim
# temp file on a FULL /data/db is what SIGABRT-crashed mongod on 2026-06-10 (the
# disk-full cascade). The default cap is 200mb; 300mb keeps a few extra days of
# high-res server diagnostics for post-incident analysis — trivial on the 3gb
# volume — while still being an explicit bound rather than relying on the default.
# (Also set live via setParameter on 2026-06-10; this line makes it survive a
# redeploy/restart.)
CMD ["mongod", "--ipv6", "--bind_ip_all", "--replSet", "rs0", "--keyFile", "/etc/mongo-keyfile", "--wiredTigerCacheSizeGB", "0.25", "--setParameter", "diagnosticDataCollectionDirectorySizeMB=300"]
