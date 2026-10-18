#!/usr/bin/env bash
# Local dev server, run under a DELIBERATELY SMALLER heap than the rest of sbt.
#
# WHY. On 2026-09-04 a plain `sbt web/run` died with `OutOfMemoryError: Java heap
# space` and left nothing behind but that one line. The investigation ruled out
# four suspects by hand — compile pressure (a 386-source full compile peaks at
# 583MB; the worst incremental at 1577MB), change-stream flood (prod was doing
# ~22 updates/s at the crash second), resume-token replay, and reload
# accumulation (10 app boots in one JVM held flat) — and still could not name the
# retainer, because a dead JVM with no dump tells you nothing.
#
# The dev server's own envelope is well understood: ~30 measured runs put boot at
# 504-831MB, /debug across all five country mirrors at 811-1111MB, and the worst
# case of all (incremental compile of 387 files plus two app boots) at 1577MB.
# Nothing legitimate approaches 2.5GB.
#
# So this is a CANARY, not a limit. At the shared 4g of `.jvmopts` the anomaly has
# to reach 4GB before the JVM says anything; at 2.5GB it trips ~1.6x sooner while
# still leaving ~60% headroom over the worst measured run, and `.jvmopts`'s
# `-XX:+HeapDumpOnOutOfMemoryError` writes `target/oom-<pid>.hprof` naming the
# retainer outright.
#
# WHY NOT JUST LOWER `.jvmopts`. Because `testUnit` runs every module's specs
# UNFORKED in that same JVM and was MEASURED at a 4092MB peak against a 4096MB
# ceiling — four megabytes of headroom. Lowering the shared file would not shrink
# a canary, it would break the whole unit-test layer. `-J` is appended after
# `.jvmopts`, and the JVM honours the last `-Xmx`, so this narrows the heap for
# THIS process only (verified: effective MaxHeapSize 2504MB).
#
# Raise it back by passing your own, e.g. `scripts/dev-server.sh -J-Xmx4g`.
#
# MONGO. The local, password-free Mongo (tool-conventions.sh's local_mongo_port: :28017, or $LOCAL_MIRROR_PORT) —
# the same one `sbt localStack` and scripts/local-mirror/start-local-mongo.sh use:
# the app reads its playground `kinowo_local`, /debug reads the prod-synced `kinowo_prod_mirror`
# (scripts/local-mirror/README.md). The app reads config from the process only (no .env.local
# since 2026-10-12), so without these it refused to boot. Anything already exported wins; for
# prod itself, `scripts/with-prod-mongo.sh scripts/dev-server.sh` (the vault + the ssh tunnel).
# The Mongo is started when nothing answers on that port (DEV_SERVER_MONGO_UP=1 skips the check).
#
# TMDB. The /debug/review cards ask TMDB live for a poster no stored read has; without TMDB_API_KEY
# every one is "no TMDB poster". So it comes from the vault (one Touch ID per launch) unless already
# exported; with no `secrets` CLI the server still boots, without those posters.
set -euo pipefail
. "$(dirname "$0")/vault-env.sh"
# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "$0")/tool-conventions.sh" # local_mongo_port / local_mongo_uri
ensure_vault_env TMDB_API_KEY -- "$BASH" "$0" "$@" || echo "[dev-server] booting without TMDB_API_KEY: no live TMDB posters on /debug/review" >&2
cd "$(dirname "$0")/.."
export MONGODB_URI="${MONGODB_URI:-$(local_mongo_uri)}"
export MONGODB_DB="${MONGODB_DB:-kinowo_local}"
export MONGODB_MOVIES_MIRROR_URI="${MONGODB_MOVIES_MIRROR_URI:-$(local_mongo_uri kinowo_prod_mirror)}"
if [ -z "${DEV_SERVER_MONGO_UP:-}" ] && ! (echo >"/dev/tcp/127.0.0.1/$(local_mongo_port)") 2>/dev/null; then
  scripts/local-mirror/start-local-mongo.sh >&2
fi
exec sbt -J-Xmx2500m "$@" web/run
