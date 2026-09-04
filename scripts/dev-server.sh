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
set -euo pipefail
cd "$(dirname "$0")/.."
exec sbt -J-Xmx2500m "$@" web/run
