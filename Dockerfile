# Single-stage runtime image. The Play `universal/stage` distribution is
# produced by `sbt stage` in the GitHub Actions `test` job (see
# .github/workflows/main.yml) and downloaded into a top-level `stage/`
# directory before this image is built. `.dockerignore` whitelists exactly
# that directory, so Fly's remote builder receives just the staged JARs +
# startup scripts — no JDK, no sbt, no source.
#
# Eclipse Temurin (HotSpot) JRE 25 — the current LTS, and the highest
# Java version Play 3.0.x has shipped tested. Scala 3.8.3 emits Java 21
# bytecode (the highest output version it accepts); JRE 25 loads those
# class files unchanged. CI builds on the same JDK 25 — toolchain
# consistent end-to-end.
# One image, two apps. `BIN` selects which staged launcher the container
# runs: `web` (the Play serving app, Fly app `kinowo`) or `worker` (the
# scrape/enrich `def main` app, Fly app `kinowo-worker`). Each app's deploy
# downloads ITS OWN `web/target/universal/stage` or
# `worker/target/universal/stage` into the build context's `stage/`, so
# `COPY stage/` stays a single fixed path and only the launcher name differs.
# The Play `-D` props below are harmless no-op system properties for the
# worker (it isn't a Play app).
FROM eclipse-temurin:25-jre
ARG COMMIT_SHA=unknown
ENV COMMIT_SHA=$COMMIT_SHA
ARG BIN=web
ENV BIN=$BIN
# libvips for the WORKER only: it shrinks share-card posters of any size in a capped subprocess
# (services.sharecards.VipsPosterShrinker), where the JDK decoder must refuse anything over 12 MP.
# Measured +59 MB on the image; the web never decodes a poster, so it doesn't carry it.
RUN if [ "$BIN" = "worker" ]; then \
      apt-get update && apt-get install -y --no-install-recommends libvips-tools && rm -rf /var/lib/apt/lists/*; \
    fi
WORKDIR /app
COPY stage/ ./
# `actions/upload-artifact@v4` strips the Unix executable bit, so the
# Play startup scripts under `bin/` arrive as 0644 in the build context.
# Without this chmod the container exits with code 126 ("command not
# executable") on every machine start, which Fly retries until
# max-restart-count and then leaves the machine stopped under a deploy
# lease — the symptom that took down prod the first time this pipeline
# ran. The fix is idempotent: a future build path that *does* preserve
# the bit (tar artefact, direct `docker build`, etc.) won't be harmed
# by re-applying 0755.
RUN chmod +x bin/*
EXPOSE 9000
# HEAP DUMPS: bounded, uniquely named, and on a volume that outlives the pod.
#
# /data/heapdumps is a hostPath on k3s-worker-1 (/var/lib/kinowo/heapdumps/<app>-<country>/,
# mounted through a per-app-country subPath in the GitOps manifests), so a dump survives the pod
# being replaced -- web-pl's heap OOM on 2026-09-23 was lost with its emptyDir. Before the JVM
# starts, `bin/heap-dumps.sh` (infra/nix/files/heap-dumps.sh, copied in by build.sbt) does two
# things, and the node's `kinowo-heap-dumps` timer repeats the budget independently of restarts:
#
#   prune      keep the 3 newest dumps and at most 4 GiB in THIS app-country's directory, oldest
#              deleted first, never touching a file written in the last 10 minutes (it may be
#              another pod's dump in progress). It logs `heap-dumps: dir=... files=N bytes=B`.
#              A failure here must never block a boot, hence the `|| true`.
#   dump-file  names this start's -XX:HeapDumpPath FILE: <app>-<country>_<pod>_<start UTC>.hprof.
#              Appended to JAVA_OPTS, so it overrides the ConfigMap's directory-form flag (the
#              last -XX occurrence wins). A DIRECTORY there makes the JVM pick
#              `java_pid<pid>.hprof`, which in a container is always java_pid1.hprof -- and the JVM
#              refuses to overwrite it, so every OOM after the first wrote NOTHING (worker-us,
#              2026-09-03). The prune still renames a leftover java_pid1.hprof to its write time.
#
# The budget lives in the script rather than a ConfigMap on purpose: a ConfigMap value on this
# fleet reaches the cluster only when somebody applies it by hand, and the image DOES deploy.
# See docs/heap-dumps.md for fetching a dump.
#
# DURABLE STDERR: the JVM's dying stderr — the `ExitOnOutOfMemoryError`
# native-OOM line (`Native memory allocation (mmap/malloc) failed…`) and, on a clean
# SIGTERM restart, the `-XX:+PrintNMTStatistics` summary — otherwise goes only to the
# container stderr → `kubectl logs`, whose short retention rolls away before the ~5 h
# OOM can be read. Append it to /data/logs/worker-stderr.log so the pre-death readout
# SURVIVES the restart. `launch()` keeps the `exec` (JVM stays PID-adjacent, receives
# SIGTERM directly for the graceful NMT dump) while the redirect at the call site
# hands the JVM an fd-2 pointing at the durable file — on web too, whose /data is an
# emptyDir that outlives a container restart; with no /data at all the `else` branch runs
# the JVM unredirected. Cap the file on boot so a crash-loop can't fill /data (keep the
# last ~4 MB); web's emptyDir sizeLimit is derived from this cap (NodeMemoryBudgetSpec). Hard JVM crashes (SIGSEGV) go
# to -XX:ErrorFile=/data/logs/hs_err_%p.log (set in each k3s overlay's JAVA_OPTS).
CMD mkdir -p /data/heapdumps /data/logs 2>/dev/null; \
    bin/heap-dumps.sh prune /data/heapdumps || true; \
    if dump=$(bin/heap-dumps.sh dump-file /data/heapdumps); then export JAVA_OPTS="$JAVA_OPTS -XX:HeapDumpPath=$dump"; fi; \
    if [ -d /data/logs ]; then ls -1t /data/logs/hs_err_*.log 2>/dev/null | tail -n +4 | xargs -r rm -f; fi; \
    if [ -f /data/logs/worker-stderr.log ] && [ "$(wc -c < /data/logs/worker-stderr.log)" -gt 16777216 ]; then \
      tail -c 4194304 /data/logs/worker-stderr.log > /data/logs/worker-stderr.log.tmp && mv /data/logs/worker-stderr.log.tmp /data/logs/worker-stderr.log; fi; \
    rm -rf /data/jfr 2>/dev/null; \
    launch() { exec bin/$BIN \
    -Dplay.http.secret.key="${APPLICATION_SECRET}" \
    -Dplay.server.http.address=0.0.0.0 \
    -Dhttp.address=0.0.0.0 \
    -Dpidfile.path=/dev/null; }; \
    if [ -d /data ]; then mkdir -p /data/logs; launch 2>> /data/logs/worker-stderr.log; else launch; fi
    # JVM sizing (heap/GC/non-heap caps) is now per-app via `JAVA_OPTS` — the
    # launcher reads it — set in each tier+country's k3s overlay (and in `fly.toml`
    # for the retired `kinowo` redirect host), so every app can be sized
    # independently from this one shared image. web runs a smaller heap (it no
    # longer scrapes); the worker keeps the larger one. The historical rationale
    # for the original single sizing is preserved below for reference.
    #
    # JVM sizing on the 1 GB cgroup. Targets:
    #
    #   - Xms == Xmx == 384m: heap pre-allocated, no resize-up pauses
    #     (the 128→256 growth events on the previous config were
    #     consistent with the ~1.3 s TTFB spikes we measured from
    #     inside the container; a `dev/tcp` ping showed 4 of 5 reqs
    #     at 100-130 ms and 1 at 1.3 s).
    #
    #   - G1 with a 50 ms pause target: at this heap size G1 keeps
    #     mixed-collection pauses comfortably under the budget; the
    #     long-tail spikes were from the default 200 ms target
    #     combined with concurrent-cycle backups when Xms→Xmx
    #     resizing was active.
    #
    #   - UseStringDeduplication: the / page is a 2 MB HTML string
    #     built by 200 film cards × repeated attribute names. G1's
    #     dedup pass merges equal char[] arrays across the heap,
    #     measurably cutting young-gen pressure during render.
    #
    # GC logging was here as `-J-Xlog:gc*:stderr:…` while diagnosing
    # the heap-resize spikes; once the tuning above settled the
    # variance it's just noise in `kubectl logs`. Re-add as a one-liner
    # if a future perf investigation needs to correlate request
    # latency with pause records.
    #
    # Non-heap caps (Java 21 defaults are unbounded for metaspace and
    # Xmx-sized for direct memory) stay tight to leave headroom:
    #
    #   - MaxMetaspaceSize=160m: from 192 — the smaller heap reduces
    #     class-loader pressure, classes loaded peaks at ~110 MB.
    #   - MaxDirectMemorySize=96m: from 128 — Pekko + Mongo driver's
    #     direct buffers measured at ~60 MB peak.
    #   - ReservedCodeCacheSize=96m: unchanged. JIT-compiled methods
    #     for Play 3 + the enrichment cascade peak at ~75 MB.
    #
    # Total committed ceiling: 384 (heap) + 160 (meta) + 96 (code) +
    # 96 (direct) = 736 MB. Plus thread stacks (~60 MB) + Pekko +
    # native overhead (~120 MB) = ~916 MB. Fits in the 1 GB cgroup
    # with ~108 MB headroom.
