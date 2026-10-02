# Single-stage runtime image. The Play `universal/stage` distribution is
# produced by `sbt stage` in the GitHub Actions `test` job (see
# .github/workflows/main.yml) and downloaded into a top-level `stage/`
# directory before this image is built. `.dockerignore` whitelists exactly
# that directory, so Fly's remote builder receives just the staged JARs +
# startup scripts — no JDK, no sbt, no source.
#
# Eclipse Temurin (HotSpot) JRE 27 — the current feature release (25 is the
# LTS; 27 is supported until JDK 28 ships in March 2027, which is the next
# move). Scala 3.9 emits Java 21 bytecode (the highest output version it
# accepts); JRE 27 loads those class files unchanged. CI builds on the same
# JDK 27 — toolchain consistent end-to-end.
# One image, two apps. `BIN` selects which staged launcher the container
# runs: `web` (the Play serving app, Fly app `kinowo`) or `worker` (the
# scrape/enrich `def main` app, Fly app `kinowo-worker`). Each app's deploy
# downloads ITS OWN `web/target/universal/stage` or
# `worker/target/universal/stage` into the build context's `stage/`, so
# `COPY stage/` stays a single fixed path and only the launcher name differs.
# The Play `-D` props below are harmless no-op system properties for the
# worker (it isn't a Play app).
FROM ubuntu:26.04
# THE TEMURIN JRE, INSTALLED HERE BECAUSE `eclipse-temurin:27-jre` IS NOT PUBLISHED YET. JDK 27
# went GA on 2026-09-15 and Adoptium ships its binaries, but Docker Hub's official image lags.
# This block is that image's own Dockerfile (adoptium/containers, ubuntu/noble, jre) reproduced on 26.04:
# the same OS packages, locale, JAVA_HOME and CDS archive, with the tarball's SHA-256 pinned here
# instead of a GPG check against a keyserver on every build. Once the official tag exists, replace
# everything down to the `java --version` line with `FROM eclipse-temurin:27-jre`.
# JdkVersionParitySpec holds JAVA_VERSION's major to the JDK CI builds the stage with.
# BUILD IT NATIVELY. A `--platform linux/amd64` build on Apple Silicon fails at the `tar` below with
# `Cannot open: Function not implemented` for every file in a subdirectory: 26.04's GNU tar makes a
# syscall neither QEMU nor Rosetta translates. A real amd64 kernel has it (checked on k3s-worker-1,
# 2026-09-27), and CI builds on one. Locally, build for the Mac's own arm64.
ENV JAVA_HOME=/opt/java/openjdk
ENV PATH=$JAVA_HOME/bin:$PATH
ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'
ENV JAVA_VERSION=jdk-27+35
RUN set -eux; \
    apt-get update; \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        fontconfig ca-certificates p11-kit tzdata locales wget; \
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen; \
    locale-gen en_US.UTF-8; \
    case "$(dpkg --print-architecture)" in \
      amd64) ESUM='2cb1b81ab49f516e5aeb28ee8acf3c73d64c77ca432ac959c6511a335342d8e9'; \
             BINARY_URL='https://github.com/adoptium/temurin27-binaries/releases/download/jdk-27%2B35/OpenJDK27U-jre_x64_linux_hotspot_27_35.tar.gz' ;; \
      arm64) ESUM='a41b54098373f1ca8f75ee344db19b93ac39eca6c529c3ee9dc50f4b3e7857de'; \
             BINARY_URL='https://github.com/adoptium/temurin27-binaries/releases/download/jdk-27%2B35/OpenJDK27U-jre_aarch64_linux_hotspot_27_35.tar.gz' ;; \
      *) echo "Unsupported arch: $(dpkg --print-architecture)"; exit 1 ;; \
    esac; \
    wget --progress=dot:giga -O /tmp/openjdk.tar.gz "$BINARY_URL"; \
    echo "$ESUM */tmp/openjdk.tar.gz" | sha256sum -c -; \
    mkdir -p "$JAVA_HOME"; \
    tar --extract --file /tmp/openjdk.tar.gz --directory "$JAVA_HOME" --strip-components 1 --no-same-owner; \
    rm -f /tmp/openjdk.tar.gz; \
    apt-get purge -y --auto-remove wget; \
    rm -rf /var/lib/apt/lists/*; \
    find "$JAVA_HOME/lib" -name '*.so' -exec dirname '{}' ';' | sort -u > /etc/ld.so.conf.d/docker-openjdk.conf; \
    ldconfig; \
    java -Xshare:dump; \
    java --version
ARG BIN=web
ENV BIN=$BIN
# libvips for the WORKER only: it shrinks share-card posters of any size in a capped subprocess
# (services.sharecards.VipsPosterShrinker), where the JDK decoder must refuse anything over 12 MP.
# Measured +59 MB on the image; the web never decodes a poster, so it doesn't carry it.
RUN if [ "$BIN" = "worker" ]; then \
      apt-get update && apt-get install -y --no-install-recommends libvips-tools && rm -rf /var/lib/apt/lists/*; \
    fi
# The commit only AFTER every layer that doesn't depend on it: an ENV changes the layer chain from
# that point on, so above the libvips install it rebuilt that apt layer on every commit.
ARG COMMIT_SHA=unknown
ENV COMMIT_SHA=$COMMIT_SHA
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
# AOT CACHE: classes served from /app/classes.aot live in that mapped file, not in metaspace
# (loading the worker's 31k classes: 157 MB metaspace + 37 MB class space without it, 0.6 MB with
# it; worker-pl died of `OutOfMemoryError: Metaspace` at its 128m cap before it). The training
# (tools.ClassArchiveTraining) runs through the app's own launcher so the classpath and the baked
# options (conf/application.ini: collector, JIT shape, JDK-25 parity) are the ones it starts with --
# a cache trained under others does not map. -Xmx512m keeps the training heap in the same
# compressed-pointer range as every pod's. `test -s` fails the build rather than ship without it.
#
# NOT FOR THE WORKER: CI trains the worker's cache on a replayed boot instead, which archives the
# same classes plus method profiles (scripts/ci/train-worker-aot.sh), and layers it on this image.
# Training this one too would ship two caches' worth of layers on every worker deploy.
RUN if [ "$BIN" = "worker" ]; then exit 0; fi; \
    JAVA_OPTS="-Xmx512m -XX:AOTCacheOutput=/app/classes.aot" bin/$BIN -main tools.ClassArchiveTraining /app/lib 2>&1 \
      | grep -v "Preload Warning" ; test -s /app/classes.aot
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
# OOM can be read. Append it to /data/logs/$BIN-stderr.log (web-stderr.log or
# worker-stderr.log, so neither app's readout is mistaken for the other's) so the pre-death readout
# SURVIVES the restart. `launch()` keeps the `exec` (JVM stays PID-adjacent, receives
# SIGTERM directly for the graceful NMT dump) while the redirect at the call site
# hands the JVM an fd-2 pointing at the durable file — on web too, whose /data is an
# emptyDir that outlives a container restart. The first line's `mkdir -p` creates /data
# wherever the root filesystem is writable, so the `else` branch (the JVM unredirected) is
# only for a read-only root with nothing mounted at /data. Cap the file on boot so a crash-loop can't fill /data (keep the
# last ~4 MB); web's emptyDir sizeLimit is derived from this cap (NodeMemoryBudgetSpec). Hard JVM crashes (SIGSEGV) go
# to -XX:ErrorFile=/data/logs/hs_err_%p.log (set in each k3s overlay's JAVA_OPTS).
#
# AOT CACHE: -XX:AOTCache=/app/classes.aot (trained above) is appended unless JAVA_OPTS already
# names a CDS archive, which the JVM refuses to start beside it ("cannot be used at the same time
# with ... SharedArchiveFile") -- an overlay still carrying one runs without the cache, not in a
# crash loop. AotCacheOptionsSpec keeps the overlays free of them.
CMD mkdir -p /data/heapdumps /data/logs 2>/dev/null; \
    bin/heap-dumps.sh prune /data/heapdumps || true; \
    if dump=$(bin/heap-dumps.sh dump-file /data/heapdumps); then export JAVA_OPTS="$JAVA_OPTS -XX:HeapDumpPath=$dump"; fi; \
    if [ -d /data/logs ]; then ls -1t /data/logs/hs_err_*.log 2>/dev/null | tail -n +4 | xargs -r rm -f; fi; \
    stderr_log=/data/logs/$BIN-stderr.log; \
    if [ -f "$stderr_log" ] && [ "$(wc -c < "$stderr_log")" -gt 16777216 ]; then \
      tail -c 4194304 "$stderr_log" > "$stderr_log.tmp" && mv "$stderr_log.tmp" "$stderr_log"; fi; \
    rm -rf /data/jfr 2>/dev/null; \
    case " $JAVA_OPTS " in *SharedArchiveFile*|*-Xshare*) ;; *) export JAVA_OPTS="$JAVA_OPTS -XX:AOTCache=/app/classes.aot";; esac; \
    launch() { exec bin/$BIN \
    -Dplay.http.secret.key="${APPLICATION_SECRET}" \
    -Dplay.server.http.address=0.0.0.0 \
    -Dhttp.address=0.0.0.0 \
    -Dpidfile.path=/dev/null; }; \
    if [ -d /data ]; then mkdir -p /data/logs; launch 2>> "$stderr_log"; else launch; fi
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
