# Single-stage runtime image. The Play `universal/stage` distribution is
# produced by `sbt stage` in the GitHub Actions `test` job (see
# .github/workflows/deploy.yml) and downloaded into a top-level `stage/`
# directory before this image is built. `.dockerignore` whitelists exactly
# that directory, so Fly's remote builder receives just the staged JARs +
# startup scripts — no JDK, no sbt, no source.
#
# Eclipse Temurin (HotSpot) JRE 25 — the current LTS, and the highest
# Java version Play 3.0.x has shipped tested. Scala 3.8.3 emits Java 21
# bytecode (the highest output version it accepts); JRE 25 loads those
# class files unchanged. CI builds on the same JDK 25 — toolchain
# consistent end-to-end.
FROM eclipse-temurin:25-jre
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
EXPOSE 9000
CMD exec bin/movies \
    -Dplay.http.secret.key="${APPLICATION_SECRET}" \
    -Dplay.server.http.address=0.0.0.0 \
    -Dhttp.address=0.0.0.0 \
    -Dpidfile.path=/dev/null \
    -J-Xms384m \
    -J-Xmx384m \
    -J-XX:+UseG1GC \
    -J-XX:MaxGCPauseMillis=50 \
    -J-XX:+UseStringDeduplication \
    -J-XX:ReservedCodeCacheSize=96m \
    -J-XX:MaxMetaspaceSize=160m \
    -J-XX:MaxDirectMemorySize=96m \
    -J-Xlog:gc*:stderr:time,uptime,level,tags
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
    #   - GC log to stderr: the JVM's own pause record. Cheap, picked
    #     up by Fly's log aggregator alongside the app's own logs.
    #     Lets the next perf investigation correlate request latency
    #     spikes with GC events without redeploying. (Note: `stderr`
    #     output doesn't take a `filecount` decorator — that's for
    #     `file=…` outputs.)
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
