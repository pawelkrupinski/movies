# Single-stage runtime image. The Play `universal/stage` distribution is
# produced by `sbt stage` in the GitHub Actions `test` job (see
# .github/workflows/deploy.yml) and downloaded into a top-level `stage/`
# directory before this image is built. `.dockerignore` whitelists exactly
# that directory, so Fly's remote builder receives just the staged JARs +
# startup scripts — no JDK, no sbt, no source.
#
# Temporarily on Java 21 (vs Java 25 in CI) to isolate Java 25's non-heap
# footprint as the suspected OOM trigger on the 512 MB Fly machine. The
# JARs are JDK-agnostic — Scala 3's default `-java-output-version 8`
# emits Java 8 bytecode — so the Java-25-built dist runs unchanged here.
FROM eclipse-temurin:21-jre
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
    -J-Xmx256m \
    -J-Xms128m \
    -J-XX:ReservedCodeCacheSize=96m \
    -J-XX:MaxMetaspaceSize=192m \
    -J-XX:MaxDirectMemorySize=128m
    # Cap JVM non-heap regions. Java 21 defaults to ReservedCodeCacheSize=240m,
    # unbounded metaspace, MaxDirectMemorySize=Xmx (256m). Capping each stops
    # silent reservation drift, but the first attempt (64m/128m/64m) starved
    # Mongo/Netty's direct buffers + Play's metaspace at boot — JVM crashed
    # before it could bind port 9000 (no OOM-kill line; native allocation
    # failure inside the JVM). Loosened to values that still recover ~150 MB
    # vs the defaults while leaving headroom for class loading and reactive
    # streams direct allocations.
