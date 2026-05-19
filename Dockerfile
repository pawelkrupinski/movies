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
EXPOSE 9000
CMD exec bin/movies \
    -Dplay.http.secret.key="${APPLICATION_SECRET}" \
    -Dplay.server.http.address=0.0.0.0 \
    -Dhttp.address=0.0.0.0 \
    -Dpidfile.path=/dev/null \
    -J-Xmx256m \
    -J-Xms128m
