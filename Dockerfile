FROM eclipse-temurin:21 AS build
WORKDIR /app

# Install sbt
RUN apt-get update && \
    apt-get install -y --no-install-recommends curl && \
    curl -fsSL "https://github.com/sbt/sbt/releases/download/v1.12.11/sbt-1.12.11.tgz" \
      | tar xz -C /usr/local --strip-components=1 && \
    rm -rf /var/lib/apt/lists/*

# Resolve dependencies before copying source so this layer is cached
COPY build.sbt .
COPY project/ project/
RUN sbt update

# Build the production distribution
COPY app/ app/
COPY conf/ conf/
RUN sbt stage

# ── Runtime image ─────────────────────────────────────────────────────────────
FROM eclipse-temurin:21-jre
ARG COMMIT_SHA=unknown
ENV COMMIT_SHA=$COMMIT_SHA
WORKDIR /app
COPY --from=build /app/target/universal/stage .
EXPOSE 9000
# Temporarily reverted to Java 21 to isolate Java 25's non-heap footprint
# as the OOM trigger on the 512 MB Fly machine. The
# `--sun-misc-unsafe-memory-access=allow` flag (Scala 3.3.7's LazyVals
# helper, JDK 24+ deprecation noise) is JDK 24+-only and would crash the
# JVM on launch, so it's dropped while we're on 21.
CMD exec bin/movies \
    -Dplay.http.secret.key="${APPLICATION_SECRET}" \
    -Dplay.server.http.address=0.0.0.0 \
    -Dhttp.address=0.0.0.0 \
    -Dpidfile.path=/dev/null \
    -J-Xmx256m \
    -J-Xms128m
