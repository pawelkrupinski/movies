FROM eclipse-temurin:25 AS build
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
FROM eclipse-temurin:25-jre
ARG COMMIT_SHA=unknown
ENV COMMIT_SHA=$COMMIT_SHA
WORKDIR /app
COPY --from=build /app/target/universal/stage .
EXPOSE 9000
# `--sun-misc-unsafe-memory-access=allow`: Scala 3.3.7's LazyVals runtime
# helper still calls `sun.misc.Unsafe.objectFieldOffset`; JDK 24+'s default
# warns on every such call. The 3.3 LTS line hasn't backported the
# VarHandle migration that landed in 3.4+, so until the next Scala LTS
# adopts it (or we move off 3.3) the warning is noise — `allow` silences
# it. `warn` is the default; `deny` would block the call.
CMD exec bin/movies \
    -Dplay.http.secret.key="${APPLICATION_SECRET}" \
    -Dplay.server.http.address=0.0.0.0 \
    -Dhttp.address=0.0.0.0 \
    -Dpidfile.path=/dev/null \
    -J-Xmx256m \
    -J-Xms128m \
    -J--sun-misc-unsafe-memory-access=allow
