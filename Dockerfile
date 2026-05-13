FROM eclipse-temurin:21 AS build
WORKDIR /app

# Install sbt
RUN apt-get update && \
    apt-get install -y --no-install-recommends curl && \
    curl -fsSL "https://github.com/sbt/sbt/releases/download/v1.10.1/sbt-1.10.1.tgz" \
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
CMD exec bin/movies \
    -Dplay.http.secret.key="${APPLICATION_SECRET}" \
    -Dplay.server.http.address=0.0.0.0 \
    -Dhttp.address=0.0.0.0 \
    -Dpidfile.path=/dev/null \
    -J-Xmx400m \
    -J-Xms128m
