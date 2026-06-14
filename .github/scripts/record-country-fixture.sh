#!/usr/bin/env bash
#
# Record the WHOLE country's fixture corpus into
# test/resources/fixtures/<dir> (default `today`) by running the real cinema
# scrape + the full enrichment cascade (TMDB → IMDb → Metacritic → RT → Filmweb)
# against live upstreams, then verify the capture is non-empty.
#
# Shared by two callers so a green local run is exactly what CI does:
#   • .github/workflows/country-fixture-artifact.yml — the daily artifact job,
#     which passes a zip target so the corpus can be uploaded.
#   • a local machine — just run `.github/scripts/record-country-fixture.sh`
#     to drop a fresh capture under test/resources/fixtures/today/.
#
# Env:
#   KINOWO_FIXTURE_DIR  fixture subdir under test/resources/fixtures
#                       (default `today`; the recorder reads this var itself).
#   TMDB_API_KEY        real TMDB key — enrichment 401s and captures nothing
#                       without it.
#   ZYTE_API_KEY        Zyte key for the Multikino / biletyna scrapes.
# (Locally these are auto-sourced from .env.local if not already set.)
#
# Args:
#   $1  optional path for a zip of the corpus. When given, the recorded tree is
#       wrapped in one colon-free zip there (the artifact job uploads it). Omit
#       for a plain on-disk capture — the normal local case.
set -euo pipefail

# Anchor to the repo root regardless of the caller's cwd.
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

# Local convenience: pull the two upstream keys from the gitignored .env.local
# when they aren't already in the environment. CI sets them as secrets, so the
# file is absent there and this is a no-op.
#
# We extract ONLY the keys we need rather than `source`-ing the whole file —
# .env.local also holds values bash can't safely execute (e.g. a comma-joined
# Fly token), and sourcing would abort the run on the first such line.
load_env_key() {
    local key="$1" line val
    line="$(grep -E "^[[:space:]]*(export[[:space:]]+)?${key}=" "$REPO_ROOT/.env.local" | tail -n1 || true)"
    [ -n "$line" ] || return 0
    val="${line#*=}"                       # drop everything up to the first =
    val="${val%\"}"; val="${val#\"}"       # strip a surrounding pair of "…"
    val="${val%\'}"; val="${val#\'}"       # …or '…'
    export "$key=$val"
}
if [ -f "$REPO_ROOT/.env.local" ]; then
    [ -n "${TMDB_API_KEY:-}" ] || load_env_key TMDB_API_KEY
    [ -n "${ZYTE_API_KEY:-}" ] || load_env_key ZYTE_API_KEY
fi

if [ -z "${TMDB_API_KEY:-}" ]; then
    echo "Warning: TMDB_API_KEY is not set — TMDB enrichment will 401 and the corpus will be sparse." >&2
fi

DIR_NAME="${KINOWO_FIXTURE_DIR:-today}"
DIR="test/resources/fixtures/$DIR_NAME"

echo "Recording country fixture corpus into $DIR …"
sbt "worker/Test/runMain clients.tools.RecordAllDataToFixture"

# Sanity-check the capture isn't empty before we treat the run as a success.
COUNT="$(find "$DIR" -type f | wc -l | tr -d ' ')"
echo "Captured $COUNT fixture files into $DIR ($(cat "$DIR/CAPTURE_DATE" 2>/dev/null | tr '\n' ' '))"
if [ "$COUNT" -eq 0 ]; then
    echo "::error::Recorder produced no fixture files — refusing to treat this as a successful capture." >&2
    exit 1
fi

ZIP_TARGET="${1:-}"
if [ -n "$ZIP_TARGET" ]; then
    # The recorder names per-URL fixture files after the URL, so some contain a
    # colon (e.g. `…,ts:1781388000`) — which upload-artifact@v4 rejects as an
    # NTFS-unsafe path char. Inside a zip those names are just bytes, so we wrap
    # the whole tree in a single colon-free zip and let the caller upload that.
    ( cd test/resources/fixtures && zip -q -r -X "$ZIP_TARGET" "$DIR_NAME" )
    echo "Wrote $(du -h "$ZIP_TARGET" | cut -f1) zip with $COUNT fixtures to $ZIP_TARGET"
fi
