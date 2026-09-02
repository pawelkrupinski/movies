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
#   KINOWO_COUNTRY      which country to record: pl (default) | de | uk | us | es. The
#                       recorder walks `country.cities`, so WITHOUT this it
#                       silently records Poland whatever you meant — which is why
#                       the corpus was Poland-only for as long as it existed and
#                       Germany and the UK had no offline replay at all.
#   KINOWO_FIXTURE_DIR  fixture subdir under test/resources/fixtures. Defaults to
#                       `today` for Poland (the name the local sync agent pulls)
#                       and `today-<code>` otherwise, so recording one country
#                       never overwrites another's corpus.
#   TMDB_API_KEY        REQUIRED — real TMDB key; the whole enrichment cascade
#                       401s and captures nothing without it.
#   ZYTE_API_KEY        REQUIRED — Zyte key for the Multikino / biletyna scrapes.
# Both keys are auto-loaded from .env.local locally; the script exits 1 if either
# is still missing rather than recording a silently-partial corpus.
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

# Both keys are required: without TMDB_API_KEY the whole enrichment cascade
# (TMDB → IMDb → MC → RT → Filmweb) 401s and captures nothing; without
# ZYTE_API_KEY the Multikino / biletyna scrapes never reach their sites. A
# partial capture is worse than no capture — it looks complete but silently
# omits whole cinemas/ratings — so fail loudly rather than record a sparse one.
missing=""
[ -n "${TMDB_API_KEY:-}" ] || missing="$missing TMDB_API_KEY"
[ -n "${ZYTE_API_KEY:-}" ] || missing="$missing ZYTE_API_KEY"
if [ -n "$missing" ]; then
    echo "::error::Missing required key(s):$missing — set them in the environment or .env.local before recording." >&2
    exit 1
fi

# WHICH country. `Country.fromEnv` reads the singular `KINOWO_COUNTRY` while the
# worker wiring reads the plural `KINOWO_COUNTRIES`, and anything that consults
# only one of them falls back to the Poland default — so set BOTH or the run
# records a catalogue you didn't ask for while looking like it worked.
COUNTRY="${KINOWO_COUNTRY:-pl}"
case "$COUNTRY" in
    pl|de|uk|us|es) ;;
    *) echo "::error::KINOWO_COUNTRY='$COUNTRY' is not one of pl|de|uk|us|es." >&2; exit 1 ;;
esac
export KINOWO_COUNTRY="$COUNTRY"
export KINOWO_COUNTRIES="$COUNTRY"

if [ -z "${KINOWO_FIXTURE_DIR:-}" ]; then
    if [ "$COUNTRY" = "pl" ]; then KINOWO_FIXTURE_DIR="today"; else KINOWO_FIXTURE_DIR="today-$COUNTRY"; fi
fi
export KINOWO_FIXTURE_DIR

DIR_NAME="$KINOWO_FIXTURE_DIR"
DIR="test/resources/fixtures/$DIR_NAME"

echo "Recording the $COUNTRY fixture corpus into $DIR …"
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
