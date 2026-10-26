#!/usr/bin/env bash
# RECORDING PREFLIGHT: before a `Record scrape fixtures` run spends 5 legs x 30-125 min, ask OMDb and TMDB, once each,
# whether they still take the run's keys.
#
# WHY. In the week to 2026-10-26, 14 of 24 recording runs failed or were cancelled (~18 runner-hours), and the largest
# single cause was OMDb refusing the key: runs 335, 408, 528, 563 and 2507 died on "OMDb refused its key N times in a
# row" (OMDbClient's KeyRefusedException) 22-125 min in, after every leg had booted, restored and replayed. A spent
# daily quota or a rotated key is already visible in one request at t=0. A dispatch that follows a run which spent
# the quota is the usual shape, and is exactly what this stops.
#
# Usage: recording-preflight.sh       (reads OMDB_API_KEY and TMDB_API_KEY; prints no value, not even a length)
#
# A REFUSAL fails (exit 1): HTTP 401/403, or OMDb's `"Response":"False"` with "Invalid API key" / "limit reached".
# Anything else that is not an answer — a timeout, a 5xx, no network — is only a warning (exit 0): the run's own
# clients retry those, and a preflight that fails on a blip would cost more runs than it saves.
# Tested by scripts/ci/recording-preflight-test.sh against a stub curl.
set -uo pipefail

problems=0
refused() { echo "::error::$*"; problems=$((problems + 1)); }

# probe <name> <url> [curl header args...]: prints "<http status> <body>" (status 000 when there was no answer).
probe() {
    local url=$1; shift
    local body status
    body=$(mktemp)
    # allow-silenced: a probe; no answer is status 000, which the caller reports as a warning
    status=$(curl -sS --max-time 20 --retry 2 --retry-all-errors -o "$body" -w '%{http_code}' "$@" "$url" 2>/dev/null) || true
    echo "${status:-000} $(head -c 400 "$body" | tr -d '\n')"
    rm -f "$body"
}

if [ -z "${OMDB_API_KEY:-}" ]; then
    refused "secret OMDB_API_KEY is unset or empty — the recording's OMDb lookups would all fail"
else
    answer=$(probe "https://www.omdbapi.com/?i=tt0113277&apikey=${OMDB_API_KEY}")
    status=${answer%% *}
    case "$status" in
        200)
            if grep -q '"Response":"True"' <<<"$answer"; then
                echo "OMDb: key accepted"
            elif grep -qiE 'invalid api key|limit reached' <<<"$answer"; then
                refused "OMDb refuses the key ($(grep -oiE 'invalid api key|request limit reached|limit reached' <<<"$answer" | head -1)); the run's lookups would fail the same way"
            else
                echo "::warning::OMDb answered 200 without a record for the probe title; not treated as a refusal"
            fi
            ;;
        401 | 403)
            refused "OMDb refuses the key (HTTP $status: $(grep -oiE 'invalid api key|request limit reached|limit reached' <<<"$answer" | head -1)) — spent daily quota or a bad key"
            ;;
        *) echo "::warning::OMDb did not answer the probe (HTTP $status); going ahead, its clients retry" ;;
    esac
fi

if [ -z "${TMDB_API_KEY:-}" ]; then
    refused "secret TMDB_API_KEY is unset or empty"
else
    # Both the `api_key` parameter and the bearer header, as TmdbApiKey.authorization sends them: a v3 key and a v4
    # read token each authenticate through one of the two.
    answer=$(probe "https://api.themoviedb.org/3/configuration?api_key=${TMDB_API_KEY}" -H "Authorization: Bearer ${TMDB_API_KEY}")
    status=${answer%% *}
    case "$status" in
        200) echo "TMDB: key accepted" ;;
        401 | 403) refused "TMDB refuses the key (HTTP $status)" ;;
        *) echo "::warning::TMDB did not answer the probe (HTTP $status); going ahead, its clients retry" ;;
    esac
fi

[ "$problems" -eq 0 ] || { echo "recording preflight: $problems refusal(s) — not starting the legs"; exit 1; }
echo "recording preflight: both keys accepted"
