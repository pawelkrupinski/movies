#!/usr/bin/env bash
# RECORDING PREFLIGHT: before a `Record scrape fixtures` run spends 5 legs x 30-125 min, ask OMDb and TMDB, once each,
# whether they still take the run's keys.
#
# WHY. In the week to 2026-10-26, 14 of 24 recording runs failed or were cancelled (~18 runner-hours), and the largest
# single cause was OMDb refusing the key: runs 335, 408, 528, 563 and 2507 died on "OMDb refused its key N times in a
# row" (OMDbClient's KeyRefusedException) 22-125 min in, after every leg had booted, restored and replayed. A rotated
# or bad key is already visible in one request at t=0.
#
# A SPENT DAILY QUOTA ONLY WARNS. OMDb is the recording's one best-effort origin -- the owner chose recordings that pass
# without it (recording_health.py, BEST_EFFORT) -- and a recording's suite is no verdict (capture only), so nothing else
# in the run fails on it: #2388 (run 3571) met OMDb's 401 from its first ask, 553 refusals in Germany alone, and went
# green. A refused key is never remembered (CachingEnrichmentFetch), so each gap stays a gap for the hermetic replay to
# name and the next fill whose key works to close. Failing here on it was the one red the quota still caused, and it
# recorded nothing at all: the nightly #2426 met a day the workers had spent, CI still holding their shared
# 1,000-a-day key (the vault had no OMDB_API_KEY_CI yet, infra/k8s-secrets/github-secrets.map).
#
# Usage: recording-preflight.sh       (reads OMDB_API_KEY and TMDB_API_KEY; prints no value, not even a length)
#
# A REFUSAL fails (exit 1): an unset key; TMDB's HTTP 401/403; OMDb's "Invalid API key!" (in a 401 or a 200 document)
# or a 401/403 that says nothing else, which no new day restores. OMDb's "Request limit reached!" (its spent daily
# quota, likewise either way) is a warning: the legs record without OMDb until its day turns at midnight US Central.
# Anything else that is not an answer — a timeout, a 5xx, no network — is only a warning (exit 0): the run's own
# clients retry those, and a preflight that fails on a blip would cost more runs than it saves.
# Tested by scripts/ci/recording-preflight-test.sh against a stub curl.
set -uo pipefail

problems=0
refused() { echo "::error::$*"; problems=$((problems + 1)); }

# probe <url> [curl header args...]: prints "<http status> <body>" (status 000 when there was no answer). The WHOLE
# body: OMDb puts "Response" last, after the plot and the ratings, and a record cut at 400 bytes read as no record.
probe() {
    local url=$1; shift
    local body status
    body=$(mktemp)
    # allow-silenced: a probe; no answer is status 000, which the caller reports as a warning
    status=$(curl -sS --max-time 20 --retry 2 --retry-all-errors -o "$body" -w '%{http_code}' "$@" "$url" 2>/dev/null) || true
    echo "${status:-000} $(tr -d '\n' <"$body")"
    rm -f "$body"
}

# omdb_error <answer>: OMDb's own reason, out of its `{"Response":"False","Error":"..."}` document; empty without one.
omdb_error() { grep -oE '"Error" *: *"[^"]*"' <<<"$1" | head -1 | sed -E 's/^"Error" *: *"(.*)"$/\1/'; }

if [ -z "${OMDB_API_KEY:-}" ]; then
    refused "secret OMDB_API_KEY is unset or empty — the recording's OMDb lookups would all fail"
else
    answer=$(probe "https://www.omdbapi.com/?i=tt0113277&apikey=${OMDB_API_KEY}")
    status=${answer%% *}
    error=$(omdb_error "$answer")
    if [ "$status" = 200 ] && grep -q '"Response":"True"' <<<"$answer"; then
        echo "OMDb: key accepted"
    elif grep -qi 'limit reached' <<<"$error"; then
        echo "::warning::OMDb's daily quota is spent (HTTP $status: $error) — recording without OMDb, its best-effort" \
            "origin: every ask is refused until OMDb's day turns at midnight US Central (05:00 UTC in summer time, 06:00" \
            "in winter) and left a gap for the next fill whose key works. Spent by this OMDb day's earlier recordings, or" \
            "by prod's workers while CI holds their key (OMDB_API_KEY_CI, infra/k8s-secrets/github-secrets.map)"
    elif grep -qi 'invalid api key' <<<"$error" || [ "$status" = 401 ] || [ "$status" = 403 ]; then
        refused "OMDb refuses the key (HTTP $status: ${error:-no reason given}) — a bad or rotated key, which no new day restores"
    elif [ "$status" = 200 ]; then
        echo "::warning::OMDb answered 200 without a record for the probe title (${error:-no reason given}); not treated as a refusal"
    else
        echo "::warning::OMDb did not answer the probe (HTTP $status); going ahead, its clients retry"
    fi
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
echo "recording preflight: no key refused"
