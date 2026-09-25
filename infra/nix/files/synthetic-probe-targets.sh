#!/usr/bin/env bash
# FINDS ONE LIVE FILM PAGE AND ITS SHARE CARD PER COUNTRY, FOR THE BLACKBOX PROBES TO FETCH.
#
# A film page cannot be a static probe target: a film's page 404s the day its last screening
# passes, so any slug written into the configuration becomes a permanent false ProbeFailing. This
# asks each country's city page which films it lists RIGHT NOW, takes one, reads the share card its
# `og:image` names, and writes both as a Prometheus file_sd document that the `blackbox` job
# re-reads on its own (no Prometheus restart). It runs every few minutes from
# roles/synthetic-probes.nix, so a film that leaves the schedule is replaced long before
# ProbeFailing's hold could elapse on it.
#
# STICKY. The film probed last time is kept for as long as its city page still lists it. The first
# film a city page lists changes many times a day (Warsaw's changed five times in three hours on
# 2026-09-24), and every change is a new `instance` -- a new series whose `for:` holds start from
# zero, so a ProbeSlow that needs twenty minutes on one URL could never complete. A kept film whose
# share card is already known is not fetched again: its targets are carried over as they were. The
# film page was the costliest fetch a run made (a London film page is ~870 KB, read from the origin
# every two minutes per country); a film with no card yet is still re-read, so a card rendered
# later is picked up.
#
# THE FILM PAGE IS PROBED THROUGH THE EDGE, 403 OR NOT. showtimes.cc's Cloudflare custom rule
# "Challenge non-verified-bot traffic on catalog paths" answers `/movie/` with a managed challenge
# (a JavaScript page, `cf-mitigated: challenge`, 403) for any client that is not a verified bot -- a
# deliberate rule against scrapers, which blackbox_exporter (no JavaScript) cannot pass. Since
# 2026-09-25 the rule exempts monitoring-1's address (`ip.src ne 128.140.49.167`), so its film pages
# answer 200 here like kinowo.net's, and the film page is probed alongside its share card. Should
# the exemption be lost (the rule edited, or monitoring-1's address changed), the probe reads 403
# and ProbeBlockedByEdge says so, naming the URL -- which is why a refused film page keeps its
# probe. Discovery must not go blind meanwhile, so a page the edge answers 403 is then read from
# the ORIGIN (PROBE_ORIGIN_ADDRESS, over the private network, TLS pinned to the origin certificates
# in PROBE_ORIGIN_CA): the share card it names is still found, and probed through the edge.
#
# ALL OR NOTHING. If any country's discovery fails -- the city page did not answer, or listed no
# film -- the previous document is left exactly as it was and the failure is logged. Replacing it
# with the countries that did answer would silently drop a country's film probe on a transient
# error; keeping the old one means a discovery that stays broken ends as a film page that 404s,
# which ProbeFailing reports, naming the URL.
#
# Usage: synthetic-probe-targets.sh <output.json> <country>=<city page URL> ...
# Env:   PROBE_USER_AGENT     (the User-Agent every fetch sends; the probes send the same one)
#        PROBE_TEXTFILE       (optional: a node_exporter textfile to publish
#                              kinowo_synthetic_probe_discovery_last_success_timestamp_seconds into,
#                              written only when a discovery succeeded, so ProbeDiscoveryStale can
#                              tell a discovery that keeps failing from one that keeps working)
#        PROBE_ORIGIN_ADDRESS (optional: the origin's address, for a page the edge answers 403)
#        PROBE_ORIGIN_CA      (with it: the PEM file of origin certificates to trust)
set -uo pipefail

out="${1:?usage: synthetic-probe-targets.sh <output.json> <country>=<city url> ...}"
shift
ua="${PROBE_USER_AGENT:-kinowo-synthetic-probe/1}"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# fetch <url> [origin] -- leaves the body in $work/body and the HTTP status in $status (000 when
# nothing answered); succeeds on a 2xx.
fetch() {
  local url="$1" args=(-sS --compressed --max-time 20 -A "$ua" -o "$work/body" -w '%{http_code}')
  if [ "${2:-}" = origin ]; then
    local host
    host="$(printf '%s' "$url" | sed -E 's|^https?://([^/:]+).*|\1|')"
    args+=(--resolve "$host:443:$PROBE_ORIGIN_ADDRESS" --cacert "$PROBE_ORIGIN_CA")
  fi
  : > "$work/body"
  status="$(curl "${args[@]}" "$url")" || true
  status="${status:-000}"
  [ "${status:0:1}" = 2 ]
}

# fetch_page <url> -- through the edge; on the edge's 403, from the origin when one is configured.
# $edge_status keeps the edge's answer, so the caller can tell a refused page from a missing one.
fetch_page() {
  fetch "$1" && { edge_status="$status"; return 0; }
  edge_status="$status"
  [ "$status" = 403 ] && [ -n "${PROBE_ORIGIN_ADDRESS:-}" ] || return 1
  fetch "$1" origin
}

# Every film href on a page, in order, absolute against `origin`. Read to the end (`sed`, not
# `head`), so grep is never cut off mid-write by a closed pipe.
film_links() {
  grep -oE 'href="[^"]*/movie/[^"/?#]+"' "$work/body" | sed -E 's/^href="//; s/"$//' |
    while IFS= read -r href; do
      case "$href" in
        http://* | https://*) printf '%s\n' "$href" ;;
        *) printf '%s%s\n' "$1" "$href" ;;
      esac
    done
}

# The film this country's probes followed last time, carried in the hidden `__film` label
# (Prometheus drops `__` labels after relabelling, so it never reaches a series).
previous_film() {
  [ -f "$out" ] || return 0
  jq -r --arg c "$1" 'first(.[] | select(.labels.country == $c) | .labels.__film // empty) // empty' \
    "$out" 2>/dev/null
}

# This country's targets from last time, when they include a share card -- printed as a JSON array
# for the caller to carry over. Fails when there is no card to carry.
previous_targets_with_card() {
  [ -f "$out" ] || return 1
  jq -ec --arg c "$1" '[.[] | select(.labels.country == $c)] | select(any(.labels.kind == "share-card"))' \
    "$out" 2>/dev/null
}

targets='[]'
failed=0
for pair in "$@"; do
  country="${pair%%=*}"
  city="${pair#*=}"
  origin="$(printf '%s' "$city" | sed -E 's|^(https?://[^/]+).*|\1|')"

  if ! fetch_page "$city"; then
    echo "synthetic-probe-targets: $country: could not fetch the city page $city (HTTP $status)" >&2
    failed=1
    continue
  fi
  film_links "$origin" > "$work/films"
  if [ ! -s "$work/films" ]; then
    echo "synthetic-probe-targets: $country: $city lists no film" >&2
    failed=1
    continue
  fi
  film="$(previous_film "$country")"
  if [ -z "$film" ] || ! grep -qxF "$film" "$work/films"; then
    film="$(sed -n 1p "$work/films")"
  elif kept="$(previous_targets_with_card "$country")"; then
    targets="$(jq -c --argjson kept "$kept" '. + $kept' <<<"$targets")"
    continue
  fi

  if fetch_page "$film"; then
    [ "$edge_status" = 403 ] &&
      echo "synthetic-probe-targets: $country: the edge refuses $film (403), so monitoring-1's Cloudflare exemption is lost; read from the origin, still probed" >&2
    targets="$(jq -c --arg t "$film" --arg c "$country" \
      '. + [{targets: [$t], labels: {country: $c, kind: "film", __param_module: "http_page", __film: $t}}]' <<<"$targets")"
    # THE SHARE CARD IS OPTIONAL. A film whose card has not been rendered yet names the city's
    # fallback image instead, which is not under /share-cards/ and is not what this probe is for.
    card="$(grep -oE 'og:image"[[:space:]]+content="[^"]*/share-cards/[^"]*"' "$work/body" |
      sed -n '1{s/.*content="//; s/"$//; p;}')"
    if [ -n "$card" ]; then
      targets="$(jq -c --arg t "$card" --arg c "$country" --arg f "$film" \
        '. + [{targets: [$t], labels: {country: $c, kind: "share-card", __param_module: "http_asset", __film: $f}}]' <<<"$targets")"
    fi
  else
    echo "synthetic-probe-targets: $country: the film page $film did not answer (HTTP $status)" >&2
    # Still probed: a film page that fails is exactly what ProbeFailing is for.
    targets="$(jq -c --arg t "$film" --arg c "$country" \
      '. + [{targets: [$t], labels: {country: $c, kind: "film", __param_module: "http_page", __film: $t}}]' <<<"$targets")"
  fi
done

if [ "$failed" -ne 0 ]; then
  echo "synthetic-probe-targets: keeping the previous $out (discovery failed above)" >&2
  exit 0
fi

tmp="$out.tmp.$$"
if jq . <<<"$targets" > "$tmp" && mv -f "$tmp" "$out"; then
  echo "synthetic-probe-targets: wrote $(jq length <<<"$targets") target(s) to $out"
  if [ -n "${PROBE_TEXTFILE:-}" ]; then
    {
      echo "# HELP kinowo_synthetic_probe_discovery_last_success_timestamp_seconds When the synthetic probes last found a live film page for every country."
      echo "# TYPE kinowo_synthetic_probe_discovery_last_success_timestamp_seconds gauge"
      echo "kinowo_synthetic_probe_discovery_last_success_timestamp_seconds $(date +%s)"
    } > "$PROBE_TEXTFILE.tmp" && chmod 0644 "$PROBE_TEXTFILE.tmp" && mv -f "$PROBE_TEXTFILE.tmp" "$PROBE_TEXTFILE"
  fi
else
  rm -f "$tmp"
  echo "synthetic-probe-targets: could not write $out" >&2
  exit 1
fi
