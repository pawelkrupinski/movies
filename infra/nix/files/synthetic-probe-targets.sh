#!/usr/bin/env bash
# FINDS ONE LIVE FILM PAGE AND ITS SHARE CARD PER COUNTRY, FOR THE BLACKBOX PROBES TO FETCH.
#
# A film page cannot be a static probe target: a film's page 404s the day its last screening
# passes, so any slug written into the configuration becomes a permanent false ProbeFailing. This
# asks each country's city page which films it lists RIGHT NOW, takes the first, reads the share
# card its `og:image` names, and writes both as a Prometheus file_sd document that the `blackbox`
# job re-reads on its own (no Prometheus restart). It runs every few minutes from
# roles/synthetic-probes.nix, so a film that leaves the schedule is replaced long before
# ProbeFailing's hold could elapse on it.
#
# ALL OR NOTHING. If any country's discovery fails -- the city page did not answer, or listed no
# film -- the previous document is left exactly as it was and the failure is logged. Replacing it
# with the countries that did answer would silently drop a country's film probe on a transient
# error; keeping the old one means a discovery that stays broken ends as a film page that 404s,
# which ProbeFailing reports, naming the URL.
#
# Usage: synthetic-probe-targets.sh <output.json> <country>=<city page URL> ...
# Env:   PROBE_USER_AGENT (the User-Agent every fetch sends; the probes send the same one)
#        PROBE_TEXTFILE   (optional: a node_exporter textfile to publish
#                          kinowo_synthetic_probe_discovery_last_success_timestamp_seconds into,
#                          written only when a discovery succeeded, so ProbeDiscoveryStale can
#                          tell a discovery that keeps failing from one that keeps working)
set -uo pipefail

out="${1:?usage: synthetic-probe-targets.sh <output.json> <country>=<city url> ...}"
shift
ua="${PROBE_USER_AGENT:-kinowo-synthetic-probe/1}"

fetch() { curl -fsS --max-time 20 -A "$ua" "$1"; }

targets='[]'
failed=0
for pair in "$@"; do
  country="${pair%%=*}"
  city="${pair#*=}"
  origin="$(printf '%s' "$city" | sed -E 's|^(https?://[^/]+).*|\1|')"

  if ! page="$(fetch "$city")"; then
    echo "synthetic-probe-targets: $country: could not fetch the city page $city" >&2
    failed=1
    continue
  fi
  href="$(printf '%s' "$page" | grep -oE 'href="[^"]*/movie/[^"/?#]+"' | head -1 | sed -E 's/^href="//; s/"$//')"
  if [ -z "$href" ]; then
    echo "synthetic-probe-targets: $country: $city lists no film" >&2
    failed=1
    continue
  fi
  case "$href" in
    http://* | https://*) film="$href" ;;
    *) film="$origin$href" ;;
  esac
  targets="$(jq -c --arg t "$film" --arg c "$country" \
    '. + [{targets: [$t], labels: {country: $c, kind: "film", __param_module: "http_page"}}]' <<<"$targets")"

  # THE SHARE CARD IS OPTIONAL. A film whose card has not been rendered yet names the city's
  # fallback image instead, which is not under /share-cards/ and is not what this probe is for.
  if film_page="$(fetch "$film")"; then
    card="$(printf '%s' "$film_page" | grep -oE 'og:image"[[:space:]]+content="[^"]*/share-cards/[^"]*"' \
      | head -1 | sed -E 's/.*content="//; s/"$//')"
    if [ -n "$card" ]; then
      targets="$(jq -c --arg t "$card" --arg c "$country" \
        '. + [{targets: [$t], labels: {country: $c, kind: "share-card", __param_module: "http_asset"}}]' <<<"$targets")"
    fi
  else
    echo "synthetic-probe-targets: $country: the film page $film did not answer" >&2
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
