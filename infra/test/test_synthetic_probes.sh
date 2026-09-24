#!/usr/bin/env bash
# THE SYNTHETIC PROBES, ASKED WHETHER THEY PROBE WHAT THEY SAY (roles/synthetic-probes.nix).
#
# Two halves, because the module fails in two different silent ways:
#
#   discovery   nix/files/synthetic-probe-targets.sh finds a live film page and share card per
#               country. Wrong, it writes a target that 404s (a paging ProbeFailing for a site that
#               is up) or throws away the last good targets on a transient error. Driven here
#               against a stub `curl` serving fixture pages, so it runs anywhere, offline.
#   the job     the `blackbox` scrape job monitoring-1 would install, read with `nix eval` and
#               handed to `promtool check config`. A job that parses but relabels wrongly probes
#               127.0.0.1:9115 itself, or labels every alert with the exporter's address instead
#               of the URL that failed -- promtool is happy with both, so the shape is asserted.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"
script="$infra/nix/files/synthetic-probe-targets.sh"
. "$here/../../scripts/shell-spec.sh"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# ── A STUB curl ────────────────────────────────────────────────────────────────────────────────
# Serves $tmp/pages/<url with / and : replaced by _> -- or, for a fetch pinned to the origin with
# `--resolve`, $tmp/pages/origin/<same>. A page's status is 200, or what `refuse` wrote beside it;
# a missing page is a 404. Honours the `-o` and `-w '%{http_code}'` the discovery reads, and logs
# every call's arguments to $STUB_LOG.
mkdir -p "$tmp/bin" "$tmp/pages/origin"
cat > "$tmp/bin/curl" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
url="${@: -1}" out=/dev/stdout write="" dir="$STUB_PAGES"
while [ $# -gt 1 ]; do
  case "$1" in
    -o) out="$2"; shift ;;
    -w) write="$2"; shift ;;
    --resolve) dir="$STUB_PAGES/origin"; shift ;;
  esac
  shift
done
page="$dir/$(printf '%s' "$url" | tr '/:' '__')"
code=404
if [ -f "$page" ]; then code="$(cat "$page.status" 2>/dev/null || echo 200)"; cat "$page" > "$out"; fi
[ -n "$write" ] && printf '%s' "$code"
exit 0
STUB
chmod +x "$tmp/bin/curl"
serve() { printf '%s' "$2" > "$tmp/pages/$(printf '%s' "$1" | tr '/:' '__')"; }
serve_origin() { printf '%s' "$2" > "$tmp/pages/origin/$(printf '%s' "$1" | tr '/:' '__')"; }
# The edge's managed challenge: a 403 with a JavaScript page.
refuse() { serve "$1" '<title>Just a moment...</title></html>'; printf 403 > "$tmp/pages/$(printf '%s' "$1" | tr '/:' '__').status"; }
# SIGPIPE IGNORED, as systemd runs every service (`IgnoreSIGPIPE=yes`): a closed pipe is then an
# EPIPE its writer reports on stderr rather than a silent death, which is how the journal filled
# with "grep: write error: Broken pipe".
run() {
  : > "$tmp/curl.log"
  (trap '' PIPE
    PATH="$tmp/bin:$PATH" STUB_PAGES="$tmp/pages" STUB_LOG="$tmp/curl.log" PROBE_TEXTFILE="$tmp/probes.prom" \
      exec bash "$script" "$@" 2>"$tmp/stderr")
}
field() { jq -r "$1" "$tmp/targets.json"; }

# Poland mounted at the root with relative hrefs; the UK path-mounted; the film page's og:image
# spaced the way the real template spaces it.
serve "https://kinowo.net/warszawa/" '<a href="/warszawa/movie/100-dni-misja-zeus">x</a><a href="/warszawa/movie/other">y</a></html>'
serve "https://kinowo.net/warszawa/movie/100-dni-misja-zeus" \
  '<meta property="og:image"        content="https://kinowo.net/share-cards/pl/had62.jpg?v=b179"></html>'
serve "https://showtimes.cc/uk/london/" '<a href="/uk/london/movie/bad-apples">x</a></html>'
serve "https://showtimes.cc/uk/london/movie/bad-apples" \
  '<meta property="og:image" content="https://showtimes.cc/uk/assets/img/og-london.jpg"></html>'

echo "discovery: a film per country, and its share card when it has one"
run "$tmp/targets.json" pl=https://kinowo.net/warszawa/ uk=https://showtimes.cc/uk/london/
check "the FIRST film listed on each city page, made absolute against the page's origin" \
  "https://kinowo.net/warszawa/movie/100-dni-misja-zeus https://showtimes.cc/uk/london/movie/bad-apples" \
  "$(field '[.[] | select(.labels.kind == "film") | .targets[0]] | join(" ")')"
check "the film's share card, probed as an asset" \
  "https://kinowo.net/share-cards/pl/had62.jpg?v=b179 http_asset pl" \
  "$(field '.[] | select(.labels.kind == "share-card") | "\(.targets[0]) \(.labels.__param_module) \(.labels.country)"')"
check "a film whose og:image is the city fallback gets no share-card probe (it is not a card)" \
  "0" "$(field '[.[] | select(.labels.kind == "share-card" and .labels.country == "uk")] | length')"
check "film pages are probed as pages" "http_page http_page" \
  "$(field '[.[] | select(.labels.kind == "film") | .labels.__param_module] | join(" ")')"
check "a success publishes the discovery timestamp for ProbeDiscoveryStale" \
  "kinowo_synthetic_probe_discovery_last_success_timestamp_seconds" \
  "$(grep -o '^kinowo_synthetic_probe_discovery_last_success_timestamp_seconds' "$tmp/probes.prom")"

echo "discovery: all or nothing"
cp "$tmp/targets.json" "$tmp/before.json"
rm -f "$tmp/probes.prom"
serve "https://showtimes.cc/de/berlin/" '<p>no films today</p></html>'
run "$tmp/targets.json" pl=https://kinowo.net/warszawa/ de=https://showtimes.cc/de/berlin/
check "a country whose city page lists no film leaves the previous targets untouched" \
  "same" "$(cmp -s "$tmp/before.json" "$tmp/targets.json" && echo same || echo changed)"
check "...says which country and why" "1" "$(grep -c 'de: https://showtimes.cc/de/berlin/ lists no film' "$tmp/stderr")"
check "...and does NOT publish a success" "absent" "$([ -f "$tmp/probes.prom" ] && echo present || echo absent)"
run "$tmp/targets.json" pl=https://kinowo.net/warszawa/ es=https://showtimes.cc/es/madrid/
check "a city page that does not answer leaves them untouched too" \
  "same" "$(cmp -s "$tmp/before.json" "$tmp/targets.json" && echo same || echo changed)"

echo "discovery: a film that left the schedule is replaced on the next run"
serve "https://kinowo.net/warszawa/" '<a href="/warszawa/movie/next-film">x</a></html>'
run "$tmp/targets.json" pl=https://kinowo.net/warszawa/
check "the new first film replaces the old one" "https://kinowo.net/warszawa/movie/next-film" \
  "$(field '[.[] | select(.labels.kind == "film") | .targets[0]] | join(" ")')"

echo "discovery: the probed film stays put while its city page still lists it"
serve "https://kinowo.net/warszawa/movie/next-film" \
  '<meta property="og:image" content="https://kinowo.net/share-cards/pl/hnext.jpg?v=1"></html>'
run "$tmp/targets.json" pl=https://kinowo.net/warszawa/
# A real city page lists hundreds of films; big enough here that a reader which stops at the first
# match closes the pipe on a grep still writing.
serve "https://kinowo.net/warszawa/" \
  "<a href=\"/warszawa/movie/newly-first\">x</a><a href=\"/warszawa/movie/next-film\">y</a>$(
    for i in $(seq 1 20000); do printf '<a href="/warszawa/movie/film-%d">f</a>' "$i"; done)</html>"
run "$tmp/targets.json" pl=https://kinowo.net/warszawa/
check "a film that is no longer FIRST but still listed keeps its probe (and its series' holds)" \
  "https://kinowo.net/warszawa/movie/next-film https://kinowo.net/share-cards/pl/hnext.jpg?v=1" \
  "$(field '[.[] | .targets[0]] | join(" ")')"
check "...carried in a hidden label Prometheus drops after relabelling" \
  "__film=https://kinowo.net/warszawa/movie/next-film" \
  "$(field '[.[] | .labels.__film] | unique | map("__film=" + .) | join(" ")')"
check "grep is never cut off mid-write (no broken-pipe noise in the journal)" "0" \
  "$(grep -c 'Broken pipe' "$tmp/stderr")"

echo "discovery: a film page the edge challenges is read from the origin, and not probed"
# showtimes.cc's managed challenge on /movie/ paths: the city page answers, the film page 403s.
serve "https://showtimes.cc/us/new-york/" '<a href="/us/new-york/movie/coyote-vs-acme">x</a></html>'
refuse "https://showtimes.cc/us/new-york/movie/coyote-vs-acme"
serve_origin "https://showtimes.cc/us/new-york/movie/coyote-vs-acme" \
  '<meta property="og:image" content="https://showtimes.cc/share-cards/us/h2dd4.jpg?v=01dd"></html>'
PROBE_ORIGIN_ADDRESS=10.20.0.12 PROBE_ORIGIN_CA=/etc/origin.pem \
  run "$tmp/targets.json" us=https://showtimes.cc/us/new-york/
check "the share card found on the origin's copy of the page is probed, through the edge" \
  "share-card https://showtimes.cc/share-cards/us/h2dd4.jpg?v=01dd" \
  "$(field '[.[] | "\(.labels.kind) \(.targets[0])"] | join(" ")')"
check "the origin fetch is pinned to the origin address and its certificates" "1" \
  "$(grep -c -- '--resolve showtimes.cc:443:10.20.0.12 --cacert /etc/origin.pem https://showtimes.cc/us/new-york/movie/coyote-vs-acme' "$tmp/curl.log")"
check "...and the film page gets no probe that could only ever read the challenge" "1" \
  "$(grep -c 'us: the edge refuses https://showtimes.cc/us/new-york/movie/coyote-vs-acme (403)' "$tmp/stderr")"
check "the stickiness survives a challenged film page (the hidden label is on the card)" \
  "https://showtimes.cc/us/new-york/movie/coyote-vs-acme" "$(field '.[0].labels.__film')"

run "$tmp/targets.json" us=https://showtimes.cc/us/new-york/
check "with no origin configured a refused film page is still probed (and reads 403)" \
  "film https://showtimes.cc/us/new-york/movie/coyote-vs-acme" \
  "$(field '[.[] | "\(.labels.kind) \(.targets[0])"] | join(" ")')"

# ── THE SCRAPE JOB monitoring-1 WOULD INSTALL ──────────────────────────────────────────────────
echo "the blackbox job on monitoring-1"
nix_flags=(--extra-experimental-features 'nix-command flakes')
if ! job="$(nix "${nix_flags[@]}" eval --raw \
  "$infra#nixosConfigurations.monitoring-1.config.environment.etc.\"prometheus/scrape.d/blackbox-targets.yaml\".text" \
  2>"$tmp/eval.err")"; then
  check "monitoring-1's blackbox scrape job evaluates" "evaluated" "$(tail -3 "$tmp/eval.err")"
  spec_summary
fi
printf '%s' "$job" > "$tmp/blackbox-targets.yaml"

check "every country's city page is a static target" "de es pl uk us" \
  "$(jq -r '[.scrape_configs[0].static_configs[] | select(.labels.kind == "city") | .labels.country] | sort | join(" ")' <<<"$job")"
check "both brands' front doors are static targets" "https://kinowo.net/ https://showtimes.cc/" \
  "$(jq -r '[.scrape_configs[0].static_configs[] | select(.labels.kind == "front-door") | .targets[0]] | sort | join(" ")' <<<"$job")"
check "every probed URL goes through the public (Cloudflare) names, never an origin address" "" \
  "$(jq -r '.scrape_configs[0].static_configs[].targets[] | select(test("^https://(kinowo\\.net|showtimes\\.cc)/") | not)' <<<"$job")"
check "the discovered film targets are read from the file the discovery writes" "/var/lib/synthetic-probes/targets.json" \
  "$(jq -r '.scrape_configs[0].file_sd_configs[0].files[0]' <<<"$job")"
check "the URL becomes the probe target AND the instance label; the scrape goes to the exporter" \
  "__param_target __param_target>instance 127.0.0.1:9115" \
  "$(jq -r '.scrape_configs[0].relabel_configs | "\(.[0].target_label) \(.[1].source_labels[0])>\(.[1].target_label) \(.[2].replacement)"' <<<"$job")"

discovery_env="$(nix "${nix_flags[@]}" eval --json \
  "$infra#nixosConfigurations.monitoring-1.config.systemd.services.synthetic-probe-targets.environment" 2>/dev/null)"
check "the discovery reads a refused page from k3s-worker-1's origin" "10.20.0.12" \
  "$(jq -r '.PROBE_ORIGIN_ADDRESS' <<<"$discovery_env")"
check "...pinned to both zones' origin certificates" "kinowo.net.crt showtimes.cc.crt" \
  "$(nix "${nix_flags[@]}" eval --json \
    "$infra#nixosConfigurations.monitoring-1.config.fleet.syntheticProbes.discoveryOrigin.certificates" 2>/dev/null |
    jq -r 'map(split("/") | last) | sort | join(" ")')"
check "...bundled into the one file curl is told to trust" "synthetic-probe-origin-certificates.pem" \
  "$(jq -r '.PROBE_ORIGIN_CA | split("-") | .[1:] | join("-")' <<<"$discovery_env")"

units="$(nix "${nix_flags[@]}" eval --json "$infra#nixosConfigurations.monitoring-1.config.fleet.autoApply.restartableUnits" 2>/dev/null)"
check "auto-apply may restart the exporter and the discovery once they exist" "3" \
  "$(jq '[.[] | select(. == "prometheus-blackbox-exporter.service" or . == "synthetic-probe-targets.service" or . == "synthetic-probe-targets.timer")] | length' <<<"$units")"

# promtool from PATH, else from the fleet's pinned nixpkgs -- the same resolution, and the same
# refusal to skip, as test_alert_rules.sh.
if ! command -v promtool >/dev/null 2>&1; then
  promtool_pkg="$(nix "${nix_flags[@]}" build --no-link --print-out-paths --inputs-from "$infra" 'nixpkgs#prometheus.cli' 2>/dev/null)"
  PATH="$promtool_pkg/bin:$PATH"
fi
printf 'scrape_config_files: ["%s"]\n' "$tmp/blackbox-targets.yaml" > "$tmp/prometheus.yaml"
check "promtool accepts the job" "ok" \
  "$(promtool check config --syntax-only "$tmp/prometheus.yaml" >/dev/null 2>&1 && echo ok || promtool check config --syntax-only "$tmp/prometheus.yaml" 2>&1 | tail -2)"

spec_summary
