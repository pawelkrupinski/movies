#!/usr/bin/env bash
# THE SYNTHETIC PROBES, ASKED WHETHER THEY PROBE WHAT THEY SAY (roles/synthetic-probes.nix).
#
# Two halves, because the module fails in two different silent ways:
#
#   discovery   nix/files/synthetic-probe-targets.sh finds a live film page and share card per
#               country. Its cases are nix/files/synthetic_probe_targets_test.py; here only that it
#               finds its posters with exactly the tools its unit is given.
#   the job     the `blackbox` scrape job monitoring-1 would install, read with `nix eval` and
#               handed to `promtool check config`. A job that parses but relabels wrongly probes
#               127.0.0.1:9115 itself, or labels every alert with the exporter's address instead
#               of the URL that failed -- promtool is happy with both, so the shape is asserted.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"
script="$infra/nix/files/synthetic-probe-targets.sh"
. "$here/../../scripts/shell-spec.sh"
. "$here/fleet-flake.sh"
flake="$(fleet_flake "$infra")"

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
# with "grep: write error: Broken pipe". The tools it finds are this shell's, or only those on
# $RUN_TOOLS when it is set.
run() {
  : > "$tmp/curl.log"
  (trap '' PIPE
    PATH="$tmp/bin:${RUN_TOOLS:-$PATH}" STUB_PAGES="$tmp/pages" STUB_LOG="$tmp/curl.log" PROBE_TEXTFILE="$tmp/probes.prom" \
      exec bash "$script" "$@" 2>"$tmp/stderr")
}
field() { jq -r "$1" "$tmp/targets.json"; }

# ── THE SCRAPE JOB monitoring-1 WOULD INSTALL ──────────────────────────────────────────────────
echo "the blackbox job on monitoring-1"
nix_flags=(--extra-experimental-features 'nix-command flakes')
if ! job="$(nix "${nix_flags[@]}" eval --raw \
  "$flake#nixosConfigurations.monitoring-1.config.environment.etc.\"prometheus/scrape.d/blackbox-targets.yaml\".text" \
  2>"$tmp/eval.err")"; then
  check "monitoring-1's blackbox scrape job evaluates" "evaluated" "$(tail -3 "$tmp/eval.err")"
  spec_summary
fi
printf '%s' "$job" > "$tmp/blackbox-targets.yaml"

check "every country's city page is a static target, in the five-minute job" "blackbox-city 5m de es pl uk us" \
  "$(jq -r '.scrape_configs[] | select(.job_name == "blackbox-city") | "\(.job_name) \(.scrape_interval) \([.static_configs[] | select(.labels.kind == "city") | .labels.country] | sort | join(" "))"' <<<"$job")"
check "...and in no other job" "" \
  "$(jq -r '.scrape_configs[] | select(.job_name != "blackbox-city") | .static_configs[] | select(.labels.kind == "city") | .targets[0]' <<<"$job")"
check "both brands' front doors are static targets" "https://kinowo.net/ https://showtimes.cc/" \
  "$(jq -r '[.scrape_configs[0].static_configs[] | select(.labels.kind == "front-door") | .targets[0]] | sort | join(" ")' <<<"$job")"
check "every probed URL of the site goes through the public (Cloudflare) names, never an origin address" "" \
  "$(jq -r '.scrape_configs[].static_configs[] | select(.labels.kind != "control") | .targets[] | select(test("^https://(kinowo\\.net|showtimes\\.cc)/") | not)' <<<"$job")"
check "two controls that are not the site, one off Cloudflare and one on it, probed every minute as assets" \
  "blackbox https://www.cloudflare.com/cdn-cgi/trace http_asset blackbox https://www.google.com/generate_204 http_asset" \
  "$(jq -r '[.scrape_configs[] | .job_name as $j | .static_configs[] | select(.labels.kind == "control") | "\($j) \(.targets[0]) \(.labels.__param_module)"] | sort | join(" ")' <<<"$job")"
check "the discovered film targets are read from the file the discovery writes" "/var/lib/synthetic-probes/targets.json" \
  "$(jq -r '.scrape_configs[0].file_sd_configs[0].files[0]' <<<"$job")"
check "the URL becomes the probe target AND the instance label; the scrape goes to the exporter" \
  "__param_target __param_target>instance 127.0.0.1:9115 __param_target __param_target>instance 127.0.0.1:9115 __param_target __param_target>instance 127.0.0.1:9115" \
  "$(jq -r '[.scrape_configs[].relabel_configs | "\(.[0].target_label) \(.[1].source_labels[0])>\(.[1].target_label) \(.[2].replacement)"] | join(" ")' <<<"$job")"

check "the discovered posters go to the five-minute poster job, and only there" \
  "blackbox:drop:poster blackbox-poster:keep:poster" \
  "$(jq -r '[.scrape_configs[] | select(.file_sd_configs) | "\(.job_name):\(.relabel_configs[3].action):\(.relabel_configs[3].regex)"] | join(" ")' <<<"$job")"
check "...every five minutes" "5m" "$(jq -r '.scrape_configs[] | select(.job_name == "blackbox-poster") | .scrape_interval' <<<"$job")"

discovery_env="$(nix "${nix_flags[@]}" eval --json \
  "$flake#nixosConfigurations.monitoring-1.config.systemd.services.synthetic-probe-targets.environment" 2>/dev/null)"
check "the discovery reads a refused page from k3s-worker-1's origin" "10.20.0.12" \
  "$(jq -r '.PROBE_ORIGIN_ADDRESS' <<<"$discovery_env")"
check "...pinned to both zones' origin certificates" "kinowo.net.crt showtimes.cc.crt" \
  "$(nix "${nix_flags[@]}" eval --json \
    "$flake#nixosConfigurations.monitoring-1.config.fleet.syntheticProbes.discoveryOrigin.certificates" 2>/dev/null |
    jq -r 'map(split("/") | last) | sort | join(" ")')"
check "the discovery is given no list of poster hosts to leave unprobed" "null" \
  "$(jq -r '.PROBE_POSTER_SKIP' <<<"$discovery_env")"
check "...bundled into the one file curl is told to trust" "synthetic-probe-origin-certificates.pem" \
  "$(jq -r '.PROBE_ORIGIN_CA | split("-") | .[1:] | join("-")' <<<"$discovery_env")"

# THE DISCOVERY WITH ITS UNIT'S PATH AND NOTHING ELSE: each package the unit is given, built for this
# machine from the fleet's nixpkgs, and the shell its script is written for -- not systemd, which the
# unit is given but the script never runs, and which no Mac builds.
unit_packages="$(nix "${nix_flags[@]}" eval --json \
  "$flake#nixosConfigurations.monitoring-1.config.systemd.services.synthetic-probe-targets.path" \
  --apply 'map (p: p.pname or (builtins.parseDrvName p.name).name)' 2>/dev/null |
  jq -r '(. + ["bash"]) - ["systemd"] | unique | map("nixpkgs#" + .) | join(" ")')"
# shellcheck disable=SC2086 # one installable per word
unit_path="$(nix "${nix_flags[@]}" build --no-link --print-out-paths --inputs-from "$flake" $unit_packages 2>/dev/null |
  sed 's|$|/bin|' | paste -sd: -)"
# A London page drawing two posters from each of two hosts: a weserv-wrapped savoysystems.co.uk and acsta.net's shards.
wk='https://images.weserv.nl/?url='
hints='&amp;w=480&amp;h=720&amp;fit=cover&amp;output=webp'
serve "https://showtimes.cc/uk/london/" "<a href=\"/uk/london/movie/bad-apples\">x</a>$(
  printf '<img data-original-src="%shttps%%3A%%2F%%2Farthousecrouchend.savoysystems.co.uk%%2Fa.png%s">' "$wk" "$hints"
  printf '<img data-original-src="%shttps%%3A%%2F%%2Fimages.savoysystems.co.uk%%2Fb.png%s">' "$wk" "$hints"
  printf '<img data-original-src="https://de.web.img2.acsta.net/a.jpg"><img data-original-src="https://de.web.img3.acsta.net/b.jpg">')</html>"
serve "https://showtimes.cc/uk/london/movie/bad-apples" \
  '<meta property="og:image" content="https://showtimes.cc/share-cards/uk/city-london.jpg"></html>'
rm -f "$tmp/targets.json"
RUN_TOOLS="$unit_path" PROBE_POSTER_MIN=2 run "$tmp/targets.json" uk=https://showtimes.cc/uk/london/
if [ -f "$tmp/targets.json" ]; then found="$(field '[.[] | select(.labels.kind == "poster") | .labels.poster_host] | unique | join(" ")')"
else found="$(cat "$tmp/stderr")"; fi
check "the discovery finds its posters with only the tools its unit is given" "acsta.net savoysystems.co.uk" "$found"

units="$(nix "${nix_flags[@]}" eval --json "$flake#nixosConfigurations.monitoring-1.config.fleet.autoApply.restartableUnits" 2>/dev/null)"
check "auto-apply may restart the exporter and the discovery once they exist" "3" \
  "$(jq '[.[] | select(. == "prometheus-blackbox-exporter.service" or . == "synthetic-probe-targets.service" or . == "synthetic-probe-targets.timer")] | length' <<<"$units")"

# promtool from PATH, else from the fleet's pinned nixpkgs -- the same resolution, and the same
# refusal to skip, as test_alert_rules.sh.
if ! command -v promtool >/dev/null 2>&1; then
  promtool_pkg="$(nix "${nix_flags[@]}" build --no-link --print-out-paths --inputs-from "$flake" 'nixpkgs#prometheus.cli' 2>/dev/null)"
  PATH="$promtool_pkg/bin:$PATH"
fi
printf 'scrape_config_files: ["%s"]\n' "$tmp/blackbox-targets.yaml" > "$tmp/prometheus.yaml"
check "promtool accepts the job" "ok" \
  "$(promtool check config --syntax-only "$tmp/prometheus.yaml" >/dev/null 2>&1 && echo ok || promtool check config --syntax-only "$tmp/prometheus.yaml" 2>&1 | tail -2)"

spec_summary
