#!/usr/bin/env bash
# THE CRAWLER THROTTLE, ASKED WHAT IT ACTUALLY ANSWERS.
#
# `nix eval` (bin/check) proves the module evaluates and the host would build. It cannot tell you
# that the matcher matches the right requests, and this rule has four ways to be quietly wrong, all
# of which read as a working config:
#
#   1. It could throttle the FILM PAGES. `/{city}/movie/{slug}` is one character from
#      `/{city}/movies` and is the content we want crawled. A regex missing its `$` takes both.
#   2. It could throttle A HUMAN. The listings are a real UI the app serves to people; only the
#      user-agent separates them from the crawler.
#   3. It could throttle THE SHARE-PREVIEW AGENT. `facebookexternalhit` is a different Meta agent
#      on different paths, and breaking it breaks every Facebook/WhatsApp link card.
#   4. It could MISS A COUNTRY. The prefixes are derived from `pathUpstreams`, so this pins that
#      derivation rather than trusting it.
#
# So it renders the real vhost config out of the real host definition, runs THE REAL CADDY against
# it, and makes the requests. The upstreams (127.0.0.1:3091x NodePorts) do not exist on a laptop,
# so anything that reaches `reverse_proxy` answers 502 -- which is exactly the signal wanted: 502
# means "passed the throttle and went to the app", 429 means "the throttle took it".
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"
port=8899
failed=0

nix_flags=(--extra-experimental-features 'nix-command flakes')
if ! command -v nix >/dev/null 2>&1; then
  echo "  FAILED nix is not on PATH, so the public proxy was not checked."
  exit 1
fi

# `type -P` and NOT `command -v`, which also finds shell FUNCTIONS -- so the obvious spelling of
# this helper reports itself as already on PATH and then fails to exec.
if caddy_bin="$(type -P caddy)"; then caddy_cmd=("$caddy_bin")
else caddy_cmd=(nix "${nix_flags[@]}" shell 'nixpkgs#caddy' -c caddy); fi

work="$(mktemp -d)"
trap 'kill %1 2>/dev/null; wait 2>/dev/null; rm -rf "$work"' EXIT

echo "==> rendering showtimes.cc's vhost out of k3s-worker-1"
vhost="$(nix "${nix_flags[@]}" eval --raw \
  "$infra/nix#nixosConfigurations.k3s-worker-1.config.services.caddy.virtualHosts.\"showtimes.cc\".extraConfig" 2>"$work/eval.err")"
if [ -z "$vhost" ]; then
  echo "  FAILED could not evaluate the vhost:"; sed 's/^/    /' "$work/eval.err" | tail -5; exit 1
fi

# THE FIXED CERTIFICATE IS ASSERTED, THEN REMOVED BEFORE SERVING.
#
# Names reached only through Cloudflare pin their own certificate
# (fleet.publicProxy.*.originCertificate). Its cert is in the nix store and its key under
# /run/secrets, so Caddy here could not load either -- and a `tls` directive inside this
# `:port { }` block would also turn the listener HTTPS, which every plain-http assertion below
# would then fail against. Both failure modes look identical from the outside: 000.
#
# So the line is checked for and then dropped. That keeps this file about what it says it is about
# -- which requests match the throttle -- while still failing if the option silently stops emitting
# anything. Whether the right certificate is SERVED is a property of the deployed host: `nix eval`
# in bin/check proves the closure builds, and a bad path fails loudly at caddy start on the switch.
case "$vhost" in
  *"tls "*) echo "  ok  the vhost pins its own certificate" ;;
  *)        echo "  FAILED expected an originCertificate tls line in showtimes.cc's vhost"; failed=1 ;;
esac
vhost="$(printf '%s\n' "$vhost" | grep -v '^[[:space:]]*tls[[:space:]]')"

# `auto_https off` and a plain port, because the rule under test is about matching, not TLS -- and
# a test that had to obtain a certificate could not run offline.
{ echo "{ auto_https off"; echo "  admin off"; echo "}"; echo ":$port {"; echo "$vhost"; echo "}"; } > "$work/Caddyfile"

"${caddy_cmd[@]}" run --config "$work/Caddyfile" --adapter caddyfile >"$work/caddy.log" 2>&1 &
for _ in $(seq 1 50); do
  curl -s -o /dev/null "http://127.0.0.1:$port/" && break
  sleep 0.2
done

META='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36 (compatible; meta-externalagent/1.1 (+https://developers.facebook.com/docs/sharing/webmasters/crawler))'
PREVIEW='facebookexternalhit/1.1 (+http://www.facebook.com/externalhit_uatext.php)'
HUMAN='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15'

check() { # <expected status> <user agent> <path> <what it proves>
  local want="$1" ua="$2" path="$3" why="$4" got
  got="$(curl -s -o /dev/null -w '%{http_code}' -A "$ua" "http://127.0.0.1:$port$path")"
  if [ "$got" = "$want" ]; then printf '  ok  %s\n' "$why"
  else printf '  FAILED %s\n         %s -> %s, wanted %s\n' "$why" "$path" "$got" "$want"; failed=1; fi
}

echo "==> what the throttle answers"
check 429 "$META"    "/us/florence/movies?cast=Tom+Hanks" "the crawler is throttled on a US faceted listing"
check 429 "$META"    "/uk/london/movies"                  "...on every mounted country, not just the one we caught it on"
check 429 "$META"    "/de/berlin/movies"                  "...including Germany"
check 429 "$META"    "/es/madrid/movies"                  "...including Spain"
check 429 "$META"    "/poznan/filmy"                      "...and the Polish spelling on a root-mounted deployment"
check 502 "$META"    "/us/florence/movie/some-film"       "the FILM PAGE is one character away and must reach the app"
check 502 "$META"    "/us/florence/movie/og-image"        "og-image reaches the app, so share cards keep rendering"
check 502 "$META"    "/us/florence/"                      "the city listing itself is content, not a facet"
check 502 "$META"    "/us/sitemap.xml"                    "the crawl map stays open"
check 502 "$HUMAN"   "/us/florence/movies?cast=Tom+Hanks" "a PERSON using the filter UI is never throttled"
check 502 "$PREVIEW" "/us/florence/movies"                "the share-preview agent is a different agent and stays open"

echo "==> the Retry-After a throttled crawler is handed"
retry="$(curl -s -o /dev/null -D - -A "$META" "http://127.0.0.1:$port/us/florence/movies" | tr -d '\r' | awk -F': ' '/^[Rr]etry-[Aa]fter/{print $2}')"
if [ "$retry" = "3600" ]; then echo "  ok  429 carries Retry-After: 3600, which is the half that reduces the RATE"
else echo "  FAILED Retry-After was '$retry', wanted 3600"; failed=1; fi

[ "$failed" = 0 ] && echo "  ok  public proxy behaves" || { echo; echo "caddy log:"; sed 's/^/    /' "$work/caddy.log" | tail -20; }
exit "$failed"
