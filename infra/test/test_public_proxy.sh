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
#
# THE SAME TRICK COVERS logs.kinowo.net ON monitoring-1, whose rule has its own quiet failures:
# the login could be missing (a reload with an unreadable hash file does not fail, it just lets
# nobody in -- or, worse, a directive order change lets everybody in), or the vhost could publish
# more of VictoriaLogs than `/select` -- and `/insert`, `/delete` are one prefix away. 502 again
# means "authenticated and proxied"; 401 and 404 are the two answers the rule exists to give.
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
trap 'kill %1 %2 2>/dev/null; wait 2>/dev/null; rm -rf "$work"' EXIT

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

echo "==> rendering logs.kinowo.net's vhost out of monitoring-1"
logs_port=8898
logs_vhost="$(nix "${nix_flags[@]}" eval --raw \
  "$infra/nix#nixosConfigurations.monitoring-1.config.services.caddy.virtualHosts.\"logs.kinowo.net\".extraConfig" 2>"$work/eval.err")"
if [ -z "$logs_vhost" ]; then
  echo "  FAILED could not evaluate the vhost:"; sed 's/^/    /' "$work/eval.err" | tail -5; exit 1
fi

# THE DOOR IS GOOGLE'S, NOT A SHARED PASSWORD. Asserted on the rendered vhost so that a change
# which quietly drops the check -- or reverts it to `basic_auth` -- fails here rather than being
# noticed by nobody.
case "$logs_vhost" in
  *"forward_auth"*) echo "  ok  the vhost asks a sign-in proxy who the visitor is" ;;
  *) echo "  FAILED expected a forward_auth in logs.kinowo.net's vhost"; failed=1 ;;
esac
case "$logs_vhost" in
  *"basic_auth"*) echo "  FAILED logs.kinowo.net still carries a shared password"; failed=1 ;;
  *) echo "  ok  ...and no shared password is left beside it" ;;
esac

# `auto_https off` and a plain port, because the rule under test is about matching, not TLS -- and
# a test that had to obtain a certificate could not run offline.
{ echo "{ auto_https off"; echo "  admin off"; echo "}"
  echo ":$port {"; echo "$vhost"; echo "}"
  echo ":$logs_port {"; echo "$logs_vhost"; echo "}"; } > "$work/Caddyfile"

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

logs_check() { # <expected status> <curl auth args or -> <path> <what it proves>
  local want="$1" auth="$2" path="$3" why="$4" got
  if [ "$auth" = "-" ]; then got="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$logs_port$path")"
  else got="$(curl -s -o /dev/null -w '%{http_code}' -u "$auth" "http://127.0.0.1:$logs_port$path")"; fi
  if [ "$got" = "$want" ]; then printf '  ok  %s\n' "$why"
  else printf '  FAILED %s\n         %s -> %s, wanted %s\n' "$why" "$path" "$got" "$want"; failed=1; fi
}

# ------------------------------------------------------------------------------------------------
# THE GOOGLE DOOR, AND THE TWO WAYS IT GOES WRONG WITHOUT LOOKING WRONG
# ------------------------------------------------------------------------------------------------
#
# oauth2-proxy is STOOD IN FOR rather than run, because what is under test is Caddy's arrangement of
# the check and not Google's answer to it. The stub plays both parts a session can be in: it says
# 202 when the request carries `X-Test-Session`, and 401 when it does not, which is exactly the
# contract `forward_auth` consumes. That is enough to tell apart:
#
#   1. THE LOOP. If the check also covered `/oauth2/*`, the redirect it issues would point at a
#      path that is itself redirected, and a browser would bounce between the door and the doorbell
#      until it gave up. The `route` wrapper in roles/public-proxy.nix is what prevents that, and
#      it CANNOT be seen in the rendered config -- it is a property of Caddy's directive order.
#   2. THE DEAD END. A bare 401 is a correct answer and a useless one for a person; the point is to
#      be sent to Google with the page you wanted preserved, so you arrive back at it.
#
# And with a session in hand, the path restrictions must still hold: signing in buys READING the
# logs, never writing or erasing them.
cat > "$work/StubAuth" <<STUB
{
	auto_https off
	admin off
}
:4180 {
	handle /oauth2/auth {
		@signedIn header X-Test-Session yes
		handle @signedIn {
			respond 202
		}
		respond 401
	}
	handle /oauth2/* {
		respond "OAUTH2PROXY-PAGE" 200
	}
}
STUB
"${caddy_cmd[@]}" run --config "$work/StubAuth" --adapter caddyfile >"$work/stub.log" 2>&1 &
for _ in $(seq 1 50); do
  curl -s -o /dev/null "http://127.0.0.1:4180/oauth2/x" && break
  sleep 0.2
done

logs_check() { # <expected status> <"in"|"out"> <path> <what it proves>
  local want="$1" session="$2" path="$3" why="$4" got
  if [ "$session" = "in" ]; then
    got="$(curl -s -o /dev/null -H "X-Test-Session: yes" -w '%{http_code}' "http://127.0.0.1:$logs_port$path")"
  else
    got="$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$logs_port$path")"
  fi
  if [ "$got" = "$want" ]; then printf '  ok  %s\n' "$why"
  else printf '  FAILED %s\n         %s -> %s, wanted %s\n' "$why" "$path" "$got" "$want"; failed=1; fi
}

echo "==> with no Google session"
logs_check 302 out "/select/vmui/" "the UI is not served, you are sent to sign in"
logs_check 302 out "/"             "...and so is the bare root"
logs_check 302 out "/insert/jsonline" "an unpublished path asks for a login FIRST, so it cannot be enumerated"

loc="$(curl -s -o /dev/null -D - "http://127.0.0.1:$logs_port/select/vmui/" | tr -d '\r' | awk -F': ' '/^[Ll]ocation/{print $2}')"
case "$loc" in
  */oauth2/start*rd=*) echo "  ok  the redirect carries the page you asked for, so you land back on it" ;;
  *) echo "  FAILED sign-in redirect was '$loc', wanted /oauth2/start with an rd="; failed=1 ;;
esac

echo "==> the sign-in flow is NOT itself behind the check"
logs_check 200 out "/oauth2/start"    "the door does not require having gone through the door"
logs_check 200 out "/oauth2/callback" "...and neither does the callback Google returns to"
if [ "$(curl -s --max-time 5 "http://127.0.0.1:$logs_port/oauth2/start")" = "OAUTH2PROXY-PAGE" ]; then
  echo "  ok  /oauth2/* reaches the sign-in proxy, not the store behind it"
else
  echo "  FAILED /oauth2/start did not reach the proxy"; failed=1
fi

echo "==> signed in, what the session actually buys"
logs_check 502 in "/select/vmui/"             "the UI is reached (502: no store on this laptop)"
logs_check 502 in "/select/logsql/query?query=*" "...and so is the query API the UI calls"
logs_check 404 in "/insert/jsonline"          "ingest is NOT published, even signed in"
logs_check 404 in "/delete/run_task"          "deletion is NOT published, even signed in"
logs_check 404 in "/metrics"                  "the store's own metrics stay private"
logs_check 404 in "/internal/force_flush"     "...and its internal endpoints"

echo "==> where the bare paths go, once you are through the door"
loc="$(curl -s -o /dev/null -D - -H "X-Test-Session: yes" "http://127.0.0.1:$logs_port/" | tr -d '\r' | awk -F': ' '/^[Ll]ocation/{print $2}')"
if [ "$loc" = "/select/vmui/" ]; then echo "  ok  / redirects to the UI"
else echo "  FAILED / redirected to '$loc', wanted /select/vmui/"; failed=1; fi
loc="$(curl -s -o /dev/null -D - -H "X-Test-Session: yes" "http://127.0.0.1:$logs_port/select" | tr -d '\r' | awk -F': ' '/^[Ll]ocation/{print $2}')"
if [ "$loc" = "/select/" ]; then echo "  ok  /select without its slash is redirected, not 404'd"
else echo "  FAILED /select redirected to '$loc', wanted /select/"; failed=1; fi

[ "$failed" = 0 ] && echo "  ok  public proxy behaves" || { echo; echo "caddy log:"; sed 's/^/    /' "$work/caddy.log" | tail -20; }
exit "$failed"
