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
#
# THE BINARY IS RESOLVED NOW, NOT WHEN THE SERVER IS STARTED. `nix shell` evaluates before it
# execs, and on a cold store that is slow -- so a `caddy_cmd` with nix still in it would spend the
# readiness window below fetching rather than listening, and every case in this file would then
# answer 000 with an EMPTY caddy.log, because nothing had got as far as writing one. That is a
# green laptop and a red CI, and it is what happened on 2026-09-07.
if caddy_bin="$(type -P caddy)"; then :
else
  echo "==> fetching caddy (not on PATH)"
  caddy_bin="$(nix "${nix_flags[@]}" build --no-link --print-out-paths 'nixpkgs#caddy' 2>/dev/null)/bin/caddy"
fi
if [ ! -x "$caddy_bin" ]; then
  echo "  FAILED could not find a caddy binary to run"; exit 1
fi
caddy_cmd=("$caddy_bin")

work="$(mktemp -d)"
trap 'kill %1 2>/dev/null; wait 2>/dev/null; rm -rf "$work"' EXIT

# EVERY HOST THAT SERVES A PUBLIC VHOST MUST LET AUTO-APPLY RELOAD CADDY. Without it any vhost edit
# changes caddy.service, and the applier refuses the WHOLE closure silently -- which is how the
# one-year HSTS change sat unapplied on k3s-worker-1 on 2026-09-23.
echo "==> auto-apply may reload caddy on every public-proxy host"
for host in k3s-worker-1 monitoring-1; do
  units="$(nix "${nix_flags[@]}" eval --json "$infra/nix#nixosConfigurations.$host.config.fleet.autoApply.reloadableUnits" 2>/dev/null)"
  case "$units" in
    *'"caddy.service"'*) echo "  ok  $host may reload caddy.service" ;;
    *) echo "  FAILED $host's reloadableUnits ($units) lacks caddy.service, so every vhost change is refused"; failed=1 ;;
  esac
done

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

# THE SHARE CARDS ARE SERVED OFF A HOST DIRECTORY, so the rendered path is swapped for a scratch one
# holding fixtures. Read off the config rather than spelled here, so a moved directory is followed.
# And EVERY vhost on the product host must serve them, the redirecting www. names included, because
# og:image carries whichever host the page was reached on.
echo "==> every product vhost serves the share cards"
share_dirs="$(nix "${nix_flags[@]}" eval --json "$infra/nix#nixosConfigurations.k3s-worker-1.config.fleet.publicProxy.vhosts" \
  --apply 'vs: builtins.mapAttrs (_: v: v.shareCardsDir) vs' 2>"$work/eval.err")"
share_dir="$(printf '%s' "$share_dirs" | python3 -c 'import json,sys; print(json.load(sys.stdin)["showtimes.cc"] or "")')"
missing="$(printf '%s' "$share_dirs" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(" ".join(sorted(k for k,v in d.items() if v != d["showtimes.cc"] or not v)))')"
if [ -n "$share_dir" ] && [ -z "$missing" ]; then echo "  ok  all of $(printf '%s' "$share_dirs" | python3 -c 'import json,sys; print(", ".join(sorted(json.load(sys.stdin))))') serve $share_dir"
else echo "  FAILED share cards are not served on: ${missing:-every vhost} ($share_dirs)"; failed=1; fi
cards="$work/share-cards"
vhost="${vhost//$share_dir/$cards}"

echo "==> rendering www.showtimes.cc's vhost out of k3s-worker-1"
www_port=8897
www_vhost="$(nix "${nix_flags[@]}" eval --raw \
  "$infra/nix#nixosConfigurations.k3s-worker-1.config.services.caddy.virtualHosts.\"www.showtimes.cc\".extraConfig" 2>"$work/eval.err")"
if [ -z "$www_vhost" ]; then
  echo "  FAILED could not evaluate the vhost:"; sed 's/^/    /' "$work/eval.err" | tail -5; exit 1
fi
www_vhost="$(printf '%s\n' "$www_vhost" | grep -v '^[[:space:]]*tls[[:space:]]')"
www_vhost="${www_vhost//$share_dir/$cards}"

# THE FIXTURES: two real cards, and one of everything that sits beside them and must NOT be served --
# a card still being written, the worker's poster cache, a dot-file, a file one level too shallow,
# and a "secret" OUTSIDE the root for the traversal cases to aim at.
mkdir -p "$cards/pl/.posters" "$cards/uk"
printf 'CARD-PL' > "$cards/pl/film1-pl-0123456789abcdef.jpg"
printf 'CARD-UK' > "$cards/uk/film2-en-fedcba9876543210.jpg"
printf 'PARTIAL' > "$cards/pl/film3-pl-0000000000000000.jpg.tmp"
printf 'POSTER'  > "$cards/pl/.posters/poster.jpg"
printf 'HIDDEN'  > "$cards/pl/.hidden.jpg"
printf 'SHALLOW' > "$cards/shallow.jpg"
printf 'SECRET'  > "$work/secret.jpg"

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

# THE STAND-IN FOR oauth2-proxy, AS A SITE IN THE SAME FILE RATHER THAN A SECOND PROCESS.
#
# It plays both halves of what `forward_auth` consumes: 202 when the request carries
# `X-Test-Session`, 401 when it does not.
#
# ONE PROCESS, because a second server here first appeared as a CI-only failure where EVERY case in
# this file answered 000 -- the throttle ones included, which have nothing to do with the login --
# with an empty `caddy.log`, i.e. nothing had started. The precise mechanism was never pinned down:
# it did not reproduce on a laptop even with caddy off PATH, which points at the cold nix store a
# runner has and a warm one not being the same thing at all. So this does not rely on having
# diagnosed it. There is one server, its binary is resolved before the clock starts (see the top of
# the file), and if it is not listening the run says SO rather than blaming twenty-five rules.
stub_port=4180
stub_site='
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
'

# `auto_https off` and a plain port, because the rule under test is about matching, not TLS -- and
# a test that had to obtain a certificate could not run offline.
{ echo "{ auto_https off"; echo "  admin off"; echo "}"
  echo ":$port {"; echo "$vhost"; echo "}"
  echo ":$logs_port {"; echo "$logs_vhost"; echo "}"
  echo ":$www_port {"; echo "$www_vhost"; echo "}"
  echo ":$stub_port {"; echo "$stub_site"; echo "}"; } > "$work/Caddyfile"

"${caddy_cmd[@]}" run --config "$work/Caddyfile" --adapter caddyfile >"$work/caddy.log" 2>&1 &
# AND IF IT NEVER COMES UP, SAY THAT AND STOP. Without this the run continues into every case in
# the file, each reporting `000`, and the output then describes twenty-five broken rules rather
# than one server that is not listening -- which is slower to read and much slower to believe. The
# window is generous because it costs nothing when the server is healthy: the loop exits on the
# first successful connection.
serving=0
for _ in $(seq 1 150); do
  if curl -s -o /dev/null "http://127.0.0.1:$port/"; then serving=1; break; fi
  sleep 0.2
done
if [ "$serving" != 1 ]; then
  echo "  FAILED caddy never listened on 127.0.0.1:$port, so nothing below was actually tested."
  echo "         caddy log:"; sed 's/^/           /' "$work/caddy.log" | tail -20
  echo "         config:";    sed 's/^/           /' "$work/Caddyfile" | head -20
  exit 1
fi

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
check 502 "$META"    "/us/florence/"                      "the city listing itself is content, not a facet"
check 502 "$META"    "/us/sitemap.xml"                    "the crawl map stays open"
check 502 "$HUMAN"   "/us/florence/movies?cast=Tom+Hanks" "a PERSON using the filter UI is never throttled"
check 502 "$PREVIEW" "/us/florence/movies"                "the share-preview agent is a different agent and stays open"

# THE SHARE CARDS, WHICH A BULK CRAWLER CAN TURN INTO AN OOM KILL.
#
# Every og-image miss fetches and decodes a poster in the JVM, and a progressive JPEG's decode holds
# native memory outside every JVM cap. AhrefsBot sent 612 og-image requests to kinowo.net on
# 2026-09-21 17:01-18:25Z (against ~10 on a normal evening) and web-pl was OOM-killed seven times.
# So the bulk crawlers are 429'd on every og-image path -- and the SHARE-PREVIEW agents, the only
# reason the endpoint exists, must never be, or every link card on every messenger goes blank.
AHREFS='Mozilla/5.0 (compatible; AhrefsBot/7.0; +http://ahrefs.com/robot/)'
SEMRUSH='Mozilla/5.0 (compatible; SemrushBot/7~bl; +http://www.semrush.com/bot.html)'
echo "==> who may fetch the share cards"
check 429 "$AHREFS"  "/poznan/movie/og-image?title=Diuna" "a bulk SEO crawler is throttled on the film share card"
check 429 "$AHREFS"  "/us/florence/movie/og-image?title=Dune" "...on every mounted country"
check 429 "$AHREFS"  "/poznan/og-image"                   "...and on the city share card"
check 429 "$AHREFS"  "/poznan/film/og-image?title=Diuna"  "...and on the pre-rename /film/og-image address"
check 429 "$SEMRUSH" "/poznan/movie/og-image?title=Diuna" "...and so is SemrushBot"
check 429 "$META"    "/poznan/movie/og-image?title=Diuna" "...and Meta's AI crawler, which is NOT its share-preview agent"
check 502 "$AHREFS"  "/poznan/movie/diuna"                "the FILM PAGE stays open to the same crawler"
check 502 "$AHREFS"  "/poznan/"                           "...and so does the city page"
for preview in \
  'facebookexternalhit/1.1 (+http://www.facebook.com/externalhit_uatext.php)' \
  'Twitterbot/1.0' \
  'Slackbot-LinkExpanding 1.0 (+https://api.slack.com/robots)' \
  'WhatsApp/2.23.20.0' \
  'LinkedInBot/1.0 (compatible; Mozilla/5.0; Apache-HttpClient +http://www.linkedin.com)' \
  'Mozilla/5.0 (compatible; Discordbot/2.0; +https://discordapp.com)' \
  'TelegramBot (like TwitterBot)' \
  'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_1) AppleWebKit/601.2.4 (KHTML, like Gecko) Version/9.0.1 Safari/601.2.4 facebookexternalhit/1.1 Facebot Twitterbot/1.0'; do
  check 502 "$preview" "/poznan/movie/og-image?title=Diuna" "a share preview renders: ${preview%% *}"
done
check 502 "$HUMAN"   "/poznan/movie/og-image?title=Diuna" "a person opening the card directly is never throttled"

# THE SHARE CARDS, SERVED OFF DISK. Each case is a way the route could be quietly wrong: headers
# missing (a card re-fetched on every share), a 404 cached for a year, a half-written `.tmp` or the
# poster cache published, a directory listed, a path escaping the root, or the prefix shadowed by a
# country mount. `--path-as-is` so curl sends the traversal paths unnormalised.
card() { # <expected status> <port> <path> <what it proves> [<expected body>]
  local want="$1" p="$2" path="$3" why="$4" body="${5:-}" got
  got="$(curl -s --path-as-is -o "$work/card.body" -w '%{http_code}' -A "$HUMAN" "http://127.0.0.1:$p$path")"
  if [ "$got" = "$want" ] && { [ -z "$body" ] || [ "$(cat "$work/card.body")" = "$body" ]; }; then printf '  ok  %s\n' "$why"
  else printf '  FAILED %s\n         %s -> %s "%s", wanted %s %s\n' "$why" "$path" "$got" "$(head -c 40 "$work/card.body")" "$want" "$body"; failed=1; fi
  if grep -q SECRET "$work/card.body"; then printf '  FAILED %s escaped the share-card root\n' "$path"; failed=1; fi
}
header_of() { # <port> <path> <header>
  curl -s --path-as-is -o /dev/null -D - -A "$HUMAN" "http://127.0.0.1:$1$2" | tr -d '\r' | awk -F': ' -v h="$3" 'tolower($1)==tolower(h){print $2}'
}
echo "==> the share cards, off disk"
card 200 "$port" "/share-cards/pl/film1-pl-0123456789abcdef.jpg" "a card that exists is served" CARD-PL
card 200 "$port" "/share-cards/uk/film2-en-fedcba9876543210.jpg" "...for a path-mounted country too: /uk does not shadow /share-cards/uk" CARD-UK
card 200 "$www_port" "/share-cards/pl/film1-pl-0123456789abcdef.jpg" "...and on the www. vhost, which redirects everything ELSE" CARD-PL
cc="$(header_of "$port" "/share-cards/pl/film1-pl-0123456789abcdef.jpg" Cache-Control)"
if [ "$cc" = "public, max-age=31536000, immutable" ]; then echo "  ok  the card is cached for a year as immutable (its name is its content hash)"
else echo "  FAILED Cache-Control was '$cc'"; failed=1; fi
ct="$(header_of "$port" "/share-cards/pl/film1-pl-0123456789abcdef.jpg" Content-Type)"
if [ "$ct" = "image/jpeg" ]; then echo "  ok  ...as image/jpeg"
else echo "  FAILED Content-Type was '$ct', wanted image/jpeg"; failed=1; fi
card 404 "$port" "/share-cards/pl/film9-pl-aaaaaaaaaaaaaaaa.jpg" "a missing card is a 404, not the app"
cc="$(header_of "$port" "/share-cards/pl/film9-pl-aaaaaaaaaaaaaaaa.jpg" Cache-Control)"
case "$cc" in *immutable*) echo "  FAILED a 404 is cached as immutable ('$cc'), so a card written later stays missing"; failed=1 ;;
  *) echo "  ok  ...and the 404 is not cached as immutable" ;; esac
card 404 "$port" "/share-cards/pl/"   "the country directory is never listed"
card 404 "$port" "/share-cards/pl"    "...with or without its slash"
card 404 "$port" "/share-cards/"      "...nor the root"
card 404 "$port" "/share-cards/pl/film3-pl-0000000000000000.jpg.tmp" "a card still being written (.tmp) is not served"
card 404 "$port" "/share-cards/pl/.posters/poster.jpg" "the worker's poster cache (.posters/) is not served"
card 404 "$port" "/share-cards/pl/.hidden.jpg" "...nor any dot-file"
card 404 "$port" "/share-cards/shallow.jpg" "only <cc>/<file> is a card; a file one level up is not"
# TRAVERSAL. Caddy's path matcher decodes and cleans the path BEFORE matching, so every spelling of
# `..` that climbs out of the directory resolves to a path outside `/share-cards/*` and goes to the
# app (502 here) -- it never reaches `file_server` at all. The SECRET check in `card` is the part
# that matters: whatever answers, it is never the file outside the root.
card 502 "$port" "/share-cards/pl/../../secret.jpg" "a literal ../ is cleaned to /secret.jpg before matching, so it never reaches the disk"
card 502 "$port" "/share-cards/pl/..%2f..%2fsecret.jpg" "...and so is an encoded ../"
card 502 "$port" "/share-cards/pl/%2e%2e/%2e%2e/secret.jpg" "...and an encoded .. segment"
card 404 "$port" "/share-cards/pl/../uk/film9-en-aaaaaaaaaaaaaaaa.jpg" "a .. that stays inside /share-cards is cleaned and still only finds cards"
card 502 "$port" "/uk/share-cards/uk/film2-en-fedcba9876543210.jpg" "/uk/share-cards/... is the UK app's path, not the disk's"
card 301 "$www_port" "/poznan/" "the www. vhost still redirects everything that is not a card"

# HSTS: one year, still without preload or includeSubDomains (both one-way doors -- see the vhost).
echo "==> what HSTS the vhost promises"
hsts="$(curl -s -o /dev/null -D - -A "$HUMAN" "http://127.0.0.1:$port/us/florence/" | tr -d '\r' | awk -F': ' 'tolower($1)=="strict-transport-security"{print $2}')"
if [ "$hsts" = "max-age=31536000" ]; then echo "  ok  browsers are told to stay on https for a year"
else echo "  FAILED Strict-Transport-Security was '$hsts', wanted max-age=31536000"; failed=1; fi

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
