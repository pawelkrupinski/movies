#!/usr/bin/env bash
#
# Generate the per-language FEATURE GRAPHIC (1024×500) for the Play listing, and
# fan the one shared APP ICON out to every locale.
#
#   android/scripts/store-graphics.sh              # every locale
#   android/scripts/store-graphics.sh de-DE        # just one
#
# Play keys both assets per LANGUAGE, but only one of them actually differs by
# language. The icon is identical everywhere, so it lives ONCE at
# android/store/icon/1.png and is copied into each
# listings/<locale>/graphics/icon/ at generation time — the copies are gitignored
# derivatives, the master is the tracked original. Keeping three byte-identical
# icons under version control was the alternative, and they drift.
#
# The feature graphic DOES differ: it carries the brand name and the tagline, both
# of which are per-language, plus that locale's own first screenshot — so the
# German card shows German UI. Copy comes from the listing files themselves
# (title.txt before the colon, short-description.txt), so the card can never
# disagree with the listing it sits above.
#
# Rendered with headless Chrome rather than an image library: the layout is CSS,
# which is far easier to adjust than hand-placed rectangles, and Chrome is already
# a dependency of the page tests.
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
LISTINGS="$REPO_ROOT/android/app/src/main/play/listings"
# Deliberately OUTSIDE app/src/main/play: gradle-play-publisher validates that
# whole tree and fails the build on any path it does not recognise ("Unknown Play
# resource file"), so the shared master cannot live inside it. android/store/
# mirrors ios/store/ instead.
SHARED_ICON="$REPO_ROOT/android/store/icon/1.png"
# shellcheck source=../../scripts/store-screenshots-common.sh
source "$REPO_ROOT/scripts/store-screenshots-common.sh"

CHROME="${CHROME:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"
WIDTH=1024
HEIGHT=500

# Play's exact slot size. Anything else is rejected at upload, so it is checked
# after rendering rather than trusted.
FEATURE_W=1024
FEATURE_H=500

# App palette, straight from ui/theme/Theme.kt so the card matches the product.
BG="#0B0B12"; CARD="#1E1E2E"; ELEVATED="#2A2A3E"
# Rating-badge colours, straight from Theme.kt so the pills are the app's, not an
# approximation of them. RtFresh really is the red one (>=60) and RtRotten green —
# that is the app's mapping, not a mistake here.
IMDB="#F5C518"; META_GOOD="#66CC66"; RT_FRESH="#FA320A"
FW_ORANGE="#FF6C00"; FW_LIGHT="#FF9C4A"

# The brand is whatever the listing title says before the colon — "Kinowo" in
# Poland, "Showtimes" everywhere else (see Country.brandName). Deriving it beats a
# second copy of that rule here.
brand_of()   { cut -d: -f1 < "$LISTINGS/$1/title.txt" | tr -d '\n'; }
# The one-line tagline under the wordmark. NOT short-description.txt: that is the
# store blurb, which in English and German spends its 80 characters listing rating
# sources and reads as a paragraph on a card. These are the same three strings the
# web share-cards use — OgCardGenerator.homeTagline — so the card, the OG image and
# the site all say the same thing in each language. The test greps that Scala file
# so the two copies cannot drift apart.
tagline_of() {
  case "$1" in
    pl-PL) echo "Repertuar kin w Twoim mieście";;
    de-DE) echo "Kinoprogramm in deiner Stadt";;
    es-ES) echo "Cartelera de cine en tu ciudad";;
    *)     echo "Cinema listings in your city";;
  esac
}

# Base64 data URI for an image, so the HTML is self-contained and Chrome needs no
# file-access flags.
data_uri() { # $1 png path
  printf 'data:image/png;base64,%s' "$(base64 < "$1" | tr -d '\n')"
}

feature_html() { # $1 locale, $2 icon data-uri, $3 screenshot data-uri (may be empty)
  local locale="$1" icon="$2" shot="$3"
  local brand tagline
  brand="$(brand_of "$locale")"
  tagline="$(tagline_of "$locale")"
  cat <<HTML
<!doctype html><meta charset="utf-8">
<style>
  * { margin:0; padding:0; box-sizing:border-box; }
  html, body { width:${WIDTH}px; height:${HEIGHT}px; overflow:hidden; }
  body {
    background:
      radial-gradient(900px 420px at 78% 50%, rgba(107,171,222,.20), transparent 70%),
      radial-gradient(600px 400px at 8% 20%, rgba(170,212,255,.10), transparent 65%),
      $BG;
    font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
    color:#fff; display:flex; align-items:center; gap:44px; padding:0 56px;
  }
  .copy { flex:1 1 auto; min-width:0; }
  .row { display:flex; align-items:center; gap:18px; margin-bottom:22px; }
  .icon { width:92px; height:92px; border-radius:21px; box-shadow:0 10px 30px rgba(0,0,0,.55); }
  h1 { font-size:62px; font-weight:700; letter-spacing:-1.5px; line-height:1; }
  /* One line, always. The copy column is ~650px wide and the longest tagline
     ("Repertuar kin w Twoim mieście", 29 chars) sets ~435px at this size, so
     nowrap cannot clip — but keep that headroom in mind before lengthening one. */
  p  { font-size:30px; line-height:1.3; color:rgba(255,255,255,.82); white-space:nowrap; }
  /* The app's LabelValuePill, at card scale: a bold label chip on the source's
     colour, then the value on CardElevated in that same colour. Same 3dp-ish
     corner, same label-Black / value-SemiBold weights. */
  .pills { display:flex; gap:9px; margin-top:30px; align-items:center; }
  .pill { display:flex; border-radius:5px; overflow:hidden; font-size:20px; line-height:1; }
  .pill .l { font-weight:900; padding:8px 9px; }
  .pill .v { font-weight:600; padding:8px 9px; background:$ELEVATED; }
  /* Metacritic is the label-less solid one — just the score on its band colour. */
  .pill.solid { font-weight:900; padding:8px 10px; color:#0B0B12; background:$META_GOOD; }
  .imdb .l { background:$IMDB; color:#000; }   .imdb .v { color:$IMDB; }
  .rt   .l { background:$RT_FRESH; color:#fff; } .rt   .v { color:$RT_FRESH; }
  .fw   .l { background:$FW_ORANGE; color:#fff; } .fw .v { color:$FW_LIGHT; }
  /* The locale's OWN first screenshot: the German card must show German UI. */
  .phone {
    flex:0 0 auto; width:216px; height:396px; border-radius:28px; overflow:hidden;
    border:5px solid $CARD; box-shadow:0 26px 60px rgba(0,0,0,.6);
    transform: rotate(3deg);
  }
  .phone img { width:100%; height:100%; object-fit:cover; object-position:top center; display:block; }
</style>
<div class="copy">
  <div class="row">
    <img class="icon" src="$icon">
    <h1>$brand</h1>
  </div>
  <p>$tagline</p>
  <!-- Real ratings rather than bare source names: this is how a film actually
       reads in the app, and it is the set the old Polish card carried. -->
  <div class="pills">
    <span class="pill imdb"><span class="l">IMDb</span><span class="v">7.9</span></span>
    <span class="pill solid">81</span>
    <span class="pill rt"><span class="l">RT</span><span class="v">91%</span></span>
    <span class="pill fw"><span class="l">FW</span><span class="v">7.4</span></span>
  </div>
</div>
$( [ -n "$shot" ] && printf '<div class="phone"><img src="%s"></div>' "$shot" )
HTML
}

# Chrome's new headless mode does not reliably EXIT after --screenshot: it writes
# the file and then sits at 0% CPU forever, which hung this script after the first
# card. So run it in the background, wait for the PNG to appear, and kill it — the
# artefact is the completion signal, not the process.
render() { # $1 html file, $2 out png
  local tmpdir pid t=0
  tmpdir="$(mktemp -d)"
  rm -f "$2"
  "$CHROME" --headless --disable-gpu --hide-scrollbars --no-first-run \
    --no-default-browser-check --disable-extensions \
    --force-device-scale-factor=1 --window-size="$WIDTH,$HEIGHT" \
    --user-data-dir="$tmpdir" --screenshot="$2" "file://$1" >/dev/null 2>&1 &
  pid=$!
  while [ ! -s "$2" ] && [ "$t" -lt 90 ]; do naps 1; t=$((t + 1)); done
  naps 1                                   # let the last bytes land before killing
  kill "$pid" 2>/dev/null || true; wait "$pid" 2>/dev/null || true
  rm -rf "$tmpdir"
  [ -s "$2" ] || die "Chrome produced no image for $2 after ${t}s"
}

generate_locale() { # $1 locale
  local locale="$1"
  [ -d "$LISTINGS/$locale" ] || die "no listing dir for '$locale'"
  local gfx="$LISTINGS/$locale/graphics"

  # The icon: one master, copied out. Derived, gitignored.
  mkdir -p "$gfx/icon"
  cp "$SHARED_ICON" "$gfx/icon/1.png"

  # The card's phone uses this locale's own first published screenshot, so each
  # language advertises its own UI. Missing one is not fatal — the card just runs
  # copy-only rather than blocking the whole generation.
  # Downscaled first: the card shows it 236px wide, and base64-ing a 3MB PNG into
  # the HTML made Chrome crawl on a loaded machine for no visible gain.
  local shot="" shot_src="$gfx/phone-screenshots/001.png"
  if [ -f "$shot_src" ]; then
    local small; small="$(mktemp -t shot-XXXX).png"
    sips -Z 560 "$shot_src" --out "$small" >/dev/null 2>&1
    shot="$(data_uri "$small")"; rm -f "$small"
  else warn "$locale: no 001.png yet — rendering the card without a phone"
  fi

  local html; html="$(mktemp -t feature-XXXX).html"
  feature_html "$locale" "$(data_uri "$SHARED_ICON")" "$shot" > "$html"

  mkdir -p "$gfx/feature-graphic"
  local out="$gfx/feature-graphic/1.png"
  step "$locale feature graphic"
    render "$html" "$out"
    rm -f "$html"
    local w h
    IFS=x read -r w h <<< "$(png_size "$out")"
    [ "$w" = "$FEATURE_W" ] && [ "$h" = "$FEATURE_H" ] ||
      die "$locale card is ${w}×${h}; Play requires exactly ${FEATURE_W}×${FEATURE_H}"
  done_
  ok "$locale → $out (${w}×${h}, $(( $(stat -f%z "$out") / 1024 ))KB)"
}

# Dispatch only when executed — store-graphics-test.sh sources this file to check
# the pure helpers, and must not start rendering by doing so.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  [ -f "$SHARED_ICON" ] || die "no shared icon at $SHARED_ICON"
  [ -x "$CHROME" ] || die "no Chrome at $CHROME — set CHROME=<path>"

  if [ $# -gt 0 ]; then
    for locale in "$@"; do generate_locale "$locale"; done
  else
    say "feature graphics + icon for every locale"
    for country in $COUNTRIES; do generate_locale "$(country_locale "$country")"; done
  fi
fi
