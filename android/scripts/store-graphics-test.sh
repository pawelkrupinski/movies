#!/usr/bin/env bash
#
# Unit tests for the pure helpers in store-graphics.sh — where the copy on the
# feature graphic comes from, and what the card is built out of. The render itself
# needs Chrome and is verified by running it; these are the parts a typo silently
# breaks (an English tagline on the German card, a missing brand, a wrong path).
#
#   android/scripts/store-graphics-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=store-graphics.sh
source "$HERE/store-graphics.sh"             # sourcing must not start a render

fails=0
check() { # $1 what, $2 expected, $3 actual
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

printf '\033[36m▸\033[0m store-graphics helpers\n'

# ── the copy comes from the listing, never a second hardcoded rule ────────────
# Brand is the title before the colon — "Kinowo" in Poland, "Showtimes" elsewhere
# (Country.brandName). Deriving it means the card cannot disagree with the listing
# it sits above, which a duplicated per-locale table would eventually do.
check "pl brand is Kinowo"     "Kinowo"    "$(brand_of pl-PL)"
check "uk brand is Showtimes"  "Showtimes" "$(brand_of en-GB)"
check "de brand is Showtimes"  "Showtimes" "$(brand_of de-DE)"
# The tagline is the SHORT translated line, not short-description.txt: that is the
# store blurb, and in English and German it spends its 80 characters listing rating
# sources, which reads as a paragraph on a card.
check "pl tagline" "Repertuar kin w Twoim mieście" "$(tagline_of pl-PL)"
check "de tagline" "Kinoprogramm in deiner Stadt"  "$(tagline_of de-DE)"
check "en tagline" "Cinema listings in your city"  "$(tagline_of en-GB)"
check "the tagline is NOT the store blurb" "0" \
  "$([ "$(tagline_of de-DE)" = "$(tr -d '\n' < "$LISTINGS/de-DE/short-description.txt")" ] && echo 1 || echo 0)"

# Drift guard: these are the same three strings the web share-cards use. Two copies
# of a translation is exactly the sort of thing that diverges silently, so each one
# must still appear verbatim in OgCardGenerator.homeTagline.
OGCARD="$HERE/../../web/src/page/scala/tools/OgCardGenerator.scala"
for l in pl-PL de-DE en-GB; do
  check "$l tagline still matches OgCardGenerator" "1" \
    "$(grep -qF "\"$(tagline_of $l)\"" "$OGCARD" && echo 1 || echo 0)"
done

# ── the card is per-language ─────────────────────────────────────────────────
# The whole point of generating one per locale: German copy on the German card.
_de="$(feature_html de-DE "data:image/png;base64,AA" "data:image/png;base64,BB")"
check "the German card carries the German brand"   "1" "$(printf '%s' "$_de" | grep -c '<h1>Showtimes</h1>')"
check "the German card carries the German tagline" "1" \
  "$(printf '%s' "$_de" | grep -cF "$(tagline_of de-DE)")"
check "and not the English one"                    "0" \
  "$(printf '%s' "$_de" | grep -cF "$(tagline_of en-GB)")"
# Exact Play slot size, asserted in the markup as well as after rendering: a card
# of any other size is rejected at upload.
check "the canvas is Play's 1024×500" "1" \
  "$(printf '%s' "$_de" | grep -c 'width:1024px; height:500px')"
# The tagline is a single line — wrapped, it read as a paragraph rather than a
# strapline. Guarded because it is one CSS property away from silently wrapping.
check "the tagline never wraps" "1" "$(printf '%s' "$_de" | grep -c 'white-space:nowrap')"
check "and is not width-capped into wrapping" "0" "$(printf '%s' "$_de" | grep -c 'p  {.*max-width')"

# The phone is optional: a locale with no screenshots yet must still render a card
# rather than blocking the whole generation.
_no_shot="$(feature_html en-GB "data:image/png;base64,AA" "")"
check "with a screenshot the card shows a phone" "1" "$(printf '%s' "$_de"      | grep -c 'class="phone"')"
check "without one it still renders"             "0" "$(printf '%s' "$_no_shot" | grep -c 'class="phone"')"
check "and still carries the brand"              "1" "$(printf '%s' "$_no_shot" | grep -c '<h1>Showtimes</h1>')"

# ── the pills are the app's, with real values ────────────────────────────────
# Bare source names read as a legend; the app shows ratings, so the card does too.
# These are the values the old Polish card carried, now on every language.
for want in 'class="l">IMDb</span><span class="v">7.9' 'class="pill solid">81' \
            'class="l">RT</span><span class="v">91%' 'class="l">FW</span><span class="v">7.4'; do
  check "the card shows ${want##*>}" "1" "$(printf '%s' "$_de" | grep -cF "$want")"
done
# …and in the app's own colours, not an approximation. Each hex must still be the
# one Theme.kt defines, or the pills quietly stop matching the product.
THEME="$HERE/../app/src/main/java/pl/kinowo/ui/theme/Theme.kt"
for pair in "ImdbYellow:$IMDB" "MetaGood:$META_GOOD" "RtFresh:$RT_FRESH" \
            "FwOrange:$FW_ORANGE" "FwOrangeLight:$FW_LIGHT" "CardElevated:$ELEVATED"; do
  name="${pair%%:*}"; hex="${pair#*:}"
  check "$name is still ${hex}" "1" \
    "$(grep -qi "val $name = Color(0xFF${hex#\#})" "$THEME" && echo 1 || echo 0)"
done

# ── one icon, fanned out ─────────────────────────────────────────────────────
# The icon is language-neutral, so it lives once and is copied. Committing three
# byte-identical copies is how they drift.
check "the shared icon is where the script expects" "1" "$([ -f "$SHARED_ICON" ] && echo 1 || echo 0)"
check "it is NOT inside a locale dir" "0" "$(printf '%s' "$SHARED_ICON" | grep -c '/listings/')"
# And not inside the play/ tree at all: gradle-play-publisher VALIDATES that whole
# directory and fails the build on any path it does not recognise, so a shared
# master parked in there breaks every publish ("Unknown Play resource file").
check "it is outside gradle-play-publisher's tree" "0" \
  "$(printf '%s' "$SHARED_ICON" | grep -c 'src/main/play/')"

IGNORE="$HERE/../.gitignore"
check ".gitignore commits the feature graphics" "1" \
  "$(grep -qxF '!app/src/main/play/listings/*/graphics/feature-graphic/' "$IGNORE" && echo 1 || echo 0)"
# The per-locale icon copies must stay ignored, or the drift starts again.
check ".gitignore keeps the icon copies out" "" \
  "$(grep -vE '^[[:space:]]*#' "$IGNORE" | grep -n 'graphics/icon/' || true)"

# ── what actually got generated ──────────────────────────────────────────────
# Play rejects anything that is not exactly 1024×500, so check the committed cards
# rather than trusting the renderer.
for l in pl-PL en-GB de-DE; do
  f="$LISTINGS/$l/graphics/feature-graphic/1.png"
  if [ -f "$f" ]; then
    check "$l card is exactly 1024x500" "1024x500" \
      "$(sips -g pixelWidth -g pixelHeight "$f" | awk '/pixelWidth/{w=$2} /pixelHeight/{h=$2} END{print w"x"h}')"
  else
    check "$l card exists" "1" "0"
  fi
done

if [ "$fails" -eq 0 ]; then printf '\033[32m✓\033[0m all passed\n'; else printf '\033[31m✗\033[0m %s failed\n' "$fails"; fi
exit $((fails > 0))
