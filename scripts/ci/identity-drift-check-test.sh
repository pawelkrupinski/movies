#!/usr/bin/env bash
# identity-drift-check.sh against a stand-in sbt: the verdict is the replay's own report line -- divergences beyond the
# allow-list fail (1), none or only allowed ones pass (0), and a replay that wrote no report, or no recording to replay,
# is no verdict (2). Flips likewise: any the flips allow-list does not name fail (1), and no flips line is no verdict (2).
# The verdict line the row's ::error quotes names divergences and flips apart.
# Run: bash scripts/ci/identity-drift-check-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-drift-check.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/repo/scripts/ci" "$work/repo/test/resources/fixtures/corpus" "$work/bin"
cp "$REPO_ROOT/scripts/ci/identity-drift-check.sh" "$work/repo/scripts/ci/"
# sbt as the replay leaves it: the report on stdout, the TSV under --out, and IdentityDrift's exit
cat > "$work/bin/sbt" <<'EOF'
#!/usr/bin/env bash
out=$(printf '%s\n' "$@" | grep -oE -- '--out [^ ]+' | tail -1 | cut -d' ' -f2)
printf '%s\n' "$@" | grep -oE -- '--seeds? [0-9]+' > "$out.seeds"
printf '%s\n' "$@" > "$out.args"
[ "${FAKE_REPORT:-}" = none ] && { echo "[error] compilation failed"; exit 1; }
mkdir -p "$out"; printf 'country\tseed\n' > "$out/drift-pl.tsv"
# FAKE_VARY: a replay whose flips differ from run to run (a counter beside the out dir)
n=$(( $(cat "$out/../runs" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$out/../runs"
if [ -n "${FAKE_VARY:-}" ]; then printf 'country\tseed\nuk\t2\trun %s\n' "$n" > "$out/flips-pl.tsv"; else printf 'country\tseed\n' > "$out/flips-pl.tsv"; fi
echo "## pl: 10804 listings, 2 seed(s), $FAKE_FOUND divergence(s) (1 listings), $FAKE_ALLOWED allow-listed, in 191s"
[ "$FAKE_FOUND" -gt 0 ] && echo "  1 listing(s)  deploy: incremental-only take (whole BelowThreshold, incremental Agreed)"
[ "${FAKE_FLIPS:-0}" = none ] && exit 0
if [ -n "${FAKE_KNOWN:-}" ]; then
  echo "## pl flips: ${FAKE_FLIPS:-0} flip(s) (1 listings), ${FAKE_FLIPS_ALLOWED:-0} allow-listed, $FAKE_KNOWN known"
  echo "## pl known flips: $FAKE_KNOWN known flip(s) remaining (1 listings in identity-flips-baseline.tsv), ${FAKE_FIXED:-0} fixed"
  [ "${FAKE_FIXED:-0}" -gt 0 ] && echo "      FIXED, remove its row: Kino Eva | LALKA [seed 1] — no flip in this run"
else
  echo "## pl flips: ${FAKE_FLIPS:-0} flip(s) (1 listings), ${FAKE_FLIPS_ALLOWED:-0} allow-listed"
fi
[ "${FAKE_FLIPS:-0}" -gt 0 ] && echo "  1 listing(s)  flip: back to a film it left" && \
  echo "      Riverside | The Stranger: tmdb:1429348 [3 scrape: OwnMatch: own] -> tmdb:848791 [4 answer: Corrected: c] -> tmdb:1429348 [9 scrape (tick-1), inputs unchanged: OwnMatch: own] (back to a film it left) [seed 1]"
[ "$FAKE_FOUND" -gt "$FAKE_ALLOWED" ] && exit 1
[ "${FAKE_FLIPS:-0}" -gt $(( ${FAKE_FLIPS_ALLOWED:-0} + ${FAKE_KNOWN:-0} )) ] && exit 1
exit 0
EOF
chmod +x "$work/bin/sbt"
run() { PATH="$work/bin:$PATH" bash "$work/repo/scripts/ci/identity-drift-check.sh" pl "$work/out" > "$work/stdout" 2>&1; echo $?; }

check "no recorded corpus to replay: no verdict" "2" "$(FAKE_FOUND=0 FAKE_ALLOWED=0 run)"
touch "$work/repo/test/resources/fixtures/corpus/cinema-scrapes-pl.json.gz"
check "no divergence passes" "0" "$(FAKE_FOUND=0 FAKE_ALLOWED=0 run)"
check "divergences the allow-list does not name fail" "1" "$(FAKE_FOUND=3 FAKE_ALLOWED=0 run)"
check "...and the verdict names them" "3 divergence(s) from a whole resolve and 0 flip(s) on the way, beyond the allow-lists and 0 known flip(s)" "$(cat "$work/out/verdict.txt")"
check "...and the report names their class" "1" "$(grep -c 'incremental-only take' "$work/out/report.txt")"
check "divergences the allow-list names all pass" "0" "$(FAKE_FOUND=3 FAKE_ALLOWED=3 run)"
check "a replay that never reported is no verdict" "2" "$(FAKE_REPORT=none run)"
check "flips the flips allow-list does not name fail" "1" "$(FAKE_FOUND=0 FAKE_ALLOWED=0 FAKE_FLIPS=2 run)"
check "...and the report names the listing and its takes" "1" "$(grep -c 'The Stranger: tmdb:1429348' "$work/out/report.txt")"
check "...and the verdict names flips, not divergences" "0 divergence(s) from a whole resolve and 2 flip(s) on the way, beyond the allow-lists and 0 known flip(s)" \
  "$(cat "$work/out/verdict.txt")"
check "flips fail beside allowed divergences too" "1" "$(FAKE_FOUND=3 FAKE_ALLOWED=3 FAKE_FLIPS=1 run)"
check "flips the flips allow-list names pass" "0" "$(FAKE_FOUND=0 FAKE_ALLOWED=0 FAKE_FLIPS=1 FAKE_FLIPS_ALLOWED=1 run)"
check "a replay that reported no flips line is no verdict" "2" "$(FAKE_FOUND=0 FAKE_ALLOWED=0 FAKE_FLIPS=none run)"
FAKE_FOUND=0 FAKE_ALLOWED=0 run > /dev/null
check "both seeds by default" "--seeds 2" "$(cat "$work/out.seeds")"
DRIFT_SEED=2 FAKE_FOUND=0 FAKE_ALLOWED=0 run > /dev/null
check "a row's one seed alone" "--seed 2" "$(cat "$work/out.seeds")"

check "known flips pass" "0" "$(FAKE_FOUND=0 FAKE_ALLOWED=0 FAKE_FLIPS=2 FAKE_KNOWN=2 run)"
check "...said loudly as a warning" "1" "$(grep -c '::warning title=identity drift (pl)::2 known flip(s) remaining' "$work/stdout")"
check "...and the verdict counts no open flip" "0 divergence(s) from a whole resolve and 0 flip(s) on the way, beyond the allow-lists and 2 known flip(s)" \
  "$(cat "$work/out/verdict.txt")"
check "a new flip beside known ones fails" "1" "$(FAKE_FOUND=0 FAKE_ALLOWED=0 FAKE_FLIPS=3 FAKE_KNOWN=2 run)"
check "...its verdict naming the one new flip" "1" "$(grep -c ' and 1 flip(s) on the way' "$work/out/verdict.txt")"
check "a baselined flip that is gone is flagged: remove its row" "0 1" \
  "$(FAKE_FOUND=0 FAKE_ALLOWED=0 FAKE_FLIPS=0 FAKE_KNOWN=0 FAKE_FIXED=1 run) $(grep -c 'Kino Eva | LALKA.*remove its row from identity-flips-baseline.tsv' "$work/stdout")"
check "divergences have no baseline: they fail beside known flips" "1" "$(FAKE_FOUND=1 FAKE_ALLOWED=0 FAKE_FLIPS=2 FAKE_KNOWN=2 run)"
FAKE_FOUND=0 FAKE_ALLOWED=0 run > /dev/null
check "the replay reads the baseline" "1" "$(grep -c 'identity-flips-baseline.tsv' "$work/out.args" 2>/dev/null || echo 0)"
DRIFT_TWICE=true FAKE_FOUND=0 FAKE_ALLOWED=0 run > /dev/null
check "the daily double run: two replays alike pass" "0" "$(DRIFT_TWICE=true FAKE_FOUND=0 FAKE_ALLOWED=0 run)"
check "...having replayed twice" "1" "$(grep -c deterministic "$work/stdout")"
check "two replays that differ fail" "1" "$(DRIFT_TWICE=true FAKE_VARY=1 FAKE_FOUND=0 FAKE_ALLOWED=0 run)"
check "...and the verdict says so" "1" "$(grep -c '^not deterministic' "$work/out/verdict.txt")"
check "a replay that differs from itself passes once alone (the push's rows replay once)" "0" "$(FAKE_VARY=1 FAKE_FOUND=0 FAKE_ALLOWED=0 run)"

spec_summary
