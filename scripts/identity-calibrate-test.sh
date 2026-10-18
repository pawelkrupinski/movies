#!/usr/bin/env bash
# identity-calibrate.sh (regenerate the identity resolver's calibrated model): the sbt command each mode runs — the
# calibration, --refit-signal, --decorations-only, --unified and --refit — in the en-US locale, its artefact versioned
# by the injected date; the inputs each mode needs; --extract-prod's READ-ONLY snapshot of every country database
# through the prod tunnel, and its stop when the tunnel cannot be opened.
# Run: bash scripts/identity-calibrate-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-calibrate.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
tree="$work/tree"
mkdir -p "$work/bin" "$tree/scripts/local-mirror" "$tree/scripts/identity-calibrate" "$tree/test/resources/fixtures/identity-unified"
# The script cds to its checkout: a tree of its own, with the real shared libraries and stubs for what it hands off to.
cp "$REPO_ROOT/scripts/identity-calibrate.sh" "$REPO_ROOT/scripts/identity-tools.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$REPO_ROOT/scripts/jvm-gate.sh" "$tree/scripts/"
cat > "$tree/scripts/identity-refit" <<'STUB'
#!/usr/bin/env bash
echo "identity-refit $*" >> "$STUB/calls"
STUB
# the prod tunnel: opened (or not, while $STUB/no-tunnel exists); mongosh_at prints one row per database read
cat > "$tree/scripts/local-mirror/prod-tunnel.sh" <<'STUB'
ensure_vault_env() { [ -n "${MONGODB_URI:-}" ]; }
prod_tunnel_uri() { echo "tunnelled:$1"; }
init_prod_tunnel() { echo "tunnel $*" >> "$STUB/calls"; }
ensure_prod_tunnel() { [ ! -e "$STUB/no-tunnel" ]; }
close_prod_tunnel() { echo "tunnel closed" >> "$STUB/calls"; }
mongosh_at() { echo "mongosh $KINOWO_CALIBRATE_DB $*" >> "$STUB/calls"; echo "{\"db\":\"$KINOWO_CALIBRATE_DB\"}"; }
STUB
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $*" >> "$STUB/calls"
all="$*"
case "$all" in *IdentityUnifiedDataset*) out=${all##*--out }; mkdir -p "$out"; echo rows > "$out/training.tsv.gz" ;; esac
STUB
chmod +x "$tree/scripts/identity-refit" "$work/bin/sbt"

calibrate() { # <args...> -> its exit status in $work/status; the calls in $work/calls. Environment: the caller's.
  rm -f "$work/calls"
  (cd "$work" && STUB="$work" PATH="$work/bin:$PATH" TODAY=2001-02-03 bash "$tree/scripts/identity-calibrate.sh" "$@" > "$work/out" 2>&1)
  echo $? > "$work/status"
}
calls() { grep "^${1:-}" "$work/calls" 2>/dev/null | sed -e "s|$work|W|g" -e "s/ *$//"; }
locale="-J-Duser.language=en -J-Duser.country=US"

CORPORA=c FIXTURES=f PROD=p calibrate
check "the calibration: one sbt run, en-US, versioned by today's date" \
  "sbt $locale -J-Xmx12g -batch worker/Test/runMain scripts.IdentityCalibrate --corpora c --fixtures f --prod p --report target/identity-calibration --epsilon certified --version calibration-2001-02-03" \
  "$(calls sbt)"
CORPORA=c FIXTURES=f PROD=p VERSION=v9 REPORT=r EPSILON=0.01 COUNTRIES=pl,de CASES=k WEIGHTS=w LABELS=l calibrate
check "...its every knob passed on" \
  "sbt $locale -J-Xmx12g -batch worker/Test/runMain scripts.IdentityCalibrate --corpora c --fixtures f --prod p --report r --epsilon 0.01 --version v9 --countries pl,de --cases k --weights w --labels l" \
  "$(calls sbt)"

CORPORA=c calibrate
check "no FIXTURES: it stops, naming it" "1 yes" "$(cat "$work/status") $(grep -q 'set FIXTURES' "$work/out" && echo yes || echo no)"
check "...before any sbt" "" "$(calls sbt)"
CORPORA=c FIXTURES=f calibrate
check "no PROD for a calibration: it stops, naming it" "1 yes" "$(cat "$work/status") $(grep -q 'set PROD' "$work/out" && echo yes || echo no)"

CORPORA=c FIXTURES=f calibrate --decorations-only
check "--decorations-only needs no PROD, and is versioned by today's date" \
  "sbt $locale -J-Xmx12g -batch worker/Test/runMain scripts.IdentityDecorationsLearn --corpora c --fixtures f --version decorations-2001-02-03" \
  "$(calls sbt)"

CORPORA=c FIXTURES=f PROD=p COUNTRIES=uk calibrate --refit-signal titleShape
check "--refit-signal refits the one signal named" \
  "sbt $locale -J-Xmx12g -batch worker/Test/runMain scripts.IdentityCalibrate --refit-signal titleShape --corpora c --fixtures f --prod p --countries uk" \
  "$(calls sbt)"

calibrate --refit
check "--refit needs no inputs: the refit's measure, its report beside the calibration's" \
  "identity-refit --skip-checks --report target/identity-calibration/refit-report.md" "$(calls identity-refit)"
RELEARN=a,b REPORT=r calibrate --refit
check "...RELEARN passed on" "identity-refit --skip-checks --report r/refit-report.md --relearn a,b" "$(calls identity-refit)"

CORPORA=c FIXTURES=f REPORT="$work/r" calibrate --unified
check "--unified: the rows re-emitted, then the unified weights refitted" "2" "$(calls sbt | wc -l | tr -d ' ')"
check "...both in the en-US locale" "2" "$(calls sbt | grep -c -- "^sbt $locale ")"
check "...the rows checked in" "rows" "$(cat "$tree/test/resources/fixtures/identity-unified/training.tsv.gz" 2>/dev/null)"
check "...the fit's report under REPORT" "yes" "$(grep -q 'IdentityUnifiedFit --report W/r/unified-report.md' <<<"$(calls sbt | tail -1)" && echo yes || echo no)"

CORPORA=c FIXTURES=f PROD="$work/prod" MONGODB_URI=mongodb://prod/ calibrate --extract-prod
check "--extract-prod reads every country database through the tunnel, then calibrates" \
  "kinowo kinowo_uk kinowo_de kinowo_es kinowo_us" "$(calls mongosh | awk '{print $2}' | paste -sd' ' -)"
check "...over the tunnelled URI, the read-only extract" "yes" \
  "$(grep -q 'tunnelled:mongodb://prod/ --quiet --file scripts/identity-calibrate/extract-prod.js' <<<"$(calls mongosh | head -1)" && echo yes || echo no)"
check "...each into its own snapshot" "{\"db\":\"kinowo_us\"}" "$(cat "$work/prod/prod-kinowo_us.jsonl" 2>/dev/null)"
check "...the tunnel closed after" "tunnel closed" "$(calls tunnel | tail -1)"
check "...and the calibration run over the snapshot" "1" "$(calls sbt | grep -c "IdentityCalibrate --corpora c --fixtures f --prod W/prod")"

touch "$work/no-tunnel"
CORPORA=c FIXTURES=f PROD="$work/prod" MONGODB_URI=mongodb://prod/ calibrate --extract-prod
check "a tunnel that cannot be opened stops it, before any read or sbt" "1  " "$(cat "$work/status") $(calls mongosh) $(calls sbt)"

spec_summary
