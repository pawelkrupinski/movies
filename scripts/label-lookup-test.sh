#!/usr/bin/env bash
# scripts/label-lookup: every judgement touching a listing or a film, on the shapes of 2026-10-14's cases — Lalka (a
# venue's own RIGHT beside every venue's WRONG), Sinn und Sinnlichkeit (two films right at one venue, a re-spaced
# billing), Emil (a part of the title; MatchedTakesLabelSpec's maps), Romeo & Juliet (an accepted move off a right
# take, a label both right and wrong) — each entry with the commit that last changed it, the lint's findings that
# touch it, and what a prod mirror (a stub mongosh) takes. The repository is a scratch one.
# Run: bash scripts/label-lookup-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m label-lookup\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo" um="$work/repo/test/resources/fixtures/identity-unmatched"
mkdir -p "$repo/scripts/ci" "$um" "$repo/test/resources/fixtures/identity-suspects" "$repo/test/resources/fixtures/identity-takes" \
  "$repo/worker/src/test/scala/services/identity" "$work/bin" "$repo/scripts/local-mirror"
cp "$REPO_ROOT/scripts/label-lookup" "$repo/scripts/"
cp "$REPO_ROOT/scripts/local-mirror/freshness" "$repo/scripts/local-mirror/"
cp "$REPO_ROOT/scripts/ci/identity_judgements_lint.py" "$REPO_ROOT/scripts/ci/identity_takes_diff.py" "$repo/scripts/ci/"

{ printf 'country\tvenue\trawTitle\tfilm\tverdict\tnote\n'
  printf 'pl\t*\tLalka\ttmdb:1321666\tright\thand label: Lalka (2026)\n'
  printf 'pl\t*\tLalka\ttmdb:81315\twrong\thand label: Lalka (1968)\n'
  printf 'de\tcamera zwo\tSinn und Sinnlichkeit\ttmdb:4584\tright\tstates 1995, Ang Lee\n'
  printf 'de\tcamera zwo\tSinn und Sinnlichkeit\ttmdb:1503762\tright\tstates 2026, Oakley\n'
  printf 'de\tApollo Hannover\tEmil und die Detektive\ttmdb:10556\tright\tits page states 2001\n'
  printf 'uk\tBEAM\tRBO: Romeo & Juliet (Ballet)\ttmdb:1702789\tright\tthe 2026/27 relay\n'
  printf 'uk\tRoses\tThe Royal Ballet: Romeo & Juliet\ttmdb:476204\tright\tfirst judged right\n'
  printf 'uk\tRoses\tThe Royal Ballet: Romeo & Juliet\ttmdb:476204\twrong\tthe 2013 recording\n'; } > "$um/labels.tsv"
printf 'country\trawTitle\tfilm\ttitle\treasons\nuk\tRomeo and Juliet\ttmdb:6003\tRomeo and Juliet (1968)\trelay taken as Zeffirelli\n' > "$um/known-wrong.tsv"
printf 'country\tvenue\trawTitle\tfilm\ttitle\nuk\tBEAM\tRBO: Romeo & Juliet (Ballet)\ttmdb:1702789\tRB&O 2026/27\n' > "$um/expected-matches.tsv"
printf 'country\trawTitle\tfilm\ttitle\treasons\nde\tEmil und die Detektive\ttmdb:1426805\tEmil (2026)\twrong by labels.tsv\n' \
  > "$repo/test/resources/fixtures/identity-suspects/baseline.tsv"
{ printf '# accepted moves\ncountry\tvenue\trawTitle\tbefore\tafter\tnote\n'
  printf 'uk\tBEAM\tRBO: Romeo & Juliet (Ballet)\ttmdb:1702789\tnone\tlost to a guard\n'; } > "$repo/test/resources/fixtures/identity-takes/accepted.tsv"
cat > "$repo/worker/src/test/scala/services/identity/MatchedTakesLabelSpec.scala" <<'SCALA'
class MatchedTakesLabelSpec {
  private lazy val multiFilmBill =
    "matched to no film since 4456afbbc (MultiFilmBill)"

  private val noLongerServed: Map[(String, String, String), String] = Map(
    ("de", "Emil und die Detektive (2026)", "tmdb:10556") ->
      ("the 2001 film at one venue; " +
        "struck once the rows are re-emitted"),
    ("uk", "The Dark Knight Trilogy", "tmdb:155") -> multiFilmBill)

  private val knownWrong: Map[(String, String, String), String] = Map(
    ("pl", "Lalka", "tmdb:81315") -> "a known wrong take")
}
SCALA
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "Judge the base labels"
printf 'pl\tKino Iluzjon\tLalka\ttmdb:81315\tright\tthe archive page bills Has 1968\n' >> "$um/labels.tsv"
scratch_git "$repo" commit -qam "Judge Kino Iluzjon's Lalka right"
iluzjon=$(scratch_git "$repo" rev-parse --short=9 HEAD)

# mongosh answers as the mirror would: scripts/local-mirror/freshness's verdict ($FRESH, exit $FRESH_EXIT), then
# identity_traces' takes ($TAKES)
cat > "$work/bin/mongosh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB_CALLS"
case " $* " in
  *" --nodb "*) echo "xx  the_mirror: $FRESH"; exit "$FRESH_EXIT" ;;
  *) printf '%s\n' "$TAKES" ;;
esac
STUB
chmod +x "$work/bin/mongosh"
lookup() { STUB_CALLS="$work/calls" FRESH=${FRESH:-fresh} FRESH_EXIT=${FRESH_EXIT:-0} TAKES=${TAKES:-[]} PATH="$work/bin:$PATH" \
  "$repo/scripts/label-lookup" "$@" 2> "$work/err"; }

out=$(lookup pl --title Lalka --venue "Kino Iluzjon" --no-prod)
check "Lalka: the venue's own RIGHT overrules every venue's WRONG" "1" \
  "$(grep -c "Lalka · tmdb:81315: RIGHT (the venue's own right beside '\*' wrong)" <<< "$out")"
check "...and every venue's RIGHT still holds there, named as two films right at one venue" "1 1" \
  "$(grep -c "Lalka · tmdb:1321666: RIGHT ('\*')" <<< "$out") $(grep -c 'NOTE 2 films judged right for one venue: tmdb:1321666, tmdb:81315' <<< "$out")"
check "...each entry with the commit that last changed its line" "1" \
  "$(grep -A2 'labels.tsv:10  Kino Iluzjon | Lalka  tmdb:81315  RIGHT' <<< "$out" | grep -c "last changed: $iluzjon .* Judge Kino Iluzjon's Lalka right")"
check "...MatchedTakesLabelSpec's known-wrong take among them" "1" \
  "$(grep -A1 'MatchedTakesLabelSpec.scala:1[0-9]  Lalka  tmdb:81315  known wrong (matched)' <<< "$out" | grep -c 'note: a known wrong take')"

out=$(lookup de --title "Sinn  und Sinnlichkeit " --venue "camera zwo" --no-prod --no-history)
check "Sinn und Sinnlichkeit: a re-spaced billing is the listing, and both its films are right" "exact 2" \
  "$(grep -c '(exact title)' <<< "$out" | sed 's/1/exact/') $(grep -c 'Sinn und Sinnlichkeit · tmdb:[0-9]*: RIGHT' <<< "$out")"

out=$(lookup de --title emil --no-prod --no-history)
check "Emil: a part of the title finds every billing holding it" "1" "$(grep -c 'titles holding it' <<< "$out")"
check "...the labels, the suspect baseline and the spec's no-longer-served take" "3" \
  "$(grep -cE 'labels.tsv:6 .*RIGHT|baseline.tsv:2 .*suspect baseline|MatchedTakesLabelSpec.scala:[0-9]+  Emil und die Detektive \(2026\)  tmdb:10556  no longer served' <<< "$out")"
check "...the spec's why joined across its lines" "1" "$(grep -c 'note: the 2001 film at one venue; struck once the rows are re-emitted' <<< "$out")"

out=$(lookup uk --title "Romeo & Juliet" --no-prod --no-history)
check "Romeo & Juliet: the lint's accepted finding is named" "1" "$(grep -c '^  accepted: accepted.tsv accepts losing tmdb:1702789 at BEAM' <<< "$out")"
check "...and its conflict" "1" "$(grep -c '^  conflict: labels.tsv judges tmdb:476204 both right and wrong' <<< "$out")"

out=$(lookup uk --film tmdb:1702789 --no-prod --no-history)
check "a film: every entry naming it" "3" "$(grep -cE '^  (labels|expected-matches|accepted).tsv:' <<< "$out")"

out=$(TAKES='[["BEAM","RBO: Romeo & Juliet (Ballet)",null,"Vetoed"]]' lookup uk --title "RBO: Romeo & Juliet (Ballet)" --venue BEAM --no-history)
check "prod's take is read from the country's mirror" "1" "$(grep -c 'mongodb://127.0.0.1:28017/kinowo_uk_prod_mirror' "$work/calls")"
check "...a take missing the film labels.tsv calls right is flagged" "1" \
  "$(grep -c 'BEAM | RBO: Romeo & Juliet (Ballet) -> none (Vetoed)  \[misses RIGHT tmdb:1702789\]  <- prod misses' <<< "$out")"
check "...its freshness asked first of scripts/local-mirror/freshness, for the collection it reads" "1" \
  "$(grep -c "var CCS=\['uk'\].*var NEED=\['identity_traces'\]" "$work/calls")"
out=$(FRESH="synced 30h ago — STALE" FRESH_EXIT=3 TAKES='[["Kino Iluzjon","Lalka",81315,"OwnMatch"]]' lookup pl --title Lalka --no-history)
check "...a mirror not fresh is said to be, and Poland's is kinowo_prod_mirror" "1 1" \
  "$(grep -c 'synced 30h ago — STALE -- NOT FRESH' <<< "$out") $(grep -c 'kinowo_prod_mirror?' "$work/calls")"
check "...a take a label judges carries its verdict" "1" "$(grep -c 'Kino Iluzjon | Lalka -> tmdb:81315 (OwnMatch)  \[labels: RIGHT\]' <<< "$out")"
out=$(FRESH="ABSENT" FRESH_EXIT=2 lookup pl --title Lalka --no-history)
check "...and no mirror at all is said so, its takes not read" "1" "$(grep -c 'identity_traces; no mirror: the_mirror: ABSENT)' <<< "$out")"

check "a call naming both a listing and a film is refused" "2" "$(lookup pl --title Lalka --film tmdb:1 > /dev/null; echo $?)"
check "a venue without a title is refused" "2" "$(lookup pl --venue X > /dev/null; echo $?)"

spec_summary
