#!/usr/bin/env bash
# scripts/label-lookup: every judgement touching a listing or a film, on the shapes of 2026-10-14's cases — Lalka (a
# venue's own RIGHT beside every venue's WRONG), Sinn und Sinnlichkeit (two films right at one venue, a re-spaced
# billing), Emil (a part of the title; a known-wrong row in scopes served and rows), Romeo & Juliet (an accepted loss of
# a right take, a listing judged twice) — each entry with the commit that last changed it, the lint's findings that
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
judgements="$work/repo/test/resources/fixtures/identity-judgements.tsv"
mkdir -p "$repo/scripts/ci" "$um" "$work/bin" "$repo/scripts/local-mirror"
cp "$REPO_ROOT/scripts/label-lookup" "$repo/scripts/"
cp "$REPO_ROOT/scripts/local-mirror/freshness" "$repo/scripts/local-mirror/"; cp "$REPO_ROOT/scripts/tool-conventions.sh" "$repo/scripts/"
cp "$REPO_ROOT"/scripts/ci/identity_*.py "$repo/scripts/ci/"  # the tool and every shared module it imports

row() { # country venue rawTitle film verdict [state scope reason evidence]
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t\t%s\t2026-10-14\t%s\n' "$1" "$2" "$3" "$4" "$5" "${6:-open}" "${7:-}" "${8:-}" "${9:-}"; }
{ printf '# the judgements\n'
  printf 'country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n'
  row pl '*' Lalka tmdb:1321666 right open '' '' 'hand label: Lalka (2026)'
  row pl '*' Lalka tmdb:81315 wrong known-wrong rows 'a known wrong take' 'hand label: Lalka (1968)'
  row de 'camera zwo' 'Sinn und Sinnlichkeit' tmdb:4584 right open '' '' 'states 1995, Ang Lee'
  row de 'camera zwo' 'Sinn und Sinnlichkeit' tmdb:1503762 right open '' '' 'states 2026, Oakley'
  row de 'Apollo Hannover' 'Emil und die Detektive' tmdb:10556 right open '' '' 'its page states 2001'
  row de '*' 'Emil und die Detektive (2026)' tmdb:10556 wrong known-wrong rows,served 'no longer served: the 2001 film at one venue'
  row uk BEAM 'RBO: Romeo & Juliet (Ballet)' tmdb:1702789 right accepted-loss '' 'lost to a guard' 'the 2026/27 relay'
  row uk Roses 'The Royal Ballet: Romeo & Juliet' tmdb:476204 right open '' '' 'first judged right'
  row uk Roses 'The Royal Ballet: Romeo & Juliet' tmdb:476204 wrong open '' '' 'the 2013 recording'; } > "$judgements"
printf 'country\tvenue\trawTitle\tfilm\ttitle\nuk\tBEAM\tRBO: Romeo & Juliet (Ballet)\ttmdb:1702789\tRB&O 2026/27\n' > "$um/expected-matches.tsv"
scratch_repo "$repo" && scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "Judge the base labels"
row pl 'Kino Iluzjon' Lalka tmdb:81315 right open '' '' 'the archive page bills Has 1968' >> "$judgements"
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
  "$(grep -A2 'identity-judgements.tsv:12  Kino Iluzjon | Lalka  tmdb:81315  RIGHT' <<< "$out" | grep -c "last changed: $iluzjon .* Judge Kino Iluzjon's Lalka right")"
check "...the known-wrong row among them, its state, scope and reason shown" "1" \
  "$(grep -A1 'identity-judgements.tsv:4  \* | Lalka  tmdb:81315  WRONG known-wrong (rows)' <<< "$out" | grep -c 'note: reason: a known wrong take')"

out=$(lookup de --title "Sinn  und Sinnlichkeit " --venue "camera zwo" --no-prod --no-history)
check "Sinn und Sinnlichkeit: a re-spaced billing is the listing, and both its films are right" "exact 2" \
  "$(grep -c '(exact title)' <<< "$out" | sed 's/1/exact/') $(grep -c 'Sinn und Sinnlichkeit · tmdb:[0-9]*: RIGHT' <<< "$out")"

out=$(lookup de --title emil --no-prod --no-history)
check "Emil: a part of the title finds every billing holding it" "1" "$(grep -c 'titles holding it' <<< "$out")"
check "...the right row and the known-wrong one, served and rows alike" "2" \
  "$(grep -cE 'identity-judgements.tsv:7 .*RIGHT$|identity-judgements.tsv:8  \* \| Emil und die Detektive \(2026\)  tmdb:10556  WRONG known-wrong \(rows,served\)' <<< "$out")"
check "...its reason" "1" "$(grep -c 'note: reason: no longer served: the 2001 film at one venue' <<< "$out")"

out=$(lookup uk --title "Romeo & Juliet" --no-prod --no-history)
check "Romeo & Juliet: the lint's finding on an accepted loss of a right take is named" "1" \
  "$(grep -c '^  store: .*accepted-loss of a take judged RIGHT' <<< "$out")"
check "...and the listing judged twice" "1" "$(grep -c '^  store: .*a second row for uk | Roses | The Royal Ballet: Romeo & Juliet | tmdb:476204' <<< "$out")"

out=$(lookup uk --film tmdb:1702789 --no-prod --no-history)
check "a film: every entry naming it" "2" "$(grep -cE '^  (identity-judgements|expected-matches).tsv:' <<< "$out")"

out=$(TAKES='[["BEAM","RBO: Romeo & Juliet (Ballet)",null,"Vetoed"]]' lookup uk --title "RBO: Romeo & Juliet (Ballet)" --venue BEAM --no-history)
check "prod's take is read from the country's mirror" "1" "$(grep -c 'mongodb://127.0.0.1:28017/kinowo_uk_prod_mirror' "$work/calls")"
check "...a take missing the film the judgements call right is flagged" "1" \
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
