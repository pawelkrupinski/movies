#!/usr/bin/env bash
# scripts/identity-explain: `identity-slice <cc> --explain …` under another name — its arguments handed on unchanged
# after the country, its help identity-slice's, its exit status identity-slice's — and identity-slice's --help still
# describing every option the alias's usage line names. identity-slice is a stub beside a copy of the alias.
# Run: bash scripts/identity-explain-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-explain\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/scripts"
cp "$REPO_ROOT/scripts/identity-explain" "$work/scripts/"
cat > "$work/scripts/identity-slice" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$@" > "$work/args"
exit "\${SLICE_EXIT:-0}"
STUB
chmod +x "$work/scripts/identity-slice"

explain() { "$work/scripts/identity-explain" "$@" > /dev/null 2>&1; echo $?; }
args() { tr '\n' ' ' < "$work/args" | sed 's/ $//'; }

check "the country, then --explain, then every argument as given" "0|de --explain --venue Kino Lalka --title Der Wolf --ref HEAD~1" \
  "$(explain de --venue 'Kino Lalka' --title 'Der Wolf' --ref HEAD~1)|$(args)"
check "...each argument one word, its spaces kept" "Kino Lalka" "$(sed -n 4p "$work/args")"
check "identity-slice's exit status is the alias's" "3" "$(SLICE_EXIT=3 explain de --venue Kino)"
check "--help is identity-slice's help" "0|--help" "$(explain --help)|$(args)"
check "no arguments is identity-slice's help too" "0|--help" "$(explain)|$(args)"

# every option the alias's usage line names is one identity-slice --explain describes
usage=$(grep -m1 'scripts/identity-explain <cc>' "$REPO_ROOT/scripts/identity-explain")
help=$("$REPO_ROOT/scripts/identity-slice" --help)
missing=""
for option in $(grep -oE -- '--[a-z-]+' <<< "$usage" | sort -u); do
  grep -q -- "$option" <<< "$(grep -- '--explain' <<< "$help")" || missing+="$option "
done
check "the usage line's options are all ones identity-slice --explain takes" "" "$missing"

spec_summary
