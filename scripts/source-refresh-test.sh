#!/usr/bin/env bash
# scripts/source-refresh: one fresh run id per call, exported as the KINOWO_ setting that makes sbt-warm start a server
# for the call AND passed to the program as --run, the database as MONGODB_DB, everything through with-prod-mongo
# (a stub here, recording what it was handed).
# Run: bash scripts/source-refresh-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m source-refresh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cat > "$work/prod" <<STUB
#!/usr/bin/env bash
{ echo "db=\$MONGODB_DB"; echo "run=\$KINOWO_SOURCE_REFRESH_RUN"; printf '%s\n' "\$@"; } > "$work/seen"
STUB
chmod +x "$work/prod"
refresh() { SOURCE_REFRESH_PROD="$work/prod" "$REPO_ROOT/scripts/source-refresh" "$@" > /dev/null 2>&1; echo $?; }

check "runs" "0" "$(refresh kinowo tmdb-films 1525208 1517105 --apply)"
run=$(sed -n 's/^run=//p' "$work/seen")
check "the database is the program's MONGODB_DB" "db=kinowo" "$(sed -n 1p "$work/seen")"
check "a run id is exported" "yes" "$([ -n "$run" ] && echo yes)"
check "...and named to the program, which refuses an environment without it" \
  'worker/Test/runMain scripts.EnqueueSourceRefresh --run '"$run"' "tmdb-films" "1525208" "1517105" "--apply"' "$(sed -n 4p "$work/seen")"
check "...through sbt-warm, so its KINOWO_ setting starts a server for the call" "$REPO_ROOT/scripts/sbt-warm" "$(sed -n 3p "$work/seen")"
refresh kinowo tmdb-films 1 > /dev/null
check "a new id every call" "no" "$([ "$(sed -n 's/^run=//p' "$work/seen")" = "$run" ] && echo yes || echo no)"
# sbt splits its command line at spaces and reads quotes: each key reaches the program as one token, as it was given
touch "$work/a-file-a-glob-would-name"
(cd "$work" && refresh kinowo imdb 'two words' '*' 'say "hi"' 'back\slash' > /dev/null)
check "a key holding a space, a glob, a quote or a backslash: one quoted, escaped sbt token each, never split or globbed" \
  "\"imdb\" \"two words\" \"*\" \"say \\\"hi\\\"\" \"back\\\\slash\"" "$(sed -n 4p "$work/seen" | sed 's/^.* --run [^ ]* //')"
check "a key holding a line break is refused, nothing run" "2 no" \
  "$(rm -f "$work/seen"; refresh kinowo imdb "$(printf 'a\nb')") $([ -f "$work/seen" ] && echo yes || echo no)"
check "no arguments at all is a usage error, not a quiet success" "2" "$(rm -f "$work/seen"; refresh)"
check "--help exits 0" "0" "$(refresh --help)"
check "too few arguments is a usage error, nothing run" "2" "$(rm -f "$work/seen"; refresh kinowo tmdb-films)"
check "...nothing handed on" "no" "$([ -f "$work/seen" ] && echo yes || echo no)"

spec_summary
