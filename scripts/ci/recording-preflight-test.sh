#!/usr/bin/env bash
# recording-preflight.sh: a refused key fails the recording before its legs start; OMDb's spent daily quota, no answer
# at all, or a 5xx only warns; a key's value is never printed. curl is a stub that answers each host as the case says,
# OMDb with the responses recorded from omdbapi.com (test/resources/fixtures/omdb/) where there is one.
# Run: bash scripts/ci/recording-preflight-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m recording-preflight.sh\n'

stub="$(mktemp -d)"
trap 'rm -rf "$stub"' EXIT
recorded="$REPO_ROOT/test/resources/fixtures/omdb"

# answers <omdb status> <omdb body> <tmdb status> <tmdb body>: the stub curl writes the body to `-o` and prints the
# status for `-w`, like the real one; status 000 exits non-zero, as curl does with no answer.
answers() {
  printf '%s' "$2" >"$stub/omdb.body"
  printf '%s' "$4" >"$stub/tmdb.body"
  cat >"$stub/curl" <<STUB
#!/usr/bin/env bash
out=""; url=""
while [ \$# -gt 0 ]; do
  case "\$1" in
    -o) out=\$2; shift 2 ;;
    -w | -H | --max-time | --retry) shift 2 ;;
    -*) shift ;;
    *) url=\$1; shift ;;
  esac
done
echo "\$url" >>"$stub/urls"
case "\$url" in
  *omdbapi.com*) cat "$stub/omdb.body" >"\$out"; printf '%s' '$1'; [ '$1' != 000 ] ;;
  *themoviedb.org*) cat "$stub/tmdb.body" >"\$out"; printf '%s' '$3'; [ '$3' != 000 ] ;;
esac
STUB
  chmod +x "$stub/curl"
}

run() {
  env -i PATH="$stub:/usr/bin:/bin" "$@" bash "$REPO_ROOT/scripts/ci/recording-preflight.sh" >"$stub/out" 2>&1
  echo $?
}
said() { grep -cF -- "$1" "$stub/out"; }

# OMDb's whole record puts "Response":"True" last, past its first 400 bytes (1,126 bytes here).
answers 200 "$(cat "$recorded/title_aftersun_2022.json")" 200 '{"images":{}}'
check "both keys accepted passes" "0" "$(run OMDB_API_KEY=omdb-secret TMDB_API_KEY=tmdb-secret)"
check "a whole OMDb record is an accepted key" "1" "$(said 'OMDb: key accepted')"
check "and draws no warning" "0" "$(said '::warning::')"
check "no key value is printed" "0" "$(grep -c 'omdb-secret\|tmdb-secret' "$stub/out")"

# OMDb's spent daily quota as the nightly #2426 met it (HTTP 401 and its "Request limit reached!" document).
answers 401 '{"Response":"False","Error":"Request limit reached!"}' 200 '{}'
check "OMDb's spent daily quota (401) only warns" "0" "$(run OMDB_API_KEY=omdb-secret TMDB_API_KEY=t)"
check "and names it" "1" "$(said "::warning::OMDb's daily quota is spent (HTTP 401: Request limit reached!)")"
check "and when its day turns" "1" "$(said 'midnight US Central')"
check "as no refusal" "0" "$(said '::error::')"
check "printing no key" "0" "$(said 'omdb-secret')"

answers 200 '{"Response":"False","Error":"Request limit reached!"}' 200 '{}'
check "OMDb's spent quota as a 200 document only warns" "0" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

answers 401 "$(cat "$recorded/invalid_api_key.json")" 200 '{}'
check "OMDb's 'Invalid API key!' (401) fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"
check "and names it" "1" "$(said '::error::OMDb refuses the key (HTTP 401: Invalid API key!)')"

answers 200 "$(cat "$recorded/invalid_api_key.json")" 200 '{}'
check "OMDb's 'Invalid API key!' on a 200 fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

answers 401 '' 200 '{}'
check "an OMDb 401 that names no reason fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

answers 200 "$(cat "$recorded/title_aftersun_2022.json")" 401 '{"status_code":7}'
check "a refused TMDB key fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

answers 401 '{"Response":"False","Error":"Request limit reached!"}' 401 '{"status_code":7}'
check "a refused TMDB key fails beside OMDb's spent quota" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

check "an unset OMDb key fails" "1" "$(run TMDB_API_KEY=t)"
check "an empty TMDB key fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=)"

answers 000 '' 503 'down'
check "no answer, or a 5xx, only warns" "0" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"
check "and says so" "2" "$(said '::warning::')"

answers 200 "$(cat "$recorded/title_not_found.json")" 200 '{}'
check "a 200 without the record is not a refusal" "0" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"
check "and names OMDb's reason" "1" "$(said '(Movie not found!)')"

spec_summary
