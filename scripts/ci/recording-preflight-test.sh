#!/usr/bin/env bash
# recording-preflight.sh: a refused OMDb or TMDB key fails the recording before its legs start; no answer at all only
# warns; a key's value is never printed. curl is a stub that answers each host as the case says.
# Run: bash scripts/ci/recording-preflight-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m recording-preflight.sh\n'

stub="$(mktemp -d)"
trap 'rm -rf "$stub"' EXIT

# answers <omdb status> <omdb body> <tmdb status> <tmdb body>: the stub curl writes the body to `-o` and prints the
# status for `-w`, like the real one; status 000 exits non-zero, as curl does with no answer.
answers() {
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
  *omdbapi.com*) printf '%s' '$2' >"\$out"; printf '%s' '$1'; [ '$1' != 000 ] ;;
  *themoviedb.org*) printf '%s' '$4' >"\$out"; printf '%s' '$3'; [ '$3' != 000 ] ;;
esac
STUB
  chmod +x "$stub/curl"
}

run() {
  env -i PATH="$stub:/usr/bin:/bin" "$@" bash "$REPO_ROOT/scripts/ci/recording-preflight.sh" >"$stub/out" 2>&1
  echo $?
}

ok_omdb='{"Title":"Heat","Response":"True"}'
answers 200 "$ok_omdb" 200 '{"images":{}}'
check "both keys accepted passes" "0" "$(run OMDB_API_KEY=omdb-secret TMDB_API_KEY=tmdb-secret)"
check "no key value is printed" "0" "$(grep -c 'omdb-secret\|tmdb-secret' "$stub/out")"

answers 401 '{"Response":"False","Error":"Request limit reached!"}' 200 '{}'
check "OMDb's spent daily quota (401) fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"
check "and names it" "1" "$(grep -c '::error::OMDb refuses the key (HTTP 401: Request limit reached' "$stub/out")"

answers 200 '{"Response":"False","Error":"Invalid API key!"}' 200 '{}'
check "OMDb's 'Invalid API key' on a 200 fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

answers 200 "$ok_omdb" 401 '{"status_code":7}'
check "a refused TMDB key fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

check "an unset OMDb key fails" "1" "$(run TMDB_API_KEY=t)"
check "an empty TMDB key fails" "1" "$(run OMDB_API_KEY=k TMDB_API_KEY=)"

answers 000 '' 503 'down'
check "no answer, or a 5xx, only warns" "0" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"
check "and says so" "2" "$(grep -c '::warning::' "$stub/out")"

answers 200 '{"Response":"False","Error":"Movie not found!"}' 200 '{}'
check "a 200 without the record is not a refusal" "0" "$(run OMDB_API_KEY=k TMDB_API_KEY=t)"

spec_summary
