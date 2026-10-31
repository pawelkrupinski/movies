#!/usr/bin/env bash
# convergence-local.sh (a country's convergence leg run locally on CI's recorded inputs): the leg each code runs, the
# pinned pair restored first, the TMDB key from the environment or the vault (none: stops before sbt), the MongoDB it
# picks (MONGODB_URI, the itAll mongod, one already on the fallback port, else a throwaway container it waits for —
# and gives up on once its deadline passes) and the hermetic environment sbt runs in.
# Run: bash scripts/convergence-local-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/tool-conventions.sh" # bounded
# The job's own keys stay out: the landing check runs this with TMDB_API_KEY/OMDB_API_KEY set, and the vault and
# no-key cases would read them instead of their stubs. Each case names the keys it gives.
unset TMDB_API_KEY OMDB_API_KEY

printf '\033[36m▸\033[0m convergence-local.sh\n'
work="$(mktemp -d)"
listener=""
trap '[ -z "$listener" ] || kill "$listener" 2>/dev/null; rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/tree/scripts"
# The script cds to its checkout and runs scripts/restore-country-corpus.sh there: a tree of its own, the restore a stub.
cp "$REPO_ROOT/scripts/convergence-local.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$REPO_ROOT/scripts/own-mongod.sh" \
  "$work/tree/scripts/"
cat > "$work/tree/scripts/restore-country-corpus.sh" <<'STUB'
#!/usr/bin/env bash
echo "restore $*" >> "$STUB/calls"
[ ! -e "$STUB/restore-fails" ]
STUB
# sbt prints the environment the leg would run in; secrets answers from $STUB/vault-<key>; docker is a container that
# answers its ping (or never does while $STUB/mongo-never-up exists) and reports PRIMARY
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $*" >> "$STUB/calls"
env | grep -E '^(MONGODB_URI|MONGODB_DB|TMDB_API_KEY|OMDB_API_KEY|KINOWO_[A-Z_]+)=' | LC_ALL=C sort > "$STUB/sbt-env"
STUB
cat > "$work/bin/secrets" <<'STUB'
#!/usr/bin/env bash
[ "$1 $2" = "get movies" ] && [ -f "$STUB/vault-$3" ] && cat "$STUB/vault-$3"
STUB
cat > "$work/bin/docker" <<'STUB'
#!/usr/bin/env bash
echo "docker $1" >> "$STUB/calls"
case "$*" in *rs.initiate*) echo "$*" > "$STUB/initiate" ;; esac
case "$1" in
  exec) [ -e "$STUB/mongo-never-up" ] && exit 1
        case "$*" in *myState*) echo 1 ;; esac ;;
esac
exit 0
STUB
chmod +x "$work/tree/scripts/restore-country-corpus.sh" "$work/bin/"*

# Ports nothing listens on: privileged ones, which no user process can bind (connecting is refused at once).
mirror_port=1 fallback_port=2

local_leg() { # <args...> -> the script's output; its exit status in $work/status. Environment: what the caller set.
  rm -f "$work/calls" "$work/sbt-env"
  STUB="$work" PATH="$work/bin:$PATH" LOCAL_MIRROR_PORT="$mirror_port" CONVERGENCE_LOCAL_MONGO_PORT="$fallback_port" \
    CONVERGENCE_LOCAL_MONGO_WAIT=3 bounded 60 bash "$work/tree/scripts/convergence-local.sh" ${BARE-"--i-need-a-local-leg"} "$@" > "$work/out" 2>&1
  echo $? > "$work/status"
}
env_of() { sed -n "s/^$1=//p" "$work/sbt-env" 2>/dev/null; }

MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb BARE='' local_leg de
check "without --i-need-a-local-leg a leg is refused" "2" "$(cat "$work/status")"
check "...restoring and running nothing" "" "$(cat "$work/calls" 2>/dev/null)"
check "...and naming the faster tools" "yes" "$(grep -q 'identity-measure' "$work/out" && grep -q 'identity-slice' "$work/out" && echo yes || echo no)"

MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb local_leg de
check "with --i-need-a-local-leg the leg runs, still naming the faster tools" "0 yes" \
  "$(cat "$work/status") $(grep -q 'running a local leg' "$work/out" && grep -q 'identity-measure' "$work/out" && echo yes || echo no)"

local_leg xx
check "an unknown country is a usage error" "2" "$(cat "$work/status")"
check "...and restores nothing" "" "$(cat "$work/calls" 2>/dev/null)"

MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb local_leg de
check "a country's leg: its pinned pair restored, then its suite" "restore de|sbt convergenceGermany" "$(paste -sd'|' "$work/calls")"
check "...on the MongoDB given" "mongodb://given:1/" "$(env_of MONGODB_URI)"
check "...hermetic, on that country's recorded enrichment tree" "true enrichment-de de de" \
  "$(env_of KINOWO_CONVERGENCE_HERMETIC) $(env_of KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES) $(env_of KINOWO_COUNTRY) $(env_of KINOWO_COUNTRIES)"
check "...its databases under a prefix of its own" "yes" "$([[ $(env_of MONGODB_DB) == kinowo_conv_de_* ]] && echo yes || echo no)"
check "...and no OMDb key, said" "yes" "$(grep -q 'no OMDB_API_KEY' "$work/out" && echo yes || echo no)"

MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb MONGODB_DB=mine local_leg us convergenceUsQuick
check "a named sbt alias replaces the leg's" "sbt convergenceUsQuick" "$(grep '^sbt' "$work/calls")"
check "...and a MONGODB_DB given is kept" "mine" "$(env_of MONGODB_DB)"

echo vault-tmdb > "$work/vault-TMDB_API_KEY"; echo vault-omdb > "$work/vault-OMDB_API_KEY"
MONGODB_URI=mongodb://given:1/ local_leg pl
check "no TMDB_API_KEY set: the vault's" "vault-tmdb vault-omdb" "$(env_of TMDB_API_KEY) $(env_of OMDB_API_KEY)"
rm -f "$work/vault-TMDB_API_KEY"
MONGODB_URI=mongodb://given:1/ local_leg pl
check "no TMDB_API_KEY anywhere: the leg would resolve nothing, so it stops" "1" "$(cat "$work/status")"
check "...before sbt" "" "$(grep '^sbt' "$work/calls")"

touch "$work/restore-fails"
MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb local_leg uk
check "a pair that cannot be restored stops the leg (no fallback)" "restore uk" "$(paste -sd'|' "$work/calls")"
rm -f "$work/restore-fails"

TMDB_API_KEY=tmdb local_leg es
check "no MONGODB_URI, nothing listening: a throwaway replica set, started then waited for" "docker rm|docker run|docker exec|docker exec|docker exec" \
  "$(grep '^docker' "$work/calls" | paste -sd'|' -)"
check "...and the leg runs on it" "mongodb://127.0.0.1:$fallback_port/?directConnection=true sbt convergenceSpain" \
  "$(env_of MONGODB_URI) $(grep '^sbt' "$work/calls")"
# A dropDatabase always waits for majority; with the journal default on, a single node's majority is its journal
# fsync, which a loaded Mac starves past that wait (scripts/own-mongod.sh, the same set for the it/ layer).
check "...a replica set acknowledging majority on apply, not on a journal sync" "1" \
  "$(grep -c 'writeConcernMajorityJournalDefault: false' "$work/initiate" 2>/dev/null || echo 0)"

touch "$work/mongo-never-up"
TMDB_API_KEY=tmdb local_leg es
check "a container that never answers is given up on, not waited for forever" "1" "$(cat "$work/status")"
check "...said, and sbt never run" "yes " "$(grep -q 'never answered' "$work/out" && echo yes || echo no) $(grep '^sbt' "$work/calls")"
rm -f "$work/mongo-never-up"

# Offline: a pair already laid on this machine, laid in place of the download.
laid="$work/laid"; mkdir -p "$laid/corpus" "$laid/enrichment-de/api.themoviedb.org"
echo corpus > "$laid/corpus/cinema-scrapes-de.json.gz"; echo sample > "$laid/corpus/cinema-scrapes-de-sample.json.gz"
echo other > "$laid/corpus/cinema-scrapes-pl.json.gz"; echo tree > "$laid/enrichment-de/api.themoviedb.org/movie"
MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb CONVERGENCE_LOCAL_PAIR="$laid" local_leg de
check "CONVERGENCE_LOCAL_PAIR: the laid pair copied in, nothing restored, then the suite" "0 sbt convergenceGermany" \
  "$(cat "$work/status") $(paste -sd'|' "$work/calls")"
check "...the country's corpus and its tree, no other country's" "corpus sample tree no" \
  "$(cat "$work/tree/test/resources/fixtures/corpus/cinema-scrapes-de.json.gz") $(cat "$work/tree/test/resources/fixtures/corpus/cinema-scrapes-de-sample.json.gz") $(cat "$work/tree/test/resources/fixtures/enrichment-de/api.themoviedb.org/movie") $([ -e "$work/tree/test/resources/fixtures/corpus/cinema-scrapes-pl.json.gz" ] && echo yes || echo no)"
MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb CONVERGENCE_LOCAL_PAIR="$laid" local_leg uk
check "...a pair holding no corpus for the country stops the leg" "1 " "$(cat "$work/status") $(grep '^sbt' "$work/calls" 2>/dev/null)"

# A mongod of the leg's own: the suite run through scripts/it-local.sh (a stub here), no server chosen.
cat > "$work/tree/scripts/it-local.sh" <<'STUB'
#!/usr/bin/env bash
echo "it-local $*" >> "$STUB/calls"
STUB
chmod +x "$work/tree/scripts/it-local.sh"
TMDB_API_KEY=tmdb CONVERGENCE_LOCAL_OWN_MONGOD=1 local_leg de
check "CONVERGENCE_LOCAL_OWN_MONGOD: the suite on a mongod of its own, no container, no shared server" "0 restore de|it-local convergenceGermany" \
  "$(cat "$work/status") $(paste -sd'|' "$work/calls")"

# An itAll mongod: a listener on a port the system picks, kept bound while the leg runs (its port printed once bound).
cat > "$work/listen.py" <<'PY'
import socket, time
s = socket.socket()
s.bind(("127.0.0.1", 0))
s.listen(5)
print(s.getsockname()[1], flush=True)  # the port it holds, never released while the leg runs
time.sleep(60)
PY
python3 "$work/listen.py" > "$work/listener-port" &
listener=$!
wait_until "the listener printed its port" '[ -s "$work/listener-port" ]'
mirror_port=$(cat "$work/listener-port")
TMDB_API_KEY=tmdb local_leg pl
check "the itAll mongod answering: reused, no container" "mongodb://127.0.0.1:$mirror_port/?directConnection=true " \
  "$(env_of MONGODB_URI) $(grep '^docker' "$work/calls")"

spec_summary
