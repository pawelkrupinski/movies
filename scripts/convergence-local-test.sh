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

printf '\033[36m▸\033[0m convergence-local.sh\n'
work="$(mktemp -d)"
listener=""
trap '[ -z "$listener" ] || kill "$listener" 2>/dev/null; rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/tree/scripts"
# The script cds to its checkout and runs scripts/restore-country-corpus.sh there: a tree of its own, the restore a stub.
cp "$REPO_ROOT/scripts/convergence-local.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$work/tree/scripts/"
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
case "$1" in
  exec) [ -e "$STUB/mongo-never-up" ] && exit 1
        case "$*" in *myState*) echo 1 ;; esac ;;
esac
exit 0
STUB
chmod +x "$work/tree/scripts/restore-country-corpus.sh" "$work/bin/"*

closed_port() { python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()'; }
mirror_port=$(closed_port) fallback_port=$(closed_port)

local_leg() { # <args...> -> the script's output; its exit status in $work/status. Environment: what the caller set.
  rm -f "$work/calls" "$work/sbt-env"
  STUB="$work" PATH="$work/bin:$PATH" LOCAL_MIRROR_PORT="$mirror_port" CONVERGENCE_LOCAL_MONGO_PORT="$fallback_port" \
    CONVERGENCE_LOCAL_MONGO_WAIT=3 bounded 60 bash "$work/tree/scripts/convergence-local.sh" "$@" > "$work/out" 2>&1
  echo $? > "$work/status"
}
env_of() { sed -n "s/^$1=//p" "$work/sbt-env" 2>/dev/null; }

local_leg xx
check "an unknown country is a usage error" "2" "$(cat "$work/status")"
check "...and restores nothing" "" "$(cat "$work/calls" 2>/dev/null)"

MONGODB_URI=mongodb://given:1/ TMDB_API_KEY=tmdb local_leg de
check "a country's leg: its pinned pair restored, then its suite" "restore de|sbt convergenceGermany" "$(paste -sd'|' "$work/calls")"
check "...on the MongoDB given" "mongodb://given:1/" "$(env_of MONGODB_URI)"
check "...hermetic, on that country's recorded enrichment tree" "true enrichment-de de de" \
  "$(env_of KINOWO_CONVERGENCE_HERMETIC) $(env_of KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES) $(env_of KINOWO_COUNTRY) $(env_of KINOWO_COUNTRIES)"
check "...its databases under a prefix of its own" "yes" "$(case "$(env_of MONGODB_DB)" in kinowo_conv_de_*) echo yes ;; *) echo no ;; esac)"
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

touch "$work/mongo-never-up"
TMDB_API_KEY=tmdb local_leg es
check "a container that never answers is given up on, not waited for forever" "1" "$(cat "$work/status")"
check "...said, and sbt never run" "yes " "$(grep -q 'never answered' "$work/out" && echo yes || echo no) $(grep '^sbt' "$work/calls")"
rm -f "$work/mongo-never-up"

python3 -c 'import socket,sys,time; s=socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1); s.bind(("127.0.0.1",int(sys.argv[1]))); s.listen(5); time.sleep(60)' "$mirror_port" &
listener=$!
for _ in 1 2 3 4 5 6 7 8 9 10; do (echo > "/dev/tcp/127.0.0.1/$mirror_port") 2>/dev/null && break; sleep 0.2; done
TMDB_API_KEY=tmdb local_leg pl
check "the itAll mongod answering: reused, no container" "mongodb://127.0.0.1:$mirror_port/?directConnection=true " \
  "$(env_of MONGODB_URI) $(grep '^docker' "$work/calls")"

spec_summary
