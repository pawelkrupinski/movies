#!/usr/bin/env bash
# WHAT A POD LOG LINE IS LABELLED WITH, asked of the real VRL out of the real host definition.
#
# `nix eval` (bin/check) proves the vector configuration evaluates and that k3s-worker-1 would
# build. It cannot tell you that a line from `worker-de` comes out labelled `country=de` -- and
# every way this transform can be wrong looks like a working config:
#
#   1. It could label NOTHING. A regex that stops matching leaves `app` unset on every line, and
#      the result is the state this file was written to end: one `container="worker"` stream
#      holding five countries, which reads as "quiet" for whichever country you are looking for.
#   2. It could half-strip a pod name. `worker-de-794576c9cf` still carries the ReplicaSet hash,
#      and as a STREAM FIELD that mints new streams on every deploy, for ever, with no way to
#      un-ingest them. Bounded cardinality is the property the whole field list rests on.
#   3. It could invent a country. `kustomize-controller` ends in a segment too, and calling it
#      `country=er` would put Flux's logs inside a country's dashboard.
#   4. It could take the WRONG segment on a deployment whose name has several dashes.
#
# So it renders the transform out of k3s-worker-1's own closure, feeds real pod-log paths through
# the REAL vector, and reads the fields back. The paths below are copied from production.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"
failed=0

nix_flags=(--extra-experimental-features 'nix-command flakes')
if ! command -v nix >/dev/null 2>&1; then
  echo "  FAILED nix is not on PATH, so the log fields were not checked."
  exit 1
fi

# `type -P` and NOT `command -v`, which also finds shell FUNCTIONS -- so the obvious spelling of
# this helper reports itself as already on PATH and then fails to exec.
if vector_bin="$(type -P vector)"; then vector_cmd=("$vector_bin")
else vector_cmd=(nix "${nix_flags[@]}" shell 'nixpkgs#vector' -c vector); fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# THE ATTRSET, NOT THE YAML FILE. `environment.etc."vector/vector.yaml"` is produced by a
# derivation that builds on the TARGET's platform, so asking for it from a developer's Mac fails
# `platform mismatch` and this test would run only on Linux -- which is how a test ends up
# existing and never being run by the person changing the thing it covers. `fleet.logs.settings`
# is the same configuration as a value, so it evaluates anywhere. See its option description.
echo "==> rendering the pod-log transform out of k3s-worker-1"
nix "${nix_flags[@]}" eval --json \
  "$infra/nix#nixosConfigurations.k3s-worker-1.config.fleet.logs.settings" \
  > "$work/settings.json" 2>"$work/eval.err"
if [ ! -s "$work/settings.json" ]; then
  echo "  FAILED could not evaluate fleet.logs.settings:"; sed 's/^/    /' "$work/eval.err" | tail -5; exit 1
fi
config="$work/settings.json"

# THE PROGRAM IS TAKEN FROM THE RENDERED CONFIG, not copied into this file. A copy would keep
# passing after the module stopped agreeing with it, which is the one thing this test must not do.
# `del(.timestamp)` IS APPENDED, and it is the only edit made to the program. `--print-object`
# prints the event in VRL's own notation, where a timestamp is `t'2026-09-06T18:26:18Z'` -- not
# JSON, and the only value in the event that is not. Dropping it makes the line parseable without
# touching a single thing this test asserts on.
python3 -c "
import json, sys
config = json.load(open(sys.argv[1]))
sys.stdout.write(config['transforms']['pod_log_fields']['source'])
sys.stdout.write('\ndel(.timestamp)\n')
" "$config" > "$work/program.vrl" 2>"$work/extract.err"
if [ ! -s "$work/program.vrl" ]; then
  echo "  FAILED could not read transforms.pod_log_fields.source:"; sed 's/^/    /' "$work/extract.err" | tail -5; exit 1
fi
echo "  ok  the transform came out of the closure ($(wc -l < "$work/program.vrl" | tr -d ' ') lines)"

# ONE INVOCATION FOR ALL THE CASES. `vector vrl` runs the program once per input event and prints
# one result per line, so the whole table costs a single process start rather than one each.
: > "$work/events.jsonl"
paths=()
add_case() { # <log path> <expected app> <expected tier> <expected country, or -> <what it proves>
  paths+=("$1|$2|$3|$4|$5")
  python3 -c "
import json, sys
print(json.dumps({'message': '2026-09-06T18:26:18.000000000Z stdout F a line', 'file': sys.argv[1]}))
" "$1" >> "$work/events.jsonl"
}

R=/var/log/pods
add_case "$R/kinowo_worker-de-794576c9cf-hftvh_9a2422f2-3e01-4851-b6c7-da3be161d1d1/worker/0.log" \
  worker-de worker de "the German worker is told apart from every other country's"
add_case "$R/kinowo_worker-pl-7d549dc9d5-zkn58_785ab20b-d007-4f9e-90ab-9d41483587d6/worker/0.log" \
  worker-pl worker pl "...and the Polish one, which shares its container name"
add_case "$R/kinowo_web-uk-7d9cbfc86d-8g92r_afff8935-ad33-4b1f-a110-ebf6d4b83ebe/web/0.log" \
  web-uk web uk "the web tier carries the same country, so a country can be asked as a whole"
add_case "$R/kinowo_web-pl-9f59545d4-7c8bc_1638f241-c70f-4877-8918-13a2c765e838/web/0.log" \
  web-pl web pl "a nine-character ReplicaSet hash is stripped too, not just a ten"
add_case "$R/kinowo_worker-uk-799df957c9-67594_e1b879c5-c7f2-478c-9b97-ecae569e7f7a/worker/0.log" \
  worker-uk worker uk "an all-DIGIT pod suffix is still a suffix"
add_case "$R/kinowo_web-es-f9b8955cb-zxrzq_d31e9458-25dd-4343-9c7b-712df66900bb/web/0.log" \
  web-es web es "Spain, the country onboarded last, needed no list to be added to"
add_case "$R/flux-system_image-automation-controller-6f9dc4dffc-82ddx_14454c22-df48-463c-8971-83d1d8298b41/manager/0.log" \
  image-automation-controller image-automation-controller - \
  "a deployment whose OWN name has dashes keeps all of them, and gets no country"
add_case "$R/flux-system_kustomize-controller-85b965c685-s5qjs_4f969ba9-1e97-407d-85b7-c707938ee24c/manager/0.log" \
  kustomize-controller kustomize-controller - "...and a trailing -controller is not read as a country code"

# `--print-object` prints the EVENT rather than the value of the last expression, which is what
# this test is about. Without it vector answers with whatever the transform happened to end on.
"${vector_cmd[@]}" vrl --print-object --quiet --input "$work/events.jsonl" --program "$work/program.vrl" \
  > "$work/out.jsonl" 2>"$work/vrl.err"
if [ ! -s "$work/out.jsonl" ]; then
  echo "  FAILED vector vrl produced nothing:"; sed 's/^/    /' "$work/vrl.err" | tail -20; exit 1
fi

echo "==> what a line is labelled with"
i=0
while IFS='|' read -r path want_app want_tier want_country why; do
  i=$((i + 1))
  got="$(sed -n "${i}p" "$work/out.jsonl" | python3 -c "
import json, sys
line = sys.stdin.read().strip()
if not line:
    print('-|-|-'); raise SystemExit
event = json.loads(line)
print('%s|%s|%s' % (event.get('app', '-'), event.get('tier', '-'), event.get('country', '-')))
")"
  want="$want_app|$want_tier|$want_country"
  if [ "$got" = "$want" ]; then
    printf '  ok  %s\n' "$why"
  else
    printf '  FAILED %s\n         app|tier|country was %s, wanted %s\n' "$why" "$got" "$want"
    failed=1
  fi
done < <(printf '%s\n' "${paths[@]}")

# ------------------------------------------------------------------------------------------------
# THE LEVEL, WHICH THE THREE SOURCES EACH SPELL DIFFERENTLY
# ------------------------------------------------------------------------------------------------
#
# The journal transform is a DIFFERENT program from the pod one, and both call the same normaliser,
# so both are run here. What is being pinned is that one query -- `level:ERROR` -- reaches the
# application's text format, Flux's JSON and journald's syslog number alike.
echo "==> the level, out of the application's own text format"
: > "$work/level-events.jsonl"
level_cases=()
add_level_case() { # <message> <expected level> <what it proves>
  level_cases+=("$2|$3")
  python3 -c "
import json, sys
print(json.dumps({'message': '2026-09-06T18:26:18.000000000Z stdout F ' + sys.argv[1],
                  'file': '/var/log/pods/kinowo_worker-de-794576c9cf-hftvh_9a2422f2-3e01-4851-b6c7-da3be161d1d1/worker/0.log'}))
" "$1" >> "$work/level-events.jsonl"
}

add_level_case '19:26:51 INFO  services.tasks.ChunkScrapePlanner - a run started' \
  INFO "logback's own two-space INFO is read off the front of the line"
add_level_case '19:26:51 ERROR services.enrichment.ImdbRatings - it failed' \
  ERROR "...and ERROR, which is the one the dashboard is for"
add_level_case '19:26:51 WARN  services.MovieCache - a slot was skipped' \
  WARN "...and WARN"
add_level_case '    at services.tasks.ChunkScrapePlanner.run(ChunkScrapePlanner.scala:42)' \
  - "a STACK-TRACE line inherits no level, because it states none"
add_level_case '19:26:51 INFO  services.Scraper - upstream returned an error for this venue' \
  INFO "the WORD error in a sentence is not a level -- the old regex called this an error"
add_level_case '{"level":"info","ts":"2026-09-06T18:26:18Z","msg":"reconciliation finished"}' \
  INFO "Flux states its level in JSON, and it is upcased to match the application's"
add_level_case '{"level":"warn","ts":"2026-09-06T18:26:18Z","msg":"apply took too long"}' \
  WARN "...and a JSON warn is the same WARN, not a second spelling"

"${vector_cmd[@]}" vrl --print-object --quiet --input "$work/level-events.jsonl" --program "$work/program.vrl" \
  > "$work/level-out.jsonl" 2>"$work/level.err"

i=0
while IFS='|' read -r want why; do
  i=$((i + 1))
  got="$(sed -n "${i}p" "$work/level-out.jsonl" | python3 -c "
import json, sys
line = sys.stdin.read().strip()
print(json.loads(line).get('level', '-') if line else '-')
")"
  if [ "$got" = "$want" ]; then printf '  ok  %s\n' "$why"
  else printf '  FAILED %s\n         level was %s, wanted %s\n' "$why" "$got" "$want"; failed=1; fi
done < <(printf '%s\n' "${level_cases[@]}")

echo "==> ...and out of journald's syslog PRIORITY, through the OTHER transform"
journal_program="$work/journal.vrl"
python3 -c "
import json, sys
config = json.load(open(sys.argv[1]))
sys.stdout.write(config['transforms']['journal_fields']['source'])
sys.stdout.write('\ndel(.timestamp)\n')
" "$config" > "$journal_program"

: > "$work/journal-events.jsonl"
journal_cases=()
add_journal_case() { # <PRIORITY> <expected level> <what it proves>
  journal_cases+=("$2|$3")
  python3 -c "
import json, sys
print(json.dumps({'message': 'something happened', 'PRIORITY': sys.argv[1], '_SYSTEMD_UNIT': 'mongodb.service'}))
" "$1" >> "$work/journal-events.jsonl"
}
add_journal_case 3 ERROR "syslog err is ERROR, so one query spans pods and units"
add_journal_case 4 WARN  "syslog warning is WARN"
add_journal_case 6 INFO  "syslog info is INFO"
add_journal_case 7 DEBUG "syslog debug is DEBUG"

"${vector_cmd[@]}" vrl --print-object --quiet --input "$work/journal-events.jsonl" --program "$journal_program" \
  > "$work/journal-out.jsonl" 2>>"$work/level.err"

i=0
while IFS='|' read -r want why; do
  i=$((i + 1))
  got="$(sed -n "${i}p" "$work/journal-out.jsonl" | python3 -c "
import json, sys
line = sys.stdin.read().strip()
print(json.loads(line).get('level', '-') if line else '-')
")"
  if [ "$got" = "$want" ]; then printf '  ok  %s\n' "$why"
  else printf '  FAILED %s\n         level was %s, wanted %s\n' "$why" "$got" "$want"; failed=1; fi
done < <(printf '%s\n' "${journal_cases[@]}")

echo "==> the pod name is shipped but never shards"
first="$(sed -n 1p "$work/out.jsonl" | python3 -c "
import json, sys
print(json.loads(sys.stdin.read()).get('pod', '-'))
")"
if [ "$first" = "worker-de-794576c9cf-hftvh" ]; then
  echo "  ok  the full pod name is still there to search on"
else
  echo "  FAILED pod was '$first', wanted worker-de-794576c9cf-hftvh"; failed=1
fi

streams="$(python3 -c "
import json, sys
print(json.load(open(sys.argv[1]))['sinks']['victoria_logs']['query']['_stream_fields'])
" "$config")"
case ",$streams," in
  *,pod,*) echo "  FAILED 'pod' is a stream field, which mints a new stream on every deploy"; failed=1 ;;
  *)       echo "  ok  'pod' is NOT a stream field" ;;
esac
for f in app tier country; do
  case ",$streams," in
    *",$f,"*) echo "  ok  '$f' shards, so it can be filtered without scanning" ;;
    *)        echo "  FAILED '$f' is not in _stream_fields"; failed=1 ;;
  esac
done

# SEE `levelVrl`. A level belongs to the LINE, so sharding on it would cut each source's stream
# into one per severity and interleave them, for a filter that is cheap anyway.
case ",$streams," in
  *,level,*) echo "  FAILED 'level' is a stream field, which splits every source's stream by severity"; failed=1 ;;
  *)         echo "  ok  'level' does NOT shard, because it describes the line and not the source" ;;
esac

[ "$failed" = 0 ] && echo "  ok  pod logs are labelled by app, tier and country" || {
  echo; echo "vrl stderr:"; sed 's/^/    /' "$work/vrl.err" | tail -10; }
exit "$failed"
