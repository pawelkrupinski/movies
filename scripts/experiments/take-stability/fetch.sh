#!/usr/bin/env bash
# Experiment take-stability (docs/identity-experiments.md): dump 8 days of prod take-change and projection lines
# (VictoriaLogs) and the local prod mirror's current takes into the current directory, then run, from it,
# analyse.py (flip rates, flips judged), signal.py (flip history vs WRONG) and hyst.py (hysteresis replay).
set -euo pipefail
ssh monitoring-1 "curl -s http://10.20.0.11:9428/select/logsql/query --data-urlencode 'query=_time:8d kind:* | fields _time, country, build, venue, listing, kind, tmdb_before, tmdb_after, title_before, title_after, basis, previous_basis, confidence, projection_mode, projection_id, pod, reason_first' --data-urlencode limit=100000" > changes.jsonl
ssh monitoring-1 "curl -s http://10.20.0.11:9428/select/logsql/query --data-urlencode 'query=_time:8d \"identity projection:\" | fields _time, country, build, listings, take_changes, projection_mode' --data-urlencode limit=100000" > proj.jsonl
for c in pl:kinowo_prod_mirror uk:kinowo_uk_prod_mirror de:kinowo_de_prod_mirror us:kinowo_us_prod_mirror es:kinowo_es_prod_mirror; do
  "$(dirname "$0")/../../local-mirror/freshness" "${c%%:*}" >/dev/null
  mongosh --quiet "mongodb://127.0.0.1:28017/${c#*:}" --eval 'db.identity_traces.find({},{listing:1,film:1,basis:1}).forEach(d=>print(JSON.stringify({v:d.listing.venue,t:d.listing.rawTitle,f:d.film,b:d.basis})))' | sed "s/^/${c%%:*}\t/"
done > traces.tsv
