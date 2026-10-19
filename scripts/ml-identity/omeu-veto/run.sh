#!/usr/bin/env bash
# The 2026-10-19 OmeU-vs-English veto study (docs/identity-experiments.md): OFFLINE, no production code.
# Prod's served DE takes (local prod mirror, READ-ONLY) of every listing with an OmeU showtime, split by how many of its
# showtimes are OmeU, each film's TMDB original_language from a recording, and the judgements' verdict.
# Usage: scripts/ml-identity/omeu-veto/run.sh <out dir> <tmdb.pkl from scripts/ml-identity/language-genre/index_tmdb.py>
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd); repo=$(cd "$here/../../.." && pwd); out=$1; tmdb=$2
"$repo/scripts/local-mirror/freshness" de >&2
mongosh --quiet mongodb://127.0.0.1:28017/kinowo_de_prod_mirror "$here/omeu-takes.js" > "$out/omeu-takes.json"
python3 "$here/analyse.py" "$tmdb" "$out/omeu-takes.json" "$repo"
