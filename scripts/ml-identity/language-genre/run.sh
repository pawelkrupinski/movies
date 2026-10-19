#!/usr/bin/env bash
# The 2026-10-19 language-from-format / genre study (docs/identity-experiments.md): OFFLINE, no production code.
# Usage: scripts/ml-identity/language-genre/run.sh <out dir> [recording run, default 38713274617] [ratchet run, default 38420977752]
# Needs a venv with numpy + scikit-learn at <out dir>/venv (python3 -m venv <out>/venv && <out>/venv/bin/pip install numpy scikit-learn).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd); repo=$(cd "$here/../../.." && pwd); out=$1; rec=${2:-38713274617}; ratchet=${3:-38420977752}
fx=${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}/recordings/$rec/pair/test/resources/fixtures
cp "$here/features.py" "$out/"
[ -f "$out/tmdb.pkl" ] || python3 "$here/index_tmdb.py" "$fx" "$out/tmdb.pkl"        # ~3 min: original_language + genres of every TMDB film the recording holds
[ -f "$out/listings.pkl" ] || python3 "$here/listings.py" "$fx" "$out/listings.pkl"  # formats + genres of every listing
python3 "$here/judged.py" "$out" "$repo"                                               # coverage + judged takes by level
python3 "$here/ratchet.py" "$out" "${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}/ratchet/$ratchet/ratchet-takes.tsv"
"$out/venv/bin/python" "$here/confidence.py" "$out" "$repo" training                  # AUC/log-loss/Brier with vs without, take simulation
