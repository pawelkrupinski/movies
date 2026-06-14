#!/usr/bin/env bash
# Wipe the LOCAL dev corpus so a locally-run worker re-scrapes + re-projects from
# scratch. Runs the repo's reset-corpus.sh in --local mode (drops the corpus
# collections on the native :28017 Mongo, db kinowo_local) with --yes, since the
# panel console has no TTY to type the interactive confirm word into.
#
# Stop your local `Web + worker` run first so the worker can't re-scrape into the
# wipe — the reset prints that reminder too.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

dispatch "$REPO_ROOT" "Reset local corpus (kinowo_local)" scripts/reset-corpus.sh --local --yes
