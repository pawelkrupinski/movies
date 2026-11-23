#!/bin/sh
# Purpose: Fetch a convergence leg's enrichment tree and unpack it into a staging directory
# The name convergence-setup runs: .github/scripts/restore_enrichment_tree.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/restore_enrichment_tree.py" "$@"
