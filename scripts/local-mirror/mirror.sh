#!/usr/bin/env bash
# Purpose: Keep a local mirror of prod's /debug data in sync, continuously (mirror.py beside it; this name is what service.sh and the README run)
exec python3 "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/mirror.py" "$@"
