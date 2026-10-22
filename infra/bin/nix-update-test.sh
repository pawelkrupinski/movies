#!/usr/bin/env bash
# infra/bin/nix-update's offline self-test: version choice, the mongodb.nix and `mongo:<tag>` rewrites,
# and that the real files still have the shape the weekly run rewrites (a file reshaped by hand fails
# here, on its own push, not on next Monday's run). No network, no nix.
set -euo pipefail
exec python3 "$(dirname "${BASH_SOURCE[0]}")/nix-update" --self-test
