# Writing a prod-ops script

A runbook step that reads or changes prod is a script with `--check` and `--apply` and a test, never a snippet pasted
into a doc. Snippets drift, get pasted into the wrong shell and cannot refuse. Examples: `scripts/mongo-grants`
(`--check` / `--apply`) and `scripts/fleet-db-finish` (read-only, or `--drop` with three exact names).

## The shape

1. **One header comment** (the tool's usage, read by `-h` through `usage_from_header`, `scripts/tool-conventions.sh`):
   what each mode does, when to run it, what it refuses.
2. **`set -euo pipefail`, then source `scripts/prod-ops.sh`.** It gives you:
   - `prod_ops_mode "$@"`: exactly one of `--check` / `--apply` (else usage, exit 2); sets `PROD_OPS_MODE`.
   - `prod_ops_exact_names --drop "a b c" "$@"`: a destructive flag takes the exact names, each once, no patterns.
     Wrong arguments exit 2 before any credential is asked for.
   - `prod_ops_session --both "$@"`: one Touch ID and one tunnel (`scripts/with-prod-mongo.sh`); the script re-runs
     itself inside it. Or `prod_ops_exec --both <command>` when the work is another program (`mongo_grants.py`).
   - `as_root <js>` / `as_app <js>`: mongosh as the root or the app user. Read user names off the connection strings;
     never write a name or a password into the script.
   - `prod_ops_phase <title>`, `prod_ops_refuse <reason>` (exit 1: the state says stop), `prod_ops_usage_error`.
3. **Idempotent phases.** `--check` changes nothing and exits 1 when something is missing; `--apply` does exactly what
   is missing, reads it back, and a second run changes nothing.
4. **Refusal guards in the script**: a destructive phase checks its precondition itself (the copy exists and is
   non-empty, the app user can read it) and re-checks the name beside the action.

## The test

`<tool>-test.sh` sources `scripts/shell-spec.sh` and `scripts/prod-ops-fixture.sh`: fakes of `secrets`, `ssh`, `nc`
and `mongosh` on PATH, so no vault, tunnel or Mongo is touched. You write only the fake mongosh body
(`fake_mongosh <<'BODY'`, a `case "$*"` over the script it was given, recording what it saw and as whom) and call the
tool through `prod_ops_run`. Specs to include: check passes / fails naming what is missing; apply grants exactly the
missing; a second apply does nothing; every refusal; one vault run and one tunnel; bad arguments touch nothing.
`scripts/run-script-tests` finds it by name. Never run the tool against prod from a test or an agent: the user runs
`--check`, reads it, then `--apply`.
