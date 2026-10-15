#!/usr/bin/env bash
#
# Assertions for mirror.sh's supervision loop — that a FAILING step recovers on
# the next cycle instead of killing the supervisor for good. No Mongo, no prod,
# no tunnel: mirror.sh is sourced for its functions and every collaborator is
# stubbed, so this runs anywhere bash does.
#
#   scripts/local-mirror/mirror-resilience-test.sh
#
# Exits 0 when every case passes, 1 on the first failure.
#
# Why this file exists: mirror.sh runs under `set -euo pipefail` with each
# database's `supervise_db` in a background subshell, and a bare non-zero exit
# from any command inside it kills that subshell silently. On 2026-08-02 the
# flyctl tunnel dropped mid-seed, mongosh exited non-zero, and all three
# supervisors died at once — while the parent went on `wait`ing forever, so
# launchd saw a healthy process and never restarted it. Nothing noticed for two
# days. The recovery paths are the whole point of this daemon, so they get a
# test that fails when one of them stops recovering.

# shellcheck disable=SC2034 # the cases set variables the SOURCED mirror.sh functions read (AUDIT_POLLS, MONGOSH_AT, …)
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

failures=0

# Runs a case and reports it. The indirection is load-bearing: bash disables
# `set -e` for the whole dynamic extent of a function invoked as an `if`
# CONDITION — subshells and sourced files included — so writing these as
# `if survives_a_failing_seed; then` would neuter the very option under test and
# the spec would pass against the bug it exists to catch. Called as a plain
# command, `set -e` behaves exactly as it does in the real daemon.
check() {
  local what="$1" case_fn="$2" code
  "$case_fn"
  code=$?
  if [ "$code" -eq 0 ]; then echo "  ok   $what"
  else echo "  FAIL $what"; failures=$((failures + 1)); fi
}

# `supervise_db` loops forever by design, so every case caps the wait rather
# than trusting its stubs to end it. Bounded by the CLOCK, not by a count of
# polls: `sleep 0.1` 200 times is not 20 seconds once process overhead is in it,
# and a bound that quietly stretches lets a case observe the state it was meant
# to prove unreachable — which is how the drift case below first passed against
# a supervisor that had no audit at all. Polled here rather than by a background
# watchdog, whose kill the shell announces as "Terminated" over the spec's own
# output.
await_child() {
  local child="$1" deadline=$((SECONDS + ${2:-20}))
  while kill -0 "$child" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do command sleep 0.1; done
  kill "$child" 2>/dev/null
  wait "$child" 2>/dev/null
}

echo "[spec] mirror supervision"

# ── A seed that fails must not end the supervisor ────────────────────────────
# Driven by running supervise_db in a subshell with `reseed` stubbed to fail.
# If the failure propagates (the bug), the loop dies on cycle 1 and cycle 2's
# marker never appears. If it recovers, cycle 2 runs and the marker is written.
#
# The child is bounded three ways so a regression can only ever make this spec
# FAIL, never hang: the stub exits the shell on cycle 2, `sleep` is stubbed to
# return instantly, and the whole thing is capped by a wall-clock watchdog.
survives_a_failing_seed() {
  local work; work="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mirror-spec.XXXXXX")"
  local cycles="$work/cycles" reached="$work/reached-second-cycle"

  (
    # shellcheck source=/dev/null
    . "$HERE/mirror.sh"                       # functions only — the startup body is guarded

    ensure_local_mongo() { return 0; }
    ensure_tunnel()      { return 0; }
    needs_reseed()       { return 0; }        # always due a seed, so the loop always reaches it
    sleep()              { return 0; }        # no real waiting: backoffs are what we skip past

    # Fails like a mongosh whose connection dropped mid-copy. On the second call
    # the loop has demonstrably survived the first — record it and stop.
    reseed() {
      echo x >> "$cycles"
      [ "$(wc -l < "$cycles")" -ge 2 ] && { touch "$reached"; exit 0; }
      return 1
    }

    supervise_db kinowo 0
  ) >/dev/null 2>&1 &
  local child=$!

  await_child "$child"

  local ok=1
  [ -f "$reached" ] || ok=0
  rm -rf "$work"
  return $((1 - ok))
}

check "a failed seed retries on the next cycle instead of killing the supervisor" \
  survives_a_failing_seed

# ── Drift appearing mid-stream must not wait for the tailer to end ───────────
# `needs_reseed` used to run once per cycle, and a healthy tailer never ends —
# so on a long-lived mirror the gate's last word could be days old. The drift it
# exists to catch does not appear at startup: a delete the stream never
# delivered lands while the tailer is streaming (2026-08-29, DE's
# `pending_movies`). The audit re-asks mid-stream and stops the tailer so the
# next cycle re-seeds; this case fails if a drifting mirror is left tailing.
#
# Bounded the same three ways as the case above, so a regression can only FAIL,
# never hang.
reseeds_when_it_drifts_while_tailing() {
  local work; work="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mirror-spec.XXXXXX")"
  local asked="$work/gate-answers" reached="$work/reseeded"

  (
    # shellcheck source=/dev/null
    . "$HERE/mirror.sh"                       # functions only — the startup body is guarded

    ensure_local_mongo()      { return 0; }
    ensure_tunnel()           { return 0; }
    published_fingerprint()   { echo unchanged; }   # no retarget: the audit is the only exit
    # A SHORT wait, not none: the poll loop forks a subshell per iteration, and
    # a no-op `sleep` turns it into a hot spin that pins a core.
    sleep()                   { command sleep 0.05; }
    AUDIT_POLLS=1                                  # audit on the first poll
    # Set because `load_endpoints` never runs when the file is sourced, and
    # mirror.sh runs under `set -u`: leaving them unbound aborts the tailer's
    # command line before the stub below is reached, so the tailer dies instantly
    # and the case passes against a supervisor that has no audit at all.
    SRCZ=stub-source-uri
    DST=stub-mirror-uri

    # Fresh the first time, so the loop gets past the gate and reaches the
    # tailer. Every answer after that is the drift appearing mid-stream.
    needs_reseed() { echo x >> "$asked"; [ "$(wc -l < "$asked")" -gt 1 ]; }

    # Stands in for tail.js — a stream that runs a while and then ends. It ends
    # ON ITS OWN so this case never has to kill anything: the assertion is about
    # WHEN the re-seed happens, and a supervisor with no audit reaches one too,
    # just not until the stream is over. A SCRIPT in $MONGOSH_AT's place, as the
    # tailer is launched: a `mongosh` function never reached it (mongosh-at.sh
    # execs the binary), so the case leaned on a real mongosh hanging on connect.
    printf '#!/bin/sh\nexec sleep %s\n' "$TAIL_SECONDS" > "$work/tailer"; chmod +x "$work/tailer"
    MONGOSH_AT="$work/tailer"

    reseed() { touch "$reached"; exit 0; }

    supervise_db kinowo 0
  ) >/dev/null 2>&1 &
  local child=$!

  # The whole assertion: a re-seed while the stream is still running. Without
  # the mid-stream audit the earliest one possible is TAIL_SECONDS away, so a
  # window far shorter than that separates the two behaviours with no timing
  # luck involved.
  local deadline=$((SECONDS + 2)) early=no
  while [ "$SECONDS" -lt "$deadline" ]; do
    [ -f "$reached" ] && { early=yes; break; }
    command sleep 0.05
  done

  await_child "$child" $((TAIL_SECONDS + 5))
  rm -rf "$work"
  [ "$early" = "yes" ]
}

# Long enough that "re-seeded within 2s" cannot happen by the stream simply
# ending, short enough that the failing case still finishes in seconds.
TAIL_SECONDS=8
export TAIL_SECONDS

check "a mirror that drifts while tailing is re-seeded without waiting for the stream to end" \
  reseeds_when_it_drifts_while_tailing

# ── The supervisor must notice its OWN code changing ─────────────────────────
# Tailers re-read the .js files whenever one starts, but the supervisor holds
# the command lines it was written with for the life of the process. A commit
# that adds a `--file` to the tailer's invocation therefore lands half-applied
# on a running daemon — new script, old command line — and the tailer dies on an
# undefined function every time (2026-08-30). The digest is what lets the
# supervisor see that and hand over to a fresh one.
#
# Driven against a COPY of the script directory, so the case can edit files
# without touching the repo: every function here reads through `$HERE`.
notices_its_own_code_changing() {
  local work; work="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mirror-spec.XXXXXX")"
  cp "$HERE"/* "$work"/ 2>/dev/null

  (
    # shellcheck source=/dev/null
    . "$HERE/mirror.sh"
    HERE="$work"                                   # …now read the copy, not the repo

    local first second after_runtime after_spec
    first="$(code_fingerprint)"
    [ -n "$first" ] || exit 1                      # a digest that cannot be computed is useless

    second="$(code_fingerprint)"
    [ "$first" = "$second" ] || exit 1             # …and it has to be stable, or every poll restarts

    # An unset baseline is "don't know", never a change — one unreadable digest
    # at startup must not bounce the daemon on its first poll.
    code_changed "" && exit 1

    code_changed "$first" && exit 1                # nothing has changed yet

    echo "// an edit" >> "$work/tail.js"
    after_runtime="$(code_fingerprint)"
    [ "$after_runtime" != "$first" ] || exit 1     # THE case: a file the daemon runs
    code_changed "$first" || exit 1

    # A spec is not something the daemon runs, and editing one must not restart
    # a healthy mirror. This is also what keeps the derived file set honest: it
    # comes from mirror.sh's own `$HERE/...` references, which name no spec.
    echo "// an edit" >> "$work/staleness-rule-spec.js"
    after_spec="$(code_fingerprint)"
    [ "$after_spec" = "$after_runtime" ] || exit 1

    exit 0
  )
  local code=$?
  rm -rf "$work"
  return $code
}

check "the supervisor sees an edit to code it runs, and ignores one to a spec" \
  notices_its_own_code_changing

# ── A stopped supervisor must take its tailer with it ────────────────────────
# The parent's exit trap TERMs each `supervise_db` subshell, but the tailer is
# that subshell's own background child: unhandled, the TERM ended the subshell
# and left its mongosh streaming into the mirror — and launchd's (or service.sh's)
# restart then started a SECOND tailer for the same database beside the orphan.
# The tailer stub records its pid and streams until killed.
tailer_dies_with_its_supervisor() {
  local work; work="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mirror-spec.XXXXXX")"
  printf '#!/bin/sh\necho $$ > "%s/tailer.pid"\nexec sleep 30\n' "$work" > "$work/tailer"; chmod +x "$work/tailer"
  (
    # shellcheck source=/dev/null
    . "$HERE/mirror.sh"
    # In the daemon this subshell is forked with `&`, which resets the parent's
    # traps; sourced here, mirror.sh's top-level `trap cleanup … TERM` would
    # swallow the TERM instead.
    trap - EXIT INT TERM
    ensure_local_mongo()    { return 0; }
    ensure_tunnel()         { return 0; }
    needs_reseed()          { return 1; }
    published_fingerprint() { echo unchanged; }
    SRCZ=stub-source-uri DST=stub-mirror-uri MONGOSH_AT="$work/tailer" TARGETS_POLL=30
    supervise_db kinowo 0
  ) >/dev/null 2>&1 &
  local supervisor=$! deadline=$((SECONDS + 5))
  while [ ! -s "$work/tailer.pid" ] && [ "$SECONDS" -lt "$deadline" ]; do command sleep 0.05; done
  local tailer; tailer="$(cat "$work/tailer.pid" 2>/dev/null)"
  kill -TERM "$supervisor" 2>/dev/null
  await_child "$supervisor" 5
  deadline=$((SECONDS + 3))
  while [ -n "$tailer" ] && kill -0 "$tailer" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do command sleep 0.05; done
  local orphaned=no
  [ -n "$tailer" ] && kill -0 "$tailer" 2>/dev/null && { orphaned=yes; kill "$tailer" 2>/dev/null; }
  rm -rf "$work"
  [ -n "$tailer" ] && [ "$orphaned" = no ]
}

check "a supervisor stopped by the parent stops its tailer too, within seconds" \
  tailer_dies_with_its_supervisor

# ── A dead supervisor must take the whole agent down with it ─────────────────
# The two cases above are about a supervisor RECOVERING. This one is about the
# supervisor that doesn't: every `supervise_db` runs in a background subshell,
# so one bare non-zero under `set -e` still ends it for good, and the parent's
# `wait` never returns because the log rotator and the targets poller outlive
# them all. That is the wedge — a live pid, KeepAlive never firing, /debug
# serving a frozen snapshot (two days in 2026-08-02, a day in 2026-08-30). The
# parent has to notice its supervisors are gone and exit, so launchd restarts a
# clean process.
exits_when_a_supervisor_dies() {
  (
    # shellcheck source=/dev/null
    . "$HERE/mirror.sh"
    LIVENESS_POLL=0.05

    command sleep 30 & local alive=$!
    command sleep 30 & local doomed=$!

    # While they all live the watch must not return — it is the parent's `wait`.
    ( await_supervisor_exit "$alive" "$doomed" ) >/dev/null 2>&1 &
    local watch=$!
    command sleep 0.5
    kill -0 "$watch" 2>/dev/null || { kill "$alive" "$doomed" 2>/dev/null || true; exit 1; }

    # …and the moment one is gone it has to return, without waiting for the rest.
    kill "$doomed" 2>/dev/null || true; wait "$doomed" 2>/dev/null || true
    local deadline=$((SECONDS + 5))
    while kill -0 "$watch" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do command sleep 0.05; done
    local returned=no; kill -0 "$watch" 2>/dev/null || returned=yes

    # `|| true` because the watch is expected to be gone already, and mirror.sh
    # is sourced with `set -e` — a kill against a pid that has exited would end
    # this case before it can report what it measured.
    kill "$alive" "$watch" 2>/dev/null || true
    wait "$alive" "$watch" 2>/dev/null || true
    [ "$returned" = "yes" ]
  )
}

check "the parent stops watching the moment a database supervisor dies" \
  exits_when_a_supervisor_dies

# ── Sourcing must be side-effect free ────────────────────────────────────────
# The spec above depends on it, and so does anything else that wants these
# functions: asking the vault for a secret or opening a tunnel at load time would
# make the file untestable and leak a temp file per source.
sourced_cleanly() {
  local out
  out="$( ( . "$HERE/mirror.sh" && declare -F supervise_db >/dev/null && echo SOURCED ) 2>&1 )"
  [ "$out" = "SOURCED" ]
}

check "sourcing defines the functions without asking the vault or starting anything" \
  sourced_cleanly

# ── The prod tunnel: reaching the RIGHT host, and knowing when it isn't ──────
# prod-tunnel.sh is the single definition of how a laptop reaches prod Mongo,
# shared by mirror.sh, the two sync scripts and reset-corpus.sh. Stubbed the
# same way as above — no ssh, no Mongo, no prod.
echo "[spec] prod tunnel"

# THE CUTOVER BUG. When prod moved off Fly onto mongo-1 (2026-08-29) the
# `flyctl proxy` from before the move kept accepting connections on :27017 while
# the machine behind it was stopped. The old check was `nc -z 127.0.0.1 27017`,
# which that proxy passed, so every cycle declared the tunnel healthy and every
# change stream then died with ECONNRESET — 420 in a row in the log, with
# nothing anywhere naming the tunnel as the cause. A liveness check a dead
# backend passes routes the failure somewhere it cannot be diagnosed.
a_dead_backend_is_not_healthy() {
  (
    # shellcheck source=/dev/null
    . "$HERE/prod-tunnel.sh"
    TUNNEL_PROBE_URI="mongodb://u:p@127.0.0.1:27017/x?serverSelectionTimeoutMS=10000"
    nc()      { return 0; }        # the port accepts — the stale proxy's whole trick
    mongosh_at() { return 1; }     # …but nothing behind it answers
    ssh()     { touch "$SPY_SSH"; return 0; }
    prod_tunnel_answers && return 1                 # must NOT read as healthy
    ensure_prod_tunnel 2>/dev/null && return 1      # must NOT claim success
    # And it must not have tried to bind over a port someone else holds.
    [ -f "$SPY_SSH" ] && return 1
    return 0
  )
}
SPY_SSH="$(mktemp "${TMPDIR:-/tmp}/kinowo-tunnel-spy.XXXXXX")"; rm -f "$SPY_SSH"
check "a tunnel whose backend is dead does not read as healthy" a_dead_backend_is_not_healthy
rm -f "$SPY_SSH"

# We may only ever kill a tunnel WE started. The port being held by someone
# else's forward (an sbt run, a manual one) is a reason to stop and say so, not
# to reach for their pid — and on exit there must be nothing of ours to strand.
close_only_touches_our_own() {
  (
    # shellcheck source=/dev/null
    . "$HERE/prod-tunnel.sh"
    TUNNEL_PROBE_URI="mongodb://u:p@127.0.0.1:27017/x"
    nc()      { return 0; }
    mongosh_at() { return 1; }
    ensure_prod_tunnel >/dev/null 2>&1
    [ -z "$PROD_TUNNEL_PID" ] || return 1     # never adopted a pid that isn't ours
    close_prod_tunnel                          # and closing is a no-op, not a stray kill
  )
}
check "a tunnel held by another process is never adopted or killed" close_only_touches_our_own

# Where the tunnel points is configuration, and the default has to be the host
# prod actually lives on — a default still naming the Fly app would send every
# one of these scripts to a stopped machine. It is NOT a secret, so it is an
# environment variable with a committed default and nothing else: no file, no vault.
target_precedence_and_default() {
  (
    # shellcheck source=/dev/null
    . "$HERE/prod-tunnel.sh"
    # Hermetic against the caller's own shell: an exported KINOWO_MONGO_SSH would
    # otherwise win every assertion below. (An `X="" [ "$(...)" ]` prefix does not
    # do this -- it scopes to `[`, and the substitution has already run by then.)
    unset KINOWO_MONGO_SSH
    [ "$(prod_tunnel_target)" = "root@178.105.221.61" ] || return 1
    [ "$(KINOWO_MONGO_SSH="root@override" prod_tunnel_target)" = "root@override" ]
  )
}
check "the ssh target defaults to mongo-1 and is overridable by the environment" \
  target_precedence_and_default

# ── Secrets: the environment first, the Touch ID vault otherwise ─────────────
# There is no .env.local any more (2026-10-09). MONGODB_URI — the only secret the
# mirror needs — comes from the environment when set (the launchd wrapper in
# service.sh, a caller that exported it), and otherwise mirror.sh re-runs itself
# under `secrets run movies MONGODB_URI`. A fake `secrets` on PATH records how it
# was called; the real vault is never touched.
echo "[spec] secrets"
FAKE_BIN="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-fake-secrets.XXXXXX")"
SECRETS_CALLS="$FAKE_BIN/calls"
cat > "$FAKE_BIN/secrets" <<'FAKE'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$SECRETS_CALLS"
FAKE
chmod +x "$FAKE_BIN/secrets"
export SECRETS_CALLS

# A missing MONGODB_URI must send mirror.sh to the vault for exactly that key —
# and nothing else, before it touches Mongo, a tunnel or a child process.
reexecs_under_the_vault() {
  rm -f "$SECRETS_CALLS"
  env -u MONGODB_URI PATH="$FAKE_BIN:$PATH" MIRROR_LOG="$FAKE_BIN/mirror.log" \
    "$BASH" "$HERE/mirror.sh" --reseed >/dev/null 2>&1 || return 1
  [ "$(cat "$SECRETS_CALLS" 2>/dev/null)" = "run movies MONGODB_URI -- $BASH $HERE/mirror.sh --reseed" ]
}
check "a missing MONGODB_URI re-execs mirror.sh under \`secrets run movies MONGODB_URI\`" \
  reexecs_under_the_vault

# An exported MONGODB_URI is used as-is, with no vault lookup; the mirror target
# is not a secret and defaults to the local mirror database.
exported_uri_skips_the_vault() {
  rm -f "$SECRETS_CALLS"
  (
    # shellcheck source=/dev/null
    . "$HERE/mirror.sh"
    unset MONGODB_MOVIES_MIRROR_URI LOCAL_MIRROR_PORT
    # The vault's value names the in-cluster host; the mirror reads it via the tunnel.
    PATH="$FAKE_BIN:$PATH" MONGODB_URI="mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin" load_endpoints
    [ "$SRC" = "mongodb://u:p@127.0.0.1:27017/kinowo?authSource=admin&directConnection=true" ] || return 1
    [ "$TUNNEL_PROBE_URI" = "$SRC" ] || return 1
    [ "$DST" = "mongodb://127.0.0.1:28017/kinowo_prod_mirror?directConnection=true" ] || return 1
    [ "$(MONGODB_MOVIES_MIRROR_URI="mongodb://elsewhere/x" MONGODB_URI=x load_endpoints; echo "$DST")" = "mongodb://elsewhere/x" ]
  ) || return 1
  [ ! -e "$SECRETS_CALLS" ]
}
check "an exported MONGODB_URI is used, via the tunnel, without touching the vault" exported_uri_skips_the_vault
# Under launchd there is no terminal, and mirror.sh deliberately EXITS to be
# restarted (a supervisor died, its own code changed). If every restart went back
# to the vault, each would raise a fresh Touch ID dialog — at login, overnight —
# so the agent's program is `service.sh agent`: it reads MONGODB_URI from the
# vault ONCE and then restarts mirror.sh itself, with the variable inherited.
agent_reads_the_vault_once() {
  local home; home="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-agent-home.XXXXXX")"
  printf '#!/bin/sh\nexit 0\n' > "$FAKE_BIN/launchctl"; chmod +x "$FAKE_BIN/launchctl"
  HOME="$home" PATH="$FAKE_BIN:$PATH" "$BASH" "$HERE/service.sh" install >/dev/null 2>&1 || { rm -rf "$home"; return 1; }
  local plist="$home/Library/LaunchAgents/pl.kinowo.local-mirror.plist" args
  args="$(tr -d ' \n' < "$plist" | sed -E 's|.*<key>ProgramArguments</key><array>(.*)</array><key>WorkingDirectory.*|\1|')"
  rm -rf "$home"
  [ "$args" = "<string>/bin/bash</string><string>$HERE/service.sh</string><string>agent</string>" ] || return 1

  rm -f "$SECRETS_CALLS"
  env -u MONGODB_URI PATH="$FAKE_BIN:$PATH" "$BASH" "$HERE/service.sh" agent >/dev/null 2>&1 || return 1
  [ "$(cat "$SECRETS_CALLS" 2>/dev/null)" = "run movies MONGODB_URI -- $BASH $HERE/service.sh agent" ]
}
check "the launchd agent asks the vault once, then supervises mirror.sh itself" agent_reads_the_vault_once

rm -rf "$FAKE_BIN"

if [ "$failures" -gt 0 ]; then echo "[spec] $failures failure(s)"; exit 1; fi
echo "[spec] all cases pass"
