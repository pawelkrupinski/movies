#!/bin/bash
# Reap stale sbt/java processes left behind by finished builds and test runs.
#
# Two independent rules, because they catch different failure modes:
#
#   Rule A (orphan) — an sbt worker/forked-test JVM whose parent is gone.
#     Conclusive, not a heuristic: these JVMs are always spawned by an sbt
#     server, so PPID 1 means the spawner exited and nothing will ever reap
#     them. Killed regardless of how much CPU they are using, because an
#     orphan that is idle now is still a multi-GB leak that wakes up later.
#
#   Rule B (hog) — any sbt/java process that is both old and has burned a
#     real share of a core over its whole life. Catches wedged JVMs that are
#     still correctly parented (e.g. a detached sbt server whose ZIO/Pekko
#     scheduler threads are spinning). Never a server scripts/sbt-warm KEEPS
#     (a live `sbt-warm --keep` in its directory): one in use compiles and tests
#     for hours, past the hog bar on lifetime CPU alone, and 2026-10-19 this rule
#     killed one mid-use -- a "vanished" server sbt-warm logged nothing about.
#     Its keeper stops it after 10 idle minutes, spinning or not.
#
#   Rule C (duplicate sbt server) — detached `sbt-launch` SERVERS (PPID 1 by
#     design, so Rule A cannot tell them from live ones; recognised by
#     `--detach-stdio`, which a one-shot `sbt` run never carries — a live run
#     sharing a cwd is never a candidate) are grouped by working directory: in
#     each group the NEWEST (the one that cwd's project/target/active.json
#     names) is kept and the rest are reaped once older than
#     ORPHAN_MIN_AGE_SECONDS. A server whose working directory no longer exists
#     (its worktree was removed) is reaped too — but a `<dir>.part` cwd whose
#     final `<dir>` exists is a finished rename (the identity tools build a ref
#     tree as `.part` and rename it), not a removed checkout.
#     Versioned in the repo (scripts/reap-stale-jvms.sh); scripts/install-hooks.sh
#     prints how to switch ~/.local/bin over to it. 2026-10-15: 69
#     idle 1-4 GB servers, ~40 in one worktree, were invisible to A and B
#     (idle: far under the hog CPU share). A server whose parent is something
#     else (the DevPanel's) is not PPID 1 and never a candidate.
#
# CPU is a LIFETIME average (cputime / elapsed), not ps's %CPU. ps %CPU on
# macOS is a decaying recent-usage figure, so it reads high for a process that
# is legitimately busy this second and low for one that spun for hours and
# just went quiet. The lifetime average is the thing that actually means
# "this process has been hogging a core", and it self-forgives: a server that
# worked hard then idled sees its average fall back below the threshold.

# Switching over (the user does this; nothing installs it for you): point whatever runs
# ~/.local/bin/reap-stale-jvms.sh (crontab / launchd) at this file, or
#   ln -sf "$PWD/scripts/reap-stale-jvms.sh" ~/.local/bin/reap-stale-jvms.sh
# from a checkout of main. Spec: scripts/reap-stale-jvms-test.sh.

set -uo pipefail

# --- tunables ---------------------------------------------------------------

HOG_MIN_AGE_SECONDS=${HOG_MIN_AGE_SECONDS:-5400}   # 1.5h
HOG_MIN_CPU_PERCENT=${HOG_MIN_CPU_PERCENT:-5}      # % of ONE core, lifetime avg
ORPHAN_MIN_AGE_SECONDS=${ORPHAN_MIN_AGE_SECONDS:-600}  # 10m grace before Rule A

# Rule B only reaps processes with no controlling terminal. A tty means a human
# has that session open in front of them and can Ctrl-C it; killing their
# 2-hour compile out from under them is far worse than leaving it. Orphaned
# workers and detached servers have no tty, so this costs no real coverage.
# Set to 0 to let Rule B reap tty-attached processes too.
HOG_REQUIRE_NO_TTY=${HOG_REQUIRE_NO_TTY:-1}

# Long-lived JVMs that are supposed to be long-lived and busy. An IDE is the
# one that really matters — IntelliJ is a java process running for days at
# well over 5% of a core, and is exactly what a naive "old + busy java" rule
# would shoot first.
PROTECT_REGEX=${PROTECT_REGEX:-'kinowo\.longrun|scripts\.IdentityCalibrate|JetBrains|IntelliJ|IDEA|Android Studio|jetbrains|toolbox|/Applications/|GradleDaemon|KotlinCompileDaemon|jdk.jcmd|VisualVM|jconsole'}

# Which processes are in scope at all. Deliberately narrow: this reaper has no
# business looking at anything that is not a JVM or an sbt launcher.
SCOPE_REGEX=${SCOPE_REGEX:-'(^|/)java( |$)|(^|/)java$|sbt-launch\.jar|org\.scala-sbt|sbt\.internal|sbt\.ForkMain'}

# Rule A targets: sbt's own worker and forked-test JVMs.
ORPHAN_REGEX=${ORPHAN_REGEX:-'sbt\.internal\.worker[0-9]*\.WorkerMain|sbt\.ForkMain|org\.scala\.sbt\.ForkMain'}

# A process LISTENING on one of these ports is exempt from both rules — it is a
# service someone is deliberately running (a local api on 9000 is the case that
# matters), and "old" is its normal condition rather than evidence of anything.
# The exemption lifts above PROTECTED_PORT_MAX_CPU_PERCENT: a service pinning
# half a core on lifetime average is no longer serving, it is spinning, and at
# that point being a listener is not a reason to spare it.
#
# Resolved fresh on every scan rather than pinned to a pid, so it follows the
# port across restarts. Listeners only — a client with an ephemeral connection
# to 9000 is not the service and gets no exemption.
PROTECTED_PORTS=${PROTECTED_PORTS:-9000}
PROTECTED_PORT_MAX_CPU_PERCENT=${PROTECTED_PORT_MAX_CPU_PERCENT:-50}

LOG_FILE=${LOG_FILE:-"$HOME/Library/Logs/jvm-reaper.log"}
LOG_MAX_LINES=${LOG_MAX_LINES:-5000}
TERM_GRACE_SECONDS=${TERM_GRACE_SECONDS:-10}

DRY_RUN=${DRY_RUN:-0}
[[ "${1:-}" == "--dry-run" || "${1:-}" == "-n" ]] && DRY_RUN=1

# --- plumbing ---------------------------------------------------------------

log() { printf '%s %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" >>"$LOG_FILE"; }

mkdir -p "$(dirname "$LOG_FILE")"

self_pid=$$
self_pgid=$(ps -o pgid= -p $$ 2>/dev/null | tr -d ' ')

# Emit: pid ppid ageSeconds cpuSeconds avgCpuPercent rule shortArgs
#
# The awk time parser reads [[dd-]hh:]mm:ss[.frac] right-to-left, which is the
# only way to handle both columns: macOS prints elapsed as 69-23:47:44 but
# cputime as unbounded minutes (17537:13.68), so a fixed field layout is wrong
# for one of them whichever way you pick it.
# Overridable so the rules can be tested against a fixture of synthetic ps
# lines — an IDE that must survive, a tty-attached build, a 19h orphan — none
# of which can be conjured on a live machine on demand.
PS_COMMAND=${PS_COMMAND:-"ps -Ao pid=,ppid=,tty=,etime=,cputime=,args="}

# Overridable for the same reason as PS_COMMAND — binding a real socket from a
# test just to prove the exemption fires is not worth the flakiness.
if [[ -z "${PORT_LISTENER_PIDS+x}" ]]; then
  PORT_LISTENER_PIDS=""
  for _port in $PROTECTED_PORTS; do
    _pids=$(lsof -nP -iTCP:"$_port" -sTCP:LISTEN -t 2>/dev/null | tr '\n' ' ')
    PORT_LISTENER_PIDS="$PORT_LISTENER_PIDS $_pids"
  done
fi

# Overridable like PS_COMMAND: "pid directory" lines, for Rule C.
CWD_COMMAND=${CWD_COMMAND:-"lsof -nP -a -d cwd -c java -Fpn 2>/dev/null | awk '/^p/{p=substr(\$0,2)} /^n/{print p, substr(\$0,2)}'"}
export CWD_MAP
CWD_MAP=$(eval "$CWD_COMMAND" 2>/dev/null)
# Overridable like PS_COMMAND: the directories (one a line) where a scripts/sbt-warm keeper lives, for Rule B.
KEPT_COMMAND=${KEPT_COMMAND:-"for p in \$(pgrep -f 'sbt-warm --keep'); do lsof -a -p \$p -d cwd -Fn 2>/dev/null | sed -n 's/^n//p'; done | sort -u"}
export KEPT_DIRS
KEPT_DIRS=$(eval "$KEPT_COMMAND" 2>/dev/null)

candidates=$(
  eval "$PS_COMMAND" 2>/dev/null | awk \
    -v port_pids=" $PORT_LISTENER_PIDS " \
    -v port_max_cpu="$PROTECTED_PORT_MAX_CPU_PERCENT" \
    -v hog_age="$HOG_MIN_AGE_SECONDS" \
    -v hog_cpu="$HOG_MIN_CPU_PERCENT" \
    -v orphan_age="$ORPHAN_MIN_AGE_SECONDS" \
    -v require_no_tty="$HOG_REQUIRE_NO_TTY" \
    -v protect="$PROTECT_REGEX" \
    -v scope="$SCOPE_REGEX" \
    -v orphan_re="$ORPHAN_REGEX" \
    -v self_pid="$self_pid" '
    function emit(pid, ppid, age, cpu, pct, rule, args,   short) {
      # Collapse the -Duser.country=US -Duser.language=en spam sbt emits; a
      # single worker line is otherwise ~4KB of identical repeated flags.
      gsub(/-Duser\.(country|language)=[A-Za-z]+ ?/, "", args)
      short = substr(args, 1, 150)
      printf "%s %s %d %d %.1f %s %s\n", pid, ppid, age, cpu, pct, rule, short
    }
    BEGIN {
      n = split(ENVIRON["CWD_MAP"], lines, "\n")
      for (i = 1; i <= n; i++) {
        sp = index(lines[i], " ")
        if (sp > 0) cwd[substr(lines[i], 1, sp - 1)] = substr(lines[i], sp + 1)
      }
      n = split(ENVIRON["KEPT_DIRS"], lines, "\n")
      for (i = 1; i <= n; i++) if (lines[i] != "") kept[lines[i]] = 1
    }
    function tosec(t,   dd, a, p, n, s) {
      dd = 0
      if (t ~ /-/) { split(t, a, "-"); dd = a[1] + 0; t = a[2] }
      n = split(t, p, ":")
      s = 0
      if (n >= 1) s += p[n] + 0
      if (n >= 2) s += (p[n-1] + 0) * 60
      if (n >= 3) s += (p[n-2] + 0) * 3600
      return s + dd * 86400
    }
    {
      pid = $1; ppid = $2; tty = $3; etime = $4; cputime = $5
      args = ""
      for (i = 6; i <= NF; i++) args = args (i > 6 ? " " : "") $i

      if (pid == self_pid || pid <= 1) next
      if (args !~ scope) next
      if (args ~ protect) next

      age = tosec(etime)
      cpu = tosec(cputime)
      pct = (age > 0) ? (cpu * 100.0 / age) : 0

      # Checked ahead of both rules, so a listener under the ceiling is spared
      # even when it is an orphan — deliberate: something is still talking to
      # it, which outranks the fact that its parent went away.
      if (index(port_pids, " " pid " ") > 0 && pct <= port_max_cpu) next

      rule = ""
      if (args ~ orphan_re && ppid == 1 && age >= orphan_age) {
        rule = "orphan"
      } else if (age >= hog_age && pct >= hog_cpu && !(args ~ /--detach-stdio/ && (pid in cwd) && (cwd[pid] in kept))) {
        if (require_no_tty == 1 && tty != "??" && tty != "?" && tty != "-") next
        rule = "hog"
      }
      if (rule == "" && ppid == 1 && args ~ /sbt-launch\.jar/ && args ~ /--detach-stdio/ && (pid in cwd)) {
        # Rule C is decided at the end, once every server of a directory is known.
        srv[pid] = 1; sage[pid] = age; scpu[pid] = cpu; spct[pid] = pct; sargs[pid] = args
        next
      }
      if (rule == "") next
      emit(pid, ppid, age, cpu, pct, rule, args)
    }
    END {
      for (pid in srv) {
        d = cwd[pid]; sub(/\.part$/, "", d); cwd[pid] = d
        if (!(d in newest) || sage[pid] < sage[newest[d]] || (sage[pid] == sage[newest[d]] && pid + 0 > newest[d] + 0)) newest[d] = pid
      }
      for (pid in srv) {
        d = cwd[pid]
        gone = system("test -d \"" d "\" || test -d \"" d ".part\"") != 0
        if (gone) rule = "dup-sbt-gone-dir"
        else if (newest[d] != pid && sage[pid] >= orphan_age) rule = "dup-sbt-server"
        else continue
        emit(pid, 1, sage[pid], scpu[pid], spct[pid], rule, sargs[pid] " cwd=" d)
      }
    }'
)

if [[ -z "$candidates" ]]; then
  # Nothing to do is the normal case; keep it to one line so the log stays
  # readable when scanned by eye after a slow machine.
  log "scan: nothing to reap"
  exit 0
fi

# --- act --------------------------------------------------------------------

reaped=0
while read -r pid ppid age cpu pct rule short; do
  [[ -z "${pid:-}" ]] && continue

  human_age=$((age / 3600))h$(((age % 3600) / 60))m
  detail="pid=$pid ppid=$ppid rule=$rule age=$human_age cpu=${pct}%-of-a-core cmd=[$short]"

  # Reported before the runtime guards below, so a dry run answers "what do the
  # rules select" rather than "what is still alive a second later" — which is
  # also what makes the rules testable against a fixture of dead pids.
  if [[ "$DRY_RUN" == "1" ]]; then
    log "WOULD REAP $detail"
    printf 'WOULD REAP %s\n' "$detail"
    continue
  fi

  # Rule C reaps a duplicate server only while it is IDLE. Two agents can legitimately hold servers in one
  # directory at once (a shared identity refs tree), and a server compiling or running tests this second is
  # live work, not a leak: 2026-10-15 00:35 it killed one at 120% of a core mid-run. Recent %CPU, not the
  # lifetime average, is the right signal here; a busy duplicate is retried on the next scan.
  if [[ "$rule" == "dup-sbt-server" && "$DRY_RUN" != "1" ]]; then
    recent=$(ps -o %cpu= -p "$pid" 2>/dev/null | tr -d ' ')
    if [[ -n "$recent" ]] && awk -v c="$recent" -v m="${DUP_MAX_RECENT_CPU_PERCENT:-5}" 'BEGIN{exit !(c+0 > m+0)}'; then
      log "skip pid=$pid ($rule) — busy now (${recent}% CPU), not idle"
      continue
    fi
  fi

  # Re-check liveness: the scan and the kill are seconds apart and a build
  # that finished in between must not have its pid reused out from under us.
  if ! kill -0 "$pid" 2>/dev/null; then
    log "skip pid=$pid ($rule) — already gone"
    continue
  fi

  # Never shoot our own process group (launchd -> bash -> ps pipeline).
  pgid=$(ps -o pgid= -p "$pid" 2>/dev/null | tr -d ' ')
  if [[ -n "$self_pgid" && "$pgid" == "$self_pgid" ]]; then
    log "skip pid=$pid ($rule) — own process group"
    continue
  fi

  log "REAP $detail"

  # TERM first so a JVM that is merely idle gets to run its shutdown hooks and
  # flush; KILL after the grace period for the wedged ones, which is most of
  # them — a JVM spinning in a runaway scheduler often never services TERM.
  kill -TERM "$pid" 2>/dev/null
  waited=0
  while ((waited < TERM_GRACE_SECONDS)) && kill -0 "$pid" 2>/dev/null; do
    sleep 1
    ((waited++))
  done

  if kill -0 "$pid" 2>/dev/null; then
    kill -KILL "$pid" 2>/dev/null
    sleep 1
    if kill -0 "$pid" 2>/dev/null; then
      log "FAILED pid=$pid survived SIGKILL"
    else
      log "killed pid=$pid (SIGKILL after ${TERM_GRACE_SECONDS}s)"
      ((reaped++))
    fi
  else
    log "killed pid=$pid (SIGTERM, ${waited}s)"
    ((reaped++))
  fi
done <<<"$candidates"

[[ "$DRY_RUN" == "1" ]] || log "scan: reaped $reaped process(es)"

# Trim the log in place so it cannot grow without bound between machine setups.
if [[ -f "$LOG_FILE" ]]; then
  line_count=$(wc -l <"$LOG_FILE" | tr -d ' ')
  if ((line_count > LOG_MAX_LINES)); then
    tail -n "$LOG_MAX_LINES" "$LOG_FILE" >"$LOG_FILE.tmp" && mv "$LOG_FILE.tmp" "$LOG_FILE"
  fi
fi

exit 0
