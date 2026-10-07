#!/usr/bin/env bash
# The busiest collections of the throwaway MongoDB a convergence leg runs against: mongod's own `top`
# counters (time held and operation counts per namespace), summed over every database the leg made, by
# collection name. A leg spends about half its CPU on Mongo — the driver's BSON work and mongod's own
# core (run 37567661957) — and this names which collections that is.
#
#   mongo-top.sh start [seconds]   # sample in the background, inside the `mongo` container
#   mongo-top.sh report [limit]    # print the summary of the samples so far
#
# Sampled rather than read once at the end: a suite drops its databases as it finishes, and mongod
# forgets a dropped namespace's counters, so each namespace keeps the last counters it was seen with.
set -uo pipefail
summary=/tmp/mongo-top.json
case "${1:-}" in
  start)
    interval="${2:-20}"
    docker exec -d mongo mongosh --quiet --eval "
      const fs = require('fs'); const seen = {};
      while (true) {
        for (const [ns, t] of Object.entries(db.adminCommand({ top: 1 }).totals)) {
          if (!ns.includes('.') || /^(admin|local|config)\./.test(ns)) continue;
          seen[ns] = { ms: t.total.time / 1000, reads: t.readLock.count, writes: t.writeLock.count };
        }
        fs.writeFileSync('$summary', JSON.stringify(seen));
        sleep($interval * 1000);
      }" ;;
  report)
    limit="${2:-15}"
    docker exec mongo mongosh --quiet --eval "
      const seen = JSON.parse(require('fs').readFileSync('$summary', 'utf8')); const totals = {};
      for (const [ns, s] of Object.entries(seen)) {
        const t = totals[ns.slice(ns.indexOf('.') + 1)] ??= { ms: 0, reads: 0, writes: 0 };
        t.ms += s.ms; t.reads += s.reads; t.writes += s.writes;
      }
      Object.entries(totals).sort((a, b) => b[1].ms - a[1].ms).slice(0, $limit).forEach(([coll, s]) =>
        print('[mongo-top] ' + coll.padEnd(34) + ' ' + (s.ms / 1000).toFixed(1).padStart(8) + ' s ' +
              String(s.reads).padStart(9) + ' reads ' + String(s.writes).padStart(9) + ' writes'));
    " || echo "[mongo-top] no samples" ;;
  *) echo "usage: $0 start [seconds] | report [limit]" >&2; exit 64 ;;
esac
