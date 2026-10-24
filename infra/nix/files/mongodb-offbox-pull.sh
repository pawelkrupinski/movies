#!/usr/bin/env bash
# THE OFF-BOX COPY OF mongo-1'S NIGHTLY DUMP, PULLED FROM A MACHINE THAT IS NOT mongo-1.
#
# roles/mongodb.nix's header names the shape this has to take, and the shape is the whole design: a
# PULL, by a host that holds its own copy, through a key mongo-1 can only ever answer READ-ONLY.
# A push from mongo-1 would need mongo-1 to hold a credential that can write the copy -- so a
# compromised mongo-1 could delete both. Pulled, the worst mongo-1 can do is stop offering new dumps,
# and the stale-copy alert in mongodb.rules says so within a day and a half.
#
# Two subcommands, because they run as two users (see roles/mongodb-offbox-backup.nix):
#
#   pull     as the unprivileged puller: copy the newest COMPLETED dump, then rotate by count.
#   metrics  as root, only after `pull` succeeded: publish the textfile node_exporter reads.
#
# Configured entirely through the environment, so infra/test/test_mongodb_offbox_pull.sh can run
# both against directories it controls, with a local directory standing in for mongo-1:
#
#   SOURCE        rsync source of the dump directory, WITHOUT a trailing slash: `user@host:` for
#                 mongo-1 (rrsync confines every path to the dump directory, so `/` is its root),
#                 a plain path in tests
#   DESTINATION   where the copies live
#   KEEP          how many copies to keep, by count
#   METRICS_FILE  (metrics only) the .prom file to write
#   RSYNC_RSH     (optional) rsync's own variable: the ssh command, with key and known_hosts
set -euo pipefail

destination="${DESTINATION:?DESTINATION is not set}"

# A COMPLETED DUMP IS A DIRECTORY NAMED BY ITS UTC STAMP, AND NOTHING ELSE IS. mongo-1 writes each
# dump to `.partial-<stamp>` and renames it only once mongodump has exited 0, so an in-progress or a
# failed dump never matches this -- which is what lets this script copy "the newest one" without
# asking mongo-1 whether it is finished.
stamp_pattern='^[0-9]{8}T[0-9]{6}Z$'

# EPOCH SECONDS OF A `20261024T032039Z` STAMP, in shell arithmetic. Not `date -d`: GNU and BSD date
# disagree on how to parse anything, and the test runs on both. Howard Hinnant's days-from-civil.
stamp_epoch() {
  local s="$1" y m d era yoe mp doy doe
  y=$((10#${s:0:4})) m=$((10#${s:4:2})) d=$((10#${s:6:2}))
  ((m <= 2)) && y=$((y - 1))
  era=$((y / 400)) yoe=$((y - era * 400)) mp=$(((m + 9) % 12))
  doy=$(((153 * mp + 2) / 5 + d - 1))
  doe=$((yoe * 365 + yoe / 4 - yoe / 100 + doy))
  echo $(((era * 146097 + doe - 719468) * 86400 + 10#${s:9:2} * 3600 + 10#${s:11:2} * 60 + 10#${s:13:2}))
}

# The copies held here, oldest first. They sort chronologically because the stamp is ISO-8601 UTC.
held_copies() {
  local entry
  for entry in "$destination"/*/; do
    entry="${entry%/}"
    entry="${entry##*/}"
    [[ "$entry" =~ $stamp_pattern ]] && echo "$entry"
  done | sort
}

pull() {
  local source="${SOURCE:?SOURCE is not set}" keep="${KEEP:?KEEP is not set}" listing newest copies i
  ((keep >= 1)) || { echo "KEEP must be at least 1, got $keep" >&2; exit 2; }

  # `--list-only` of the source's top level: one line per entry, the name last. Taken on its own so
  # an unreachable mongo-1 fails here, as itself, rather than reading as "no dumps". `/.` AND NOT
  # `/`: rrsync refuses a bare `/` or `./` as an "unsafe arg" (it resolves to the restricted
  # directory itself), and admits `.`; the test runs this through a real rrsync to keep it so.
  listing="$(rsync --list-only "$source/.")"
  newest="$(awk '$1 ~ /^d/ { print $NF }' <<<"$listing" | { grep -E "$stamp_pattern" || true; } | sort | tail -n1)"
  if [[ -z "$newest" ]]; then
    echo "no completed dump at $source -- nothing to copy, and the copy held here is not refreshed" >&2
    exit 1
  fi

  if [[ -d "$destination/$newest" ]]; then
    echo "already holding $newest"
  else
    # THE SAME RENAME-ON-COMPLETION AS mongo-1's: a pull cut off half-way leaves `.partial-*`, which
    # is cleared here and is never counted, rotated past, or published as a copy.
    rm -rf "$destination"/.partial-*
    # 0700/0600 regardless of the source's modes: a database backup is as sensitive as the database.
    rsync -rt --chmod=Du=rwx,Dgo=,Fu=rw,Fgo= "$source/$newest/" "$destination/.partial-$newest/"
    mv "$destination/.partial-$newest" "$destination/$newest"
    echo "copied $newest"
  fi

  # ROTATION BY COUNT, NOT BY AGE, for roles/mongodb.nix's reason: an age-based prune deletes the
  # old copies whether or not new ones arrived, so a fortnight of failed pulls would end with
  # nothing here. Keeping the newest N cannot delete the last copy, whatever the timer has done.
  # (An index loop, not `head -n -N`: that is GNU-only, and the test runs on BSD too.)
  mapfile -t copies < <(held_copies)
  for ((i = 0; i < ${#copies[@]} - keep; i++)); do
    rm -rf -- "${destination:?}/${copies[i]}"
    echo "rotated out ${copies[i]}"
  done
}

metrics() {
  local metrics_file="${METRICS_FILE:?METRICS_FILE is not set}" newest size_bytes tmp
  newest="$(held_copies | tail -n1)"
  [[ -n "$newest" ]] || { echo "no copy held in $destination -- nothing to publish" >&2; exit 1; }
  size_bytes="$(du -sk "$destination/$newest" | cut -f1)"
  size_bytes=$((size_bytes * 1024))

  # Written beside the target and RENAMED: node_exporter reads the directory on every scrape, and a
  # half-written .prom is a parse error that discards the whole file.
  tmp="$metrics_file.$$"
  {
    echo "# HELP kinowo_mongodump_offbox_last_success_timestamp_seconds Unix time of the last off-box pull of mongo-1's dumps that completed."
    echo "# TYPE kinowo_mongodump_offbox_last_success_timestamp_seconds gauge"
    echo "kinowo_mongodump_offbox_last_success_timestamp_seconds $(date +%s)"
    echo "# HELP kinowo_mongodump_offbox_newest_dump_timestamp_seconds Unix time the newest off-box copy was dumped on mongo-1, from its directory stamp."
    echo "# TYPE kinowo_mongodump_offbox_newest_dump_timestamp_seconds gauge"
    echo "kinowo_mongodump_offbox_newest_dump_timestamp_seconds $(stamp_epoch "$newest")"
    echo "# HELP kinowo_mongodump_offbox_last_size_bytes Size on disk of the newest off-box copy."
    echo "# TYPE kinowo_mongodump_offbox_last_size_bytes gauge"
    echo "kinowo_mongodump_offbox_last_size_bytes $size_bytes"
    echo "# HELP kinowo_mongodump_offbox_copies Number of off-box copies held."
    echo "# TYPE kinowo_mongodump_offbox_copies gauge"
    echo "kinowo_mongodump_offbox_copies $(held_copies | wc -l | tr -d ' ')"
  } >"$tmp"
  mv "$tmp" "$metrics_file"
}

case "${1:-}" in
  pull) pull ;;
  metrics) metrics ;;
  *) echo "usage: $0 pull|metrics" >&2; exit 2 ;;
esac
