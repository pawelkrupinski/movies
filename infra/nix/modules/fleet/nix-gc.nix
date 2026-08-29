# Bound the system generations and collect the nix store.
#
# PORTED FROM bitcashier's `nix/modules/fleet/nix-gc.nix`, INCLUDING THE INCIDENT THAT PRODUCED IT,
# because that incident is a property of the DEPLOY MODEL rather than of that fleet -- and this
# fleet has the same deploy model.
#
# WHAT HAPPENED THERE, 2026-08-28: a control-plane host reached 100% INODE exhaustion on / with 19G
# of bytes still free, after 84 system generations accumulated in six days. At zero inodes a
# machine keeps routing traffic and answering health checks while every process that tries to
# CREATE a file fails -- so it looks healthy from outside and cannot write its own logs. It was
# found by hand. Measured across all 26 machines the same day, the `nix-gc` timer count was ZERO on
# every one of them and five hosts were already between 79% and 86% inode use.
#
# WHY IT APPLIES HERE. `.github/workflows/nix-stage-closures.yaml` copies a full system closure
# onto all three hosts on every merge that touches infra/nix, and ./auto-apply.nix activates the
# inert ones. Generations therefore arrive at the rate of MERGES. Nothing in nixpkgs collects them:
# `nix.gc` is off by default, and a NixOS host will fill its disk with its own history indefinitely
# while reporting nothing.
#
# WHY BOUNDING THE COUNT AND NOT THE AGE, which is what `nix.gc.options = "--delete-older-than 7d"`
# would do and is what everybody reaches for first. Age is the wrong instrument when the RATE is
# the problem: 84 generations in six days means a seven-day retention would have held around ninety
# and killed the host just the same. `--delete-generations +N` keeps the newest N whatever the
# merge rate, which is the only bound that holds when the pipeline gets busy -- and a busy pipeline
# is exactly when a host is least able to survive it.
#
# THE AGE BACKSTOP STAYS ANYWAY, second and subordinate: a machine that stopped receiving closures
# months ago should still shed old ones, and the count bound never reaches that case.
#
# WHY THIS IS SAFE ALONGSIDE STAGING, which is the one thing that could have made it dangerous. A
# staged closure is NOT garbage: bin/stage-nixos-closures pins it at
# /var/lib/nixdeploy/staged-system and ./deploy-staging.nix keeps it from the collector with the
# indirect root /nix/var/nix/gcroots/auto/nixdeploy-staged, which nix follows. The running system
# is a root too, so `switch-to-configuration --rollback` keeps as many targets as
# `keepGenerations`. If either of those two roots is ever removed, this timer is what deletes the
# thing that was staged -- so they are one mechanism, not two.
#
# AND IT PUBLISHES WHETHER IT RAN, because a collector that silently stopped is indistinguishable
# from a fleet with room to spare -- which is the whole shape of the incident above.
# `fleet_nix_gc_last_success_timestamp_seconds` is what says so; an ABSENT gauge means it has never
# once succeeded on this host, which is a different and worse claim than an old one.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.nixGc;
  textfileDir = config.fleet.observability.textfileDirectory;

  script = pkgs.writeShellApplication {
    name = "fleet-nix-gc";
    runtimeInputs = with pkgs; [ coreutils config.nix.package findutils ];
    text = ''
      metrics_file="''${METRICS_FILE:?METRICS_FILE is not set}"
      keep="''${KEEP_GENERATIONS:?KEEP_GENERATIONS is not set}"
      max_age="''${MAX_AGE:?MAX_AGE is not set}"
      profile=/nix/var/nix/profiles/system

      before_paths=$(find /nix/store -maxdepth 1 -mindepth 1 | wc -l)

      # THE COUNT BOUND, and it runs FIRST so the collection below has something to collect.
      # `+N` is nix's own spelling for "all but the newest N".
      nix-env -p "$profile" --delete-generations "+''${keep}"

      # The age backstop, and then the sweep. Both are no-ops when nothing is unreferenced.
      nix-collect-garbage --delete-older-than "$max_age" >/dev/null

      after_paths=$(find /nix/store -maxdepth 1 -mindepth 1 | wc -l)
      generations=$(find "$(dirname "$profile")" -maxdepth 1 -name 'system-*-link' | wc -l)

      # WRITTEN LAST AND THROUGH A TEMPORARY, so a half-written file is never scraped as current --
      # node_exporter does not ignore a truncated textfile, it fails to parse it and drops EVERY
      # series in it. And written only on the success path, so the timestamp means "this ran and
      # finished", which is the only reading that makes an alert on it worth anything.
      tmp="''${metrics_file}.tmp"
      {
        echo "# HELP fleet_nix_gc_last_success_timestamp_seconds When this host last completed a nix store collection."
        echo "# TYPE fleet_nix_gc_last_success_timestamp_seconds gauge"
        echo "fleet_nix_gc_last_success_timestamp_seconds $(date +%s)"
        echo "# HELP fleet_nix_gc_system_generations System generations remaining after the collection."
        echo "# TYPE fleet_nix_gc_system_generations gauge"
        echo "fleet_nix_gc_system_generations ''${generations}"
        echo "# HELP fleet_nix_gc_store_paths Store paths remaining after the collection."
        echo "# TYPE fleet_nix_gc_store_paths gauge"
        echo "fleet_nix_gc_store_paths ''${after_paths}"
        echo "# HELP fleet_nix_gc_store_paths_deleted Store paths removed by the last collection."
        echo "# TYPE fleet_nix_gc_store_paths_deleted gauge"
        echo "fleet_nix_gc_store_paths_deleted $(( before_paths - after_paths ))"
      } > "$tmp"
      mv "$tmp" "$metrics_file"
      chmod 0644 "$metrics_file"
    '';
  };
in
{
  options.fleet.nixGc = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        DEFAULT ON, and that is the point of putting it here rather than in each host. A machine
        that has to opt in is a machine somebody forgets, and the failure it opts out of is silent
        until the disk is already full.
      '';
    };

    keepGenerations = lib.mkOption {
      type = lib.types.ints.positive;
      default = 10;
      description = ''
        How many system generations survive. Ten is a rollback depth nobody has ever needed more
        than a couple of, against a bound that held bitcashier's worst host at 15% inode use where
        84 generations had put it at 100%.

        UNIFORM ACROSS THE FLEET ON PURPOSE. Tuning it per host would mean knowing which disk tier
        a machine is in before knowing whether it is safe, and the small-root hosts are exactly the
        ones that die -- so the host that most needs the bound is the one whose exemption would be
        easiest to write.
      '';
    };

    maximumAge = lib.mkOption {
      type = lib.types.str;
      default = "30d";
      description = ''
        The subordinate age backstop, for a machine that stopped receiving closures entirely and so
        never reaches the count bound. NOT the primary control -- see this file's header for why an
        age bound alone would not have saved the host that produced it.
      '';
    };

    dates = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = "systemd calendar expression for the collection.";
    };
  };

  config = lib.mkIf cfg.enable {
    # STORE OPTIMISATION, as a second timer rather than as part of the collection above. It
    # hard-links identical files between store paths, which is where the win is on a fleet that
    # keeps ten generations of nearly-identical closures -- most of the bytes in generation N are
    # byte-identical to generation N-1.
    #
    # SEPARATE FROM THE COLLECTOR BECAUSE THE TWO FAIL DIFFERENTLY. Optimising walks the whole
    # store and is slow and interruptible with no consequence; collecting deletes and must finish
    # to be meaningful. Folding them into one unit would mean a slow optimise pass eating the
    # collector's timeout, and the collector is the one that prevents an outage.
    #
    # `nix.optimise` rather than `nix.settings.auto-optimise-store`, deliberately: the latter
    # optimises on every store WRITE, which puts the linking cost inside every `nix copy` the
    # staging job makes -- turning a deploy's own latency into a function of how big the store has
    # got. A nightly pass has the same effect on disk and none on the deploy.
    nix.optimise = {
      automatic = true;
      dates = [ "03:45" ];
    };

    systemd.services.fleet-nix-gc = {
      description = "Bound the system generations and collect the nix store";
      serviceConfig = {
        Type = "oneshot";
        # Root, because the profile and the store are root's and the textfile directory is writable
        # by nobody else.
        User = "root";
        # Generous: a first collection on a neglected host deleted 2495 paths and 5.1 GiB on the
        # fleet this was ported from, and a collection killed half way leaves the store consistent
        # but the metric UNWRITTEN -- which reads as "never succeeded" and is worse than letting it
        # finish.
        TimeoutStartSec = "2h";
        Environment = [
          "METRICS_FILE=${textfileDir}/fleet-nix-gc.prom"
          "KEEP_GENERATIONS=${toString cfg.keepGenerations}"
          "MAX_AGE=${cfg.maximumAge}"
        ];
        ExecStart = lib.getExe script;
        # Collection is I/O heavy and never urgent. Losing the race to anything the machine
        # actually serves -- mongod's checkpoint, Prometheus's WAL -- is the correct outcome, every
        # time.
        IOSchedulingClass = "idle";
        CPUSchedulingPolicy = "idle";
      };
    };

    systemd.timers.fleet-nix-gc = {
      description = "Bound the system generations and collect the nix store";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.dates;
        # JITTER, because mongo-1 and monitoring-1 are both in nbg1 and may well share a
        # hypervisor; three hosts doing heavy I/O in the same minute is a self-inflicted spike on
        # the one of them that also serves the database.
        RandomizedDelaySec = "1h";
        # PERSISTENT ON PURPOSE, and it has a deliberate first effect: a host that missed its
        # window collects at boot, and a host that has never collected at all does so shortly after
        # taking this module. On the fleet this was ported from, that first run WAS the repair
        # rather than merely the schedule starting.
        Persistent = true;
      };
    };
  };
}
