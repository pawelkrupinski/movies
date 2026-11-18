# PROMETHEUS'S PERSISTED BLOCKS, OFFERED READ-ONLY TO ci-runner FOR THE ALERT BACKTEST.
#
# infra/bin/backtest-alerts used to ask THIS Prometheus for 14 days of every rule over an ssh/SOCKS
# tunnel from the laptop: every full run hit its 60-70 min cap, heavy rules took 45-119 s a query,
# and live rule evaluation went from 3 s to 21 s while it ran (owner, 2026-11-18). Now ci-runner
# (bitcashier infra, hosts/ci-runner/prometheus-snapshot.nix) copies the blocks nightly and replays
# them on a Prometheus of its own; this host only serves files.
#
# WHAT IS OFFERED. One account, `prometheus-snapshot`, whose single key can only run
# `rrsync -ro` over ${exportDir}:
#
#   tsdb/            a READ-ONLY bind of Prometheus's data directory. The puller copies only the
#                    persisted, immutable ULID block directories from it, never the WAL, the head
#                    chunks or the lock (its header says how it tells them apart).
#   rules.json       /api/v1/rules as this Prometheus had them loaded, refreshed hourly, so the
#   buildinfo.json   backtest's "deployed?" and "stale recording?" checks read what PRODUCTION ran,
#                    and its replay can refuse a Prometheus of another version.
#
# NO ADMIN API, NO SNAPSHOT, NO RESTART. The admin API would also expose delete_series on the only
# copy of this history to the whole private network (roles/prometheus.nix says why it stays off),
# and a hard-linked snapshot would pin compacted-away blocks on a volume 20 GB of whose 40 GB the
# TSDB already uses. Persisted blocks need neither: Prometheus writes each one whole and renames it
# into place, and never changes it after.
#
# THE DATA DIRECTORY STAYS 0700 (roles/prometheus.nix). This account reads it through one ACL entry
# on that directory alone; the block directories inside are 0755 already.
#
# THE PULL IS CHEAP FOR THIS HOST BY CONSTRUCTION: rrsync runs under `ionice -c3` and `nice 19`, so
# a copy yields the disk and CPU to Prometheus's own evaluation, and after the first night it sends
# only blocks created since (a few hundred MB).
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.prometheusBlockExport;
  prometheus = config.fleet.prometheus;
  exportDir = cfg.directory;

  # The hourly capture of what this Prometheus has loaded. Written to a temporary name and renamed,
  # so a pull never reads half a file.
  capture = pkgs.writeShellScript "prometheus-export-rules" ''
    set -euo pipefail
    for name in rules:api/v1/rules buildinfo:api/v1/status/buildinfo; do
      file="${exportDir}/''${name%%:*}.json"
      ${pkgs.curl}/bin/curl -fsS --max-time 60 "http://${prometheus.listenAddress}:9090/''${name#*:}" >"$file.tmp"
      ${pkgs.jq}/bin/jq -e '.status == "success"' "$file.tmp" >/dev/null
      mv "$file.tmp" "$file"
    done
  '';
in
{
  options.fleet.prometheusBlockExport = {
    enable = lib.mkEnableOption "the read-only export of Prometheus's persisted blocks to ci-runner";

    directory = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/monitoring/prometheus-export";
      description = "rrsync's root: `tsdb/` (the read-only bind) and the two captured JSON files.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "prometheus-snapshot";
      description = "The pull account; the ssh user of ci-runner's puller.";
    };

    authorizedKey = lib.mkOption {
      type = lib.types.str;
      default = lib.fileContents ../../files/prometheus-snapshot-pull.pub;
      defaultText = "contents of nix/files/prometheus-snapshot-pull.pub";
      description = ''
        The public half of ci-runner's pull key. Minted by bitcashier infra
        bin/provision-prometheus-snapshot-key, which seals the private half into ci-runner's sops
        file and writes this one into nix/files/prometheus-snapshot-pull.pub.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [{
      assertion = prometheus.enable;
      message = "fleet.prometheusBlockExport exports fleet.prometheus's data directory; enable that first.";
    }];

    # A real shell because sshd runs the forced command through it (roles/mongodb.nix, the off-box
    # puller's account, says the same). `command=` replaces whatever the client asked for.
    users.groups.${cfg.user} = { };
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.user;
      shell = pkgs.bashInteractive;
      description = "ci-runner's read-only pull of Prometheus's persisted blocks";
      openssh.authorizedKeys.keys = [
        (lib.concatStringsSep " " [
          ''command="${pkgs.util-linux}/bin/ionice -c3 ${pkgs.coreutils}/bin/nice -n19 ${pkgs.rrsync}/bin/rrsync -ro ${exportDir}",restrict''
          cfg.authorizedKey
        ])
      ];
    };

    systemd.tmpfiles.rules = [
      "d ${exportDir} 0750 ${cfg.user} ${cfg.user} -"
      # Traverse-and-list on the data directory for this account only; the mode stays 0700.
      "a+ ${prometheus.dataDir} - - - - user:${cfg.user}:r-x"
    ];

    fileSystems."${exportDir}/tsdb" = {
      device = prometheus.dataDir;
      fsType = "none";
      options = [ "bind" "ro" ];
    };

    systemd.services.prometheus-export-rules = {
      description = "Capture the rules and build info Prometheus has loaded, for ci-runner's backtest";
      after = [ "prometheus.service" ];
      unitConfig.RequiresMountsFor = [ exportDir ];
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.user;
        ExecStart = capture;
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ exportDir ];
      };
    };

    systemd.timers.prometheus-export-rules = {
      description = "Hourly capture of Prometheus's loaded rules";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "hourly";
        RandomizedDelaySec = "5m";
        Persistent = true;
      };
    };
  };
}
