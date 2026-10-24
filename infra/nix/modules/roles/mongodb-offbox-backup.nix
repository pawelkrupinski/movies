# THE OFF-BOX COPY OF mongo-1's NIGHTLY DUMP -- the gap roles/mongodb.nix's header used to call
# "the largest known gap in this role".
#
# WHAT IT IS: a timer on a host that is NOT mongo-1 which pulls the newest completed dump over ssh,
# keeps its own N copies by count, and publishes when it last succeeded. The logic is
# ../../files/mongodb-offbox-pull.sh, kept as a file so ../../../test/test_mongodb_offbox_pull.sh
# can run it against directories it controls.
#
# WHY A PULL. A push would need mongo-1 to hold a credential that can WRITE here, and then a
# compromised mongo-1 could delete both copies -- the one failure an off-box copy exists for. Pulled,
# mongo-1 holds nothing for this host, and the key this host holds can only run `rrsync -ro` over
# mongo-1's dump directory (fleet.mongodb.backup.offBoxPull). The worst a compromised mongo-1 can
# do is stop offering new dumps, which MongodumpOffboxStale reports.
#
# WHY monitoring-1 (2026-10-24). The two candidates were monitoring-1 and k3s-worker-1. The dump is
# ~575MB (kinowo_mongodump_last_size_bytes), seven copies ~4GB, and monitoring-1's ROOT disk has
# ~27GB free of 38GB; the worker has more disk, but it runs the internet-facing application pods
# and is the most exposed machine in the fleet (.sops.yaml says the same about its secrets), so a
# container escape there would reach the copies. monitoring-1 runs no application workload (it is
# the tainted k3s server), is already where the operator's tooling lives, and its root disk is
# inside Hetzner's server backups -- which mongo-1's data volume is not -- so the copy is also
# imaged nightly into a third place.
#
# THE ROOT DISK, NOT /var/lib/monitoring. That volume's space is budgeted to the byte between
# Prometheus and VictoriaLogs (roles/victoria-logs.nix `maxDiskSpaceUsage`), and is not in
# Hetzner's backups. filesystem-capacity rules watch `/` already.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.mongodbOffboxBackup;

  pullScript = pkgs.writeShellScript "kinowo-mongodb-offbox-pull"
    (builtins.readFile ../../files/mongodb-offbox-pull.sh);

  # THE SOURCE'S HOST KEY, PINNED. `StrictHostKeyChecking=yes` against this one line means a machine
  # answering on mongo-1's address with any other key gets nothing -- no trust-on-first-use, which
  # on an unattended timer would be trust-whatever-answers. It changes when mongo-1 is re-converted
  # (nixos-anywhere mints a new host key, see infra/.sops.yaml), and the symptom is this unit
  # failing with "Host key verification failed" and MongodumpOffboxStale a day and a half later.
  knownHosts = pkgs.writeText "mongodb-offbox-known-hosts" "${cfg.source.address} ${cfg.source.hostKey}\n";

  sshCommand = lib.concatStringsSep " " [
    "${pkgs.openssh}/bin/ssh"
    "-i ${config.sops.secrets."mongodb-offbox/ssh-key".path}"
    "-o IdentitiesOnly=yes"
    "-o BatchMode=yes"
    "-o StrictHostKeyChecking=yes"
    "-o UserKnownHostsFile=${knownHosts}"
    "-o GlobalKnownHostsFile=/dev/null"
    "-o ConnectTimeout=30"
  ];

  environment = {
    SOURCE = "${cfg.source.user}@${cfg.source.address}:";
    DESTINATION = cfg.directory;
    KEEP = toString cfg.keep;
    METRICS_FILE = "${cfg.textfileDirectory}/mongodump-offbox.prom";
    RSYNC_RSH = sshCommand;
  };
in
{
  options.fleet.mongodbOffboxBackup = {
    enable = lib.mkEnableOption "the off-box pull of mongo-1's nightly mongodump";

    source = {
      address = lib.mkOption {
        type = lib.types.str;
        example = "10.20.0.13";
        description = ''
          mongo-1's PRIVATE address. The dump never crosses the public internet, and ssh on the
          private interface is open between siblings (modules/fleet/firewall.nix).
        '';
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = "mongodb-offbox";
        description = "Must equal fleet.mongodb.backup.offBoxPull.user on the source.";
      };

      hostKey = lib.mkOption {
        type = lib.types.str;
        example = "ssh-ed25519 AAAA...";
        description = ''
          The source's /etc/ssh/ssh_host_ed25519_key.pub, without its comment. Read it back with
          `ssh-keyscan -t ed25519 <address>` from a sibling.
        '';
      };
    };

    directory = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/mongodb-offbox";
      description = "Where the copies live. On the root disk -- see this file's header.";
    };

    keep = lib.mkOption {
      type = lib.types.int;
      default = 7;
      description = ''
        How many copies to keep, BY COUNT, matching fleet.mongodb.backup.keep: a week of logical
        history, the window in which a data bug is usually noticed. See the script for why a count.
      '';
    };

    onCalendar = lib.mkOption {
      type = lib.types.str;
      default = "05:00";
      description = ''
        After mongo-1's 03:20 dump (+ up to 20 minutes' randomised delay; a dump takes about a
        minute today, and its timeout is two hours). A pull that runs before the dump finishes is
        not wrong -- it takes the previous night's, already held -- it is only a day late.
      '';
    };

    textfileDirectory = lib.mkOption {
      type = lib.types.str;
      default = config.fleet.observability.textfileDirectory;
      defaultText = "config.fleet.observability.textfileDirectory";
      description = "Read from the fleet option for the reason roles/mongodb.nix gives for its own.";
    };
  };

  config = lib.mkIf cfg.enable {
    # THE PRIVATE HALF of the key whose public half is fleet.mongodb.backup.offBoxPull.authorizedKey
    # on mongo-1. Written into nix/secrets/monitoring-1.yaml by infra/bin/provision-mongodb-offbox-key,
    # which is also how to rotate it. ssh refuses a private key readable by anyone but its owner.
    sops.secrets."mongodb-offbox/ssh-key" = { owner = "mongodb-offbox"; mode = "0400"; };

    users.groups.mongodb-offbox = { };
    users.users.mongodb-offbox = {
      isSystemUser = true;
      group = "mongodb-offbox";
      description = "Pulls mongo-1's dumps; holds the off-box copies";
    };

    # 0700: a database backup is exactly as sensitive as the database.
    systemd.tmpfiles.rules = [ "d ${cfg.directory} 0700 mongodb-offbox mongodb-offbox -" ];

    systemd.services.mongodb-offbox-pull = {
      description = "Pull mongo-1's newest mongodump off the box, with rotation";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      unitConfig.RequiresMountsFor = [ cfg.directory ];
      inherit environment;
      path = [ pkgs.rsync pkgs.coreutils pkgs.gawk pkgs.gnugrep ];
      serviceConfig = {
        Type = "oneshot";
        User = "mongodb-offbox";
        Group = "mongodb-offbox";
        ExecStart = "${pullScript} pull";
        # `+`: as root, ignoring User= -- the textfile directory is root-owned and shared by every
        # publisher on the host; roles/mongodb.nix's publishDumpMetrics explains the same choice.
        # ExecStartPost runs only if the pull succeeded, so the timestamp means "a pull completed".
        ExecStartPost = "+${pullScript} metrics";
        # Today's dump crosses the private network in seconds; an hour is room for growth, not a
        # target. If it is ever hit, look at why.
        TimeoutStartSec = "1h";
        Nice = 10;
        IOSchedulingClass = "idle";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.directory cfg.textfileDirectory ];
      };
    };

    systemd.timers.mongodb-offbox-pull = {
      description = "Daily off-box pull of mongo-1's mongodump";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.onCalendar;
        RandomizedDelaySec = "10m";
        # A missed run is run late rather than skipped, for roles/mongodb.nix's reason.
        Persistent = true;
      };
    };
  };
}
