# VictoriaLogs -- this fleet's log store, on the same box as everything else that watches it.
#
# PORTED FROM bitcashier/infra's nix/modules/roles/victoria-logs.nix, which is where the flag set,
# the `RequiresMountsFor` guard and the `-delete.enable` reasoning all come from. What is NOT
# ported is stated at the bottom of this header, so that a reader comparing the two files can tell
# a deliberate difference from a missing line.
#
# ------------------------------------------------------------------------------------------------
# WHY A LOG STORE AT ALL, ON A FLEET THAT ALREADY HAS A PERSISTENT JOURNAL
# ------------------------------------------------------------------------------------------------
#
# modules/fleet/default.nix sets `services.journald.storage = "persistent"` and its header names
# that as "the whole of the log retention story ... deliberately the first thing to revisit when an
# incident here needs evidence from a host that has rebooted". This is that revisit. A per-host
# journal answers "what did THIS box do" only while you can still ssh to the box, and the two
# questions an incident on a three-machine fleet actually raises are "what did all three do in the
# same minute" and "what did the one that is now unreachable say last". Neither is answerable from
# a journal you have to log in to read.
#
# ------------------------------------------------------------------------------------------------
# IT HAS NO AUTHENTICATION, AND THAT IS THE WHOLE OF ITS ACCESS CONTROL STORY
# ------------------------------------------------------------------------------------------------
#
# VictoriaLogs ships no auth of its own -- upstream's answer is to put vmauth or a reverse proxy in
# front of it. There is none here, so the bind address IS the control: `fleet.privateAddress`, on
# an interface Hetzner's edge firewall does not filter at all, admitted by one rule in
# modules/fleet/firewall.nix. That is the same posture Prometheus and Alertmanager already have on
# this host and it has the same uncomfortable edge: "on the private network" includes every pod
# k3s will ever schedule on these two machines. Anything that reads this store reads every log line
# the fleet has produced, including whatever a service was careless enough to print.
#
# ------------------------------------------------------------------------------------------------
# DELIBERATELY NOT PORTED FROM bitcashier
# ------------------------------------------------------------------------------------------------
#
#   * THE CONSUL SERVICE REGISTRATION (`fleet.consul.services`, the `advertise` flag and the long
#     note about a flat name splitting fleet-wide ingest). There is no Consul here -- service
#     discovery on this fleet is three static addresses in a /24 -- so shippers name
#     `10.20.0.11:9428` directly and there is no second answer for them to be handed. The hazard
#     that flag existed for cannot occur; the hazard it is REPLACED by is that an address literal
#     outlives the machine it named, which is why modules/fleet/logs.nix reads this host's
#     `fleet.privateAddress` through the flake rather than writing 10.20.0.11 down again.
#   * THE PINNED uid/gid 704. That number exists there because a data directory carried over on a
#     volume was already owned by it, and that file states the rule as "a uid must match whatever
#     owns the bytes the machine is about to inherit". This volume inherits no bytes -- it is a
#     fresh ext4 whose only content was lost+found -- so pinning would buy nothing and could
#     collide with a NixOS static id. NixOS allocates.
#   * THE BACKUP TIMER (bc-backup-victoria-logs). Not ported, and worth naming as a gap rather than
#     leaving as an absence: if this volume is lost, the fleet's log history is lost with it. That
#     is an accepted trade for now -- logs here are evidence, not records anyone must keep -- but
#     it is a trade, not an oversight.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.victoriaLogs;
in
{
  options.fleet.victoriaLogs = {
    enable = lib.mkEnableOption "VictoriaLogs, this fleet's log store";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.victorialogs;
      defaultText = "pkgs.victorialogs";
      description = ''
        From the pinned nixpkgs, NOT a pinned package of its own. bitcashier pins several binaries
        (consul, vault, promtail) because it has to match a Puppet-managed half of the same estate
        version for version; there is no second half here, so whatever nixos-26.05 carries is by
        definition what this fleet runs, and the flake pin is the version pin.

        THE STORAGE FORMAT IS NOT A THING TO UPGRADE CASUALLY ANYWAY: VictoriaLogs supports
        upgrading in place but not DOWNGRADING across a format change, so a nixpkgs bump that moves
        this package is a one-way door for the data on the volume. That is an argument for reading
        the release notes at bump time, not for pinning here.
      '';
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = config.fleet.privateAddress;
      defaultText = "config.fleet.privateAddress";
      description = ''
        THE PRIVATE ADDRESS, AND NEVER 0.0.0.0 -- the same hard convention as Prometheus, Grafana
        and mongod on this fleet, and it carries more weight here than for any of them because this
        process has no login page to fail at. Its HTTP API serves `/select/logsql/query` (every log
        line the fleet has) to whoever can open the socket.
      '';
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 9428;
      description = ''
        VictoriaLogs' own default, kept, and the same port bitcashier's shippers push to -- so a
        runbook or a curl written for one fleet works on the other.
      '';
    };

    dataPath = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/monitoring/victoria-logs";
      description = ''
        ON THE MONITORING VOLUME, which is mounted at the PARENT (/var/lib/monitoring) -- the same
        shape as `fleet.prometheus.dataDir` and its `alertmanagerDataDir`, and the same shape
        bitcashier's VictoriaLogs has on its builds volume.

        THE HAZARD IS THE ONE roles/prometheus.nix AND roles/mongodb.nix BOTH STATE: with the
        volume absent this is an empty directory on the 40GB ROOT disk, and VictoriaLogs would
        start and write there perfectly happily -- no error, a working-looking log store, and the
        whole fleet's logs filling the root filesystem of the host that runs the alarm. The unit
        below carries `RequiresMountsFor` so it refuses to start instead. Refusing is the right
        failure: it is loud, and it is not data written somewhere nobody will look for it.
      '';
    };

    retentionPeriod = lib.mkOption {
      type = lib.types.str;
      default = "30d";
      description = ''
        HOW FAR BACK A QUERY CAN REACH, AS AN INTENT -- the same 30d bitcashier keeps, and for the
        same reason: the question logs get asked is "what happened during the incident", and
        incidents get investigated within days, not quarters. Metrics answer the
        "worse-than-last-quarter" question and roles/prometheus.nix keeps 90d for it.

        IT IS NOT WHAT BOUNDS THE DISK -- see `maxDiskSpaceUsage`, which is. Time-based retention
        says how OLD data may be, not how MUCH of it there may be, so one chatty new workload on
        k3s-worker-1 multiplies the space this uses with this value unchanged. Both are set for
        exactly that reason: only one of the two can be violated by deploying a pod.
      '';
    };

    maxDiskSpaceUsage = lib.mkOption {
      type = lib.types.str;
      default = "10GiB";
      description = ''
        THE HARD BOUND ON DISK, and the one of the two retention settings that a new log source
        cannot undo. When it bites, VictoriaLogs drops the OLDEST data -- so the failure is a
        shorter history, which is recoverable, rather than a full volume, which would take
        Prometheus's TSDB and Grafana's sqlite down beside it. On THIS host that is the whole
        argument: the monitoring volume is shared, and the process most likely to fill it is the
        one whose input is unbounded and comes from somewhere else.

        THE ARITHMETIC, ON A 40GB VOLUME THAT ALREADY HOLDS PROMETHEUS:

            ~38.0 GiB   usable (ext4 reserves ~5% for root)
            -20.0 GiB   fleet.prometheus.retentionSize, itself a hard bound
            -10.0 GiB   this
            ------------
             ~8.0 GiB   left for Prometheus's WAL and head block (NOT counted by
                        --storage.tsdb.retention.size), Grafana's sqlite, and slack

        EVERY NUMBER ABOVE EXCEPT THE VOLUME SIZE IS A BUDGET, NOT A MEASUREMENT. Nobody has run
        this fleet's real log volume through this store for a month; 10GiB is a deliberately
        conservative guess made against a shared 40GB disk, not a figure derived from an observed
        ingest rate. For scale, three hosts' systemd journals are a few hundred MB a day
        uncompressed and VictoriaLogs compresses aggressively, so 30d of journal alone should fit
        several times over -- what is genuinely unknown is what the app worker pod on
        k3s-worker-1 prints, because that is application output and it is the one input here that
        can change by an order of magnitude without anybody touching this repository.

        IT IS A THRESHOLD, NOT A HARD WALL, and the difference is documented: VictoriaLogs drops
        older per-day partitions when usage passes this, but it KEEPS AT LEAST THE LAST TWO DAYS
        whatever the setting says -- so a sudden flood can push real usage above 10GiB rather than
        failing to ingest. That is another reason the ~8GiB of slack above is deliberate and should
        not be spent: this bound protects the volume against a trend, not against a burst.

        THE RIGHT WAY TO REVISE IT is to read the store's own
        `vl_data_size_bytes` / `vl_storage_disk_space_usage_bytes` after a month of real ingest and
        compare it against `node_filesystem_avail_bytes` for /var/lib/monitoring -- not to raise it
        because a query returned less history than somebody hoped. If both this and Prometheus's
        bound need raising, the answer is a bigger volume, not a thinner margin: the two together
        must stay well under the disk or the shared-volume decision stops being safe.
      '';
    };

    memoryAllowedBytes = lib.mkOption {
      type = lib.types.str;
      default = "512MB";
      description = ''
        HOW MUCH RAM VictoriaLogs MAY BUDGET FOR ITSELF, and this is the option most worth reading
        on this particular host.

        VictoriaMetrics components size their caches from `-memory.allowedPercent`, WHICH DEFAULTS
        TO 60% OF SYSTEM MEMORY. monitoring-1 is a 4GB cx23 already carrying Prometheus,
        Alertmanager, Grafana and a k3s control plane, so the default would have this process plan
        around ~2.4GB it does not have. The failure that produces is not a slow log store: it is
        the kernel OOM killer choosing the largest process on a box where the largest process is
        supposed to be the one recording why things went wrong. CPUWeight does nothing about
        memory pressure -- roles/prometheus.nix says so in its own header -- so this flag is the
        only lever there is.

        512MB IS A GUESS, chosen as "clearly enough for three hosts' journals, clearly small
        enough not to matter against 4GB". It bounds the caches, not the process: Go's heap can
        still exceed it under a heavy merge. If ingestion errors or slow queries ever trace back to
        cache pressure, raise it against a measured `process_resident_memory_bytes`, and remember
        that every MB given here is one Prometheus does not have.
      '';
    };

    deletionEnabled = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether `-delete.enable` is passed, which exposes `POST /delete/run_task?filter=<logsql>`
        and lets stored log lines be erased.

        OFF, AND MEANT TO BE TURNED ON FOR ONE ERASURE AND OFF AGAIN -- ported verbatim in intent
        from bitcashier, including the reasoning: VictoriaLogs ships this disabled so that "an
        attacker cannot remove the existing logs", and that property is what makes the store a
        record rather than a scratchpad. Erasure also rewrites all stored logs, so it is not
        something to leave armed for convenience.

        WHAT IT IS FOR: the case retention cannot answer. Something gets logged that must not stay
        logged -- a credential printed by a service, a personal detail in a request body -- and
        "wait 30 days" is not an answer. bitcashier had exactly that happen and its file records
        the incident; NOTHING OF THE KIND HAS HAPPENED ON THIS FLEET. The mechanism is ported
        because the risk is generic to any log store, not because there is a leak here to clean up.

        RUNBOOK, deliberately three steps rather than a flag left on:
          1. set this true for monitoring-1, merge, let the closure be applied;
          2. count first, erase, count again -- the filter is the ONLY thing standing between one
             bad query and the whole store:
               curl -s http://10.20.0.11:9428/select/logsql/stats_query \
                 --data-urlencode 'query=`"password":"` | count()' --data-urlencode start=30d
               curl -s -XPOST http://10.20.0.11:9428/delete/run_task \
                 --data-urlencode 'filter=`"password":"`'
               curl -s http://10.20.0.11:9428/delete/active_tasks
          3. set it false again and merge.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.victorialogs = {
      isSystemUser = true;
      group = "victorialogs";
      home = cfg.dataPath;
      description = "VictoriaLogs";
    };
    users.groups.victorialogs = { };

    # CREATE THE DIRECTORY ON THE VOLUME, OWNED BY THE SERVICE USER. Exactly the rule
    # roles/prometheus.nix carries and for exactly the reason written there: `StateDirectory=`
    # cannot help, because it creates /var/lib/<name> and this lives at a NESTED path on a SEPARATE
    # MOUNT, which systemd will not create on a unit's behalf. A FRESH ext4 COMES UP ROOT-OWNED
    # WITH NOTHING BUT lost+found, so without this the service user has nowhere it may write.
    #
    # AND THE FAILURE DOES NOT NAME THE PROBLEM. With `ProtectSystem=strict` and a `ReadWritePaths`
    # naming a directory that is not there, the unit dies at `status=226/NAMESPACE` with "Failed to
    # set up mount namespacing" -- which reads as a systemd hardening fault, not a missing folder.
    # That is what happened to Prometheus on monitoring-1's first deploy (2026-08-29) and is why
    # that file says so at length; this is the same trap on the same volume.
    #
    # 0700 and owned by victorialogs: on this host "anything else on the box" includes a k3s
    # control plane and whatever it runs, and this directory holds every log line the fleet has.
    systemd.tmpfiles.rules = [
      "d ${cfg.dataPath} 0700 victorialogs victorialogs -"
    ];

    systemd.services.victoria-logs = {
      description = "VictoriaLogs";
      documentation = [ "https://docs.victoriametrics.com/victorialogs/" ];
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      # SEE `dataPath` -- the volume is the whole reason this line exists. Ported from bitcashier's
      # unit, which carries it because its storage path is also a directory INSIDE a mount rather
      # than the mount itself, so nothing about the path's existence tells you the disk is there.
      unitConfig.RequiresMountsFor = builtins.dirOf cfg.dataPath;

      serviceConfig = {
        Type = "simple";
        User = "victorialogs";
        Group = "victorialogs";

        # APPENDED TO THE LIST, INSIDE the concatStringsSep call. Function application binds tighter
        # than `++`, so appending outside it concatenates onto the finished STRING and fails to
        # evaluate. Appended rather than interpolated as a conditional empty string, too: a stray
        # "" would be an empty argv entry rather than nothing, and VictoriaLogs refuses to start on
        # one. (Both learned in the bitcashier file this is ported from.)
        ExecStart = lib.concatStringsSep " " ([
          "${cfg.package}/bin/victoria-logs"
          "-httpListenAddr=${cfg.listenAddress}:${toString cfg.port}"
          "-storageDataPath=${cfg.dataPath}"
          "-retentionPeriod=${cfg.retentionPeriod}"
          "-retention.maxDiskSpaceUsageBytes=${cfg.maxDiskSpaceUsage}"
          "-memory.allowedBytes=${cfg.memoryAllowedBytes}"
        ] ++ lib.optional cfg.deletionEnabled "-delete.enable");

        Restart = "on-failure";
        RestartSec = "5s";

        # A FILE PER ACTIVE STREAM PLUS ITS OWN MMAPPED INDEX. The default 1024 is well under what
        # even three hosts open once pod logs are in the mix, and running out presents as INGESTION
        # ERRORS rather than a crash -- a store that is up, answers queries, and is quietly not
        # accepting some of what is pushed at it. Worth setting rather than discovering. Ported.
        LimitNOFILE = 131072;

        # NO CPUWeight, AND THAT IS THE DECISION RATHER THAN AN OMISSION -- systemd's default of
        # 100, which leaves this BELOW Prometheus (400) and Alertmanager (300) on the same box.
        # roles/prometheus.nix argues that when this host is starved the thing that must keep
        # working is the thing that RECORDS the starvation; that argument does not extend to this
        # process, because the difference in kind is that a scrape gap is PERMANENT while a log
        # line that cannot be ingested this second is still sitting in the shipper's disk buffer a
        # minute later (see modules/fleet/logs.nix). Losing throughput here is recoverable; losing
        # it in Prometheus is not.
        #
        # k3s is at the default too, deliberately, and k3s-server.nix says so on its own unit.

        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        # Named explicitly so `ProtectSystem=strict` cannot be quietly defeated by a later change
        # of `dataPath` that forgets to update the unit.
        ReadWritePaths = [ cfg.dataPath ];
      };
    };

    # THE PORT, VIA THE FLEET'S ONE FIREWALL FILE. `privateTCPPorts` rather than
    # `networking.firewall.interfaces.<name>` written here, because that option exists for exactly
    # this: the interface name is `fleet.privateInterface`, and a role that spelled it out would
    # open NOTHING on a host whose private NIC is called something else -- iptables accepts a rule
    # naming a nonexistent interface, installs it, prints it correctly, and never matches it.
    #
    # NOT a named `fleet.firewall.*` intent like `monitoring`, which would be the tidier home for
    # it: adding one means editing modules/fleet/firewall.nix, and this change was made while
    # another change was in flight in that file. IT SHOULD BECOME ONE -- either folded into
    # `monitoring` beside 9090/9093/3000, which is where a reader will look for it, or its own
    # `logs` flag. Until then this is the escape hatch doing what the escape hatch is for, and 9428
    # is a port number in a role file rather than in the fleet's one readable list, which is the
    # thing that file exists to avoid.
    fleet.firewall.privateTCPPorts = [ cfg.port ];
  };
}
