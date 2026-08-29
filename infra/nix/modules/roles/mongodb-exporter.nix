# mongodb_exporter -- the thing that lets an alert ask mongod how it is, rather than asking systemd
# whether it is running.
#
# ------------------------------------------------------------------------------------------------
# WHY THIS EXISTS: `active` IS NOT `PRIMARY`, AND THE DIFFERENCE IS INVISIBLE FROM OUTSIDE
# ------------------------------------------------------------------------------------------------
#
# Everything this fleet knew about mongod until now came through node_exporter's systemd collector:
# `node_systemd_unit_state{name="mongodb.service"}`. That series answers exactly one question --
# has the process exited -- and mongod's worst states do not involve exiting. A member that has
# stepped down, or that cannot see its own replica-set configuration, keeps its unit `active`,
# keeps answering reads, and refuses every write. `MongodNotRunning` in mongodb.rules stays silent
# throughout.
#
# THAT MATTERS HERE MORE THAN IT WOULD ELSEWHERE, because of what the application does with the
# replica set. The web tier and the worker open CHANGE STREAMS, which exist only because there is
# an oplog, and which need a healthy set to stay open. Losing them does not take the site down --
# the Fly-hosted read tier keeps serving its projected read model out of memory, health checks keep
# passing, and the content quietly ages. roles/mongodb.nix's header describes that failure at
# length and could do nothing about detecting it; this role is the missing half.
#
# ------------------------------------------------------------------------------------------------
# WHAT IT COSTS, STATED SO THE TRADE IS LEGIBLE
# ------------------------------------------------------------------------------------------------
#
# A second process on the database box, a Mongo user that can read `serverStatus` and
# `replSetGetStatus`, and a password in sops. mongo-1 is a 2-core 4GB cx23 whose memory budget is
# already spoken for (see `wiredTigerCacheSizeGB`), so the exporter is deliberately configured with
# the two collectors the alerts need and NOT with `--collect-all`: the per-collection and
# per-index collectors multiply series by the number of collections in every database, and this
# fleet's Prometheus has a hard 20GB retention cap that a cardinality mistake spends silently.
#
# THE USER IS NOT CREATED BY THIS ROLE, for the same reason `backup.username` is not: creating it
# means a `db.createUser()` against a running mongod, which is a one-time step alongside
# `rs.initiate()` and not something to attempt on every activation. The exact command is in
# `username`'s description below, and the password it is given must be sealed into
# nix/secrets/mongo-1.yaml as `mongodb/exporter-password`. Until both are done this unit starts,
# fails to authenticate, and publishes `mongodb_up 0` -- which is loud, and is the direction this
# should fail in.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.mongodbExporter;
in
{
  options.fleet.mongodbExporter = {
    enable = lib.mkEnableOption "mongodb_exporter, so alerts can see inside mongod";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.prometheus-mongodb-exporter;
      defaultText = "pkgs.prometheus-mongodb-exporter";
      description = ''
        Percona's mongodb_exporter, pinned by this flake's nixpkgs lock like everything else here --
        there is no image tag to forget to pin because there is no image.

        THE BINARY IS `mongodb_exporter`, WITH AN UNDERSCORE, while the nixpkgs attribute is
        hyphenated. Said out loud because the ExecStart below is the one place that has to spell it
        the upstream way, and a wrong guess there is a build failure rather than a silent one.
      '';
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 9216;
      description = ''
        9216 is the exporter's registered default and is what every piece of documentation about it
        assumes. Changing it means changing `fleet.prometheus.mongodbExporterTargets` on
        monitoring-1 in the same commit; split across two, the target simply sits down and reads as
        a machine problem.
      '';
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = config.fleet.privateAddress;
      defaultText = "config.fleet.privateAddress";
      description = ''
        THE PRIVATE ADDRESS, AND NEVER 0.0.0.0. This exporter has no authentication and its /metrics
        page is a detailed description of the database -- database names, collection counts,
        connection counts, replica-set membership. The assertion below refuses anything that is not
        this host's private address or loopback, rather than trusting review.

        THE BIND IS THE ACCESS CONTROL, not the firewall rule, exactly as
        modules/fleet/observability.nix argues for node_exporter: the firewall opens the port on the
        private interface and Hetzner's edge never names it, but the thing that makes a wildcard
        bind impossible to reach from the internet is that there is no listener there.
      '';
    };

    mongoHost = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = ''
        LOOPBACK, for the same reason `fleet.mongodb.backup.host` is loopback: the exporter runs on
        the database's own machine and has no reason to put its credential or its query traffic on
        a wire. It also means the exporter keeps working during any maintenance window that drops
        mongod back to a loopback-only bind (`fleet.mongodb.bootstrapMode` does exactly that).
      '';
    };

    username = lib.mkOption {
      type = lib.types.str;
      default = "kinowo_monitor";
      description = ''
        The exporter's own Mongo user, which should hold `clusterMonitor` on admin AND NOTHING ELSE.

        A SEPARATE USER FROM BOTH THE APPLICATION'S AND THE BACKUP'S, deliberately. The application
        can write and the backup can read every document in every database; a metrics scraper needs
        neither, and a credential that sits in a process listening on a network port should be the
        least useful one on the box.

        THE ROLE IS `clusterMonitor` PRECISELY, and it is worth knowing what that covers because it
        is what makes the oplog rules possible: as well as `serverStatus` and `replSetGetStatus` it
        grants read on the `local` and `config` databases, and `local.oplog.rs` is where the oplog
        window is measured. A user given only `read` on admin would authenticate, scrape, and
        publish a subset of the series with no error anywhere.

        THE USER IS NOT CREATED BY THIS ROLE -- see the module header. Once, against a running
        mongod, as a user that can grant roles:

            use admin
            db.createUser({
              user: "kinowo_monitor",
              pwd: "<a long random password, no @ : / ? # [ ] characters>",
              roles: [ { role: "clusterMonitor", db: "admin" } ]
            })

        THE PASSWORD CHARACTER RESTRICTION IS NOT FUSSINESS. The credential reaches the exporter
        inside a MongoDB connection URI (see `environmentFile`), and a URI is parsed before it is
        authenticated -- so a password containing a URI delimiter does not fail with "bad
        password", it fails with a parse error naming a host that does not exist, or worse,
        silently truncates. Generate it with `openssl rand -hex 32` and the question does not arise.
      '';
    };

    environmentFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.templates."mongodb-exporter.env".path;
      defaultText = ''config.sops.templates."mongodb-exporter.env".path'';
      description = ''
        A systemd `EnvironmentFile` carrying `MONGODB_URI`, rendered into /run by sops-nix.

        THE ENVIRONMENT-FILE FORM IS THE POINT, and it is the same argument mongodump's `--config`
        file makes one module over: `--mongodb.uri=mongodb://user:password@...` on a command line is
        readable through /proc by every local user for as long as the process runs, and this one
        runs for ever. A NixOS `Environment=` line would be worse still -- it would put the password
        in the unit file, which is in the world-readable Nix store.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.listenAddress == config.fleet.privateAddress
          || lib.hasPrefix "127." cfg.listenAddress;
        message = ''
          fleet.mongodbExporter.listenAddress is "${cfg.listenAddress}", which is neither this
          host's private address (${config.fleet.privateAddress}) nor loopback. The exporter has no
          authentication and its /metrics page describes the database in detail; it must not be
          reachable from anywhere except this fleet's private network.
        '';
      }
    ];

    # THE PASSWORD, AND THE URI BUILT AROUND IT. Declared here rather than in the host file so that
    # enabling the role is the whole of what it takes to have them -- the same argument
    # roles/mongodb.nix makes for its keyfile and backup password.
    #
    # `directConnection=true` MATCHES WHAT EVERY OTHER CLIENT ON THIS FLEET DOES, and it matters
    # more than it looks. Without it the driver reads the replica-set configuration, discovers the
    # member is registered as 10.20.0.10:27017 (`fleet.mongodb.replSetMemberHost`), and reconnects
    # there -- so a connection deliberately made over loopback quietly becomes one over the private
    # NIC, and a bootstrap window that binds loopback only would break the exporter for reasons
    # that have nothing to do with the exporter.
    sops.secrets."mongodb/exporter-password" = { };
    sops.templates."mongodb-exporter.env" = {
      owner = "mongodb-exporter";
      mode = "0400";
      content = ''
        MONGODB_URI=mongodb://${cfg.username}:${config.sops.placeholder."mongodb/exporter-password"}@${cfg.mongoHost}:${toString config.fleet.mongodb.port}/admin?authSource=admin&directConnection=true
      '';
    };

    # ITS OWN USER, NOT `mongodb`. The exporter listens on a network port and the database does not
    # have to; running it as the account that owns the data directory and the replica-set key file
    # would hand anything that got through it the whole database. It needs no filesystem access at
    # all beyond reading one 0400 file in /run.
    users.users.mongodb-exporter = {
      isSystemUser = true;
      group = "mongodb-exporter";
      description = "mongodb_exporter";
    };
    users.groups.mongodb-exporter = { };

    # OPENED THROUGH THE FLEET OPTION RATHER THAN BY WRITING AN INTERFACE RULE HERE. The interface
    # name is `fleet.privateInterface`, and modules/fleet/firewall.nix's own comment records what
    # happens to a role that spells it out: iptables accepts a rule naming an interface that does
    # not exist, installs it, prints it correctly, and never matches -- so the exporter would be
    # unreachable with nothing anywhere reporting a rule problem.
    fleet.firewall.privateTCPPorts = [ cfg.port ];

    systemd.services.mongodb-exporter = {
      description = "mongodb_exporter (kinowo)";
      wantedBy = [ "multi-user.target" ];

      # `after` mongod BUT NOT `requires`. An exporter that refuses to start because the database
      # is down is an exporter that publishes nothing at the exact moment somebody needs to know
      # what the database is doing -- and `mongodb_up 0` from a running exporter is a far better
      # signal than a scrape target that has vanished, because it distinguishes "mongod is
      # unreachable" from "the machine is gone" (which `TargetDown` already covers).
      after = [ "network-online.target" "mongodb.service" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        User = "mongodb-exporter";
        Group = "mongodb-exporter";
        EnvironmentFile = cfg.environmentFile;

        ExecStart = lib.concatStringsSep " " [
          "${cfg.package}/bin/mongodb_exporter"
          "--web.listen-address=${cfg.listenAddress}:${toString cfg.port}"

          # THE ALERT RULES IN mongodb.rules ARE WRITTEN AGAINST THE NAMES THIS FLAG PRODUCES.
          # Percona's exporter publishes two families: its own `mongodb_ss_*` / `mongodb_rs_*`
          # names, and -- only with this flag -- the older `mongodb_mongod_replset_*` names that
          # every published dashboard and rule set for MongoDB assumes. Removing it does not make
          # the rules fail, it makes them SILENT, which is the failure this fleet's rule files
          # spend their headers warning about. The `absent()` companions in mongodb.rules are the
          # backstop that turns that silence back into an alert.
          "--compatible-mode"

          # `serverStatus` plus `replSetGetStatus`: connections, asserts, opcounters, member state.
          # This is the collector the headline "not PRIMARY" rule reads.
          "--collector.diagnosticdata"
          "--collector.replicasetstatus"

          # NOT `--collect-all`, AND NOT collstats / indexstats / topmetrics. Those emit a series
          # per collection (and per index) per metric, across every database on the server. This
          # host holds one database per country plus the read-model collections; the resulting
          # cardinality is a standing cost paid against a Prometheus with a hard 20GB retention
          # cap, in exchange for numbers no alert here reads. Turn one on when there is a question
          # it answers, not by default.
        ];

        Restart = "on-failure";
        RestartSec = 10;

        # IT BINDS ONE ADDRESS, SO IT HAS TO SURVIVE THAT ADDRESS BEING LATE. This is the same trap
        # modules/fleet/observability.nix documents for node_exporter: a single-address bind fails
        # outright rather than retrying, the default five restarts in ten seconds are spent losing
        # the same race, and systemd then gives up permanently with `start-limit-hit`. A monitoring
        # process that disables itself is the worst shape this can fail in.
        startLimitIntervalSec = 120;
        startLimitBurst = 10;

        # BELOW mongod ON BOTH AXES, deliberately. The exporter exists to describe the database; it
        # must never be why the database is slow. mongod carries OOMScoreAdjust = -500 in
        # roles/mongodb.nix, so this is also, correctly, an earlier candidate for the OOM killer.
        Nice = 10;
        CPUWeight = 50;

        # It reads one file in /run and talks to a socket. Nothing else.
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        MemoryDenyWriteExecute = false; # Go runtime; denying it makes the process fail to start.
      };
    };
  };
}
