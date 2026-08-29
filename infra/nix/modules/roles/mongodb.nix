# mongod -- the whole of kinowo's state.
#
# WHAT THIS REPLACES. A self-hosted MongoDB running on Fly.io in region `arn`, reached by the web
# and worker apps over Fly's private 6PN network. Nothing about that access path changes here: this
# host joins the SAME 6PN as a WireGuard peer (nix/modules/roles/wireguard-fly.nix) and mongod binds
# the tunnel address, so the applications' connection URI changes HOST and nothing else -- no
# TLS-on-the-public-internet, no IP allow-list, no bastion. Read that file before changing anything
# about `bindAddresses` below; the two are one design in two halves.
#
# ------------------------------------------------------------------------------------------------
# THE REPLICA SET IS MANDATORY. A STANDALONE mongod HERE BREAKS THE WEB TIER SILENTLY.
# ------------------------------------------------------------------------------------------------
#
# The application uses CHANGE STREAMS, and change streams are a replica-set feature: `watch()`
# against a standalone mongod does not degrade, it errors -- the server has no oplog to tail
# because replication was never enabled. What makes that dangerous rather than obvious is WHERE it
# lands. The read tier serves its projected read model out of memory; losing the change stream does
# not take the site down, it stops the site UPDATING. Pages keep rendering, health checks keep
# passing, and the content quietly ages until somebody notices yesterday's showtimes.
#
# So `replSetName` has NO default that could be dropped by accident, the assertion below refuses an
# empty one, and a single-node replica set is the minimum correct configuration for this host --
# not a step towards a "real" one. One member is enough for an oplog; more members would be about
# availability, which this fleet has not bought (see the honest note in wireguard-fly.nix about
# single points of failure -- the database is another one, and it is on purpose for now).
#
# `rs.initiate()` IS NOT DONE HERE, and its absence is a decision. Initiating a replica set writes
# a member's HOST NAME into the set's own config, so an automated initiate on every boot is one
# address change away from a set that believes in a member nothing can reach. It is a one-time
# manual step at build, and `verify-mongodb` (not written yet) is where the check that it happened
# belongs.
#
# ------------------------------------------------------------------------------------------------
# HETZNER'S SERVER BACKUPS DO NOT COVER ATTACHED VOLUMES. THAT IS WHY THE TIMER BELOW EXISTS.
# ------------------------------------------------------------------------------------------------
#
# A Hetzner Cloud server backup images the server's own disk. A Volume attached to it is a separate
# product and is NOT in that image -- so on this host, where `dbPath` is the volume, the backup
# product covers the operating system (which Nix already describes completely and can rebuild in
# minutes) and covers NONE of the data (which nothing else has a copy of). Ticking "backups" in the
# Hetzner console and believing the database is protected is the exact shape of mistake this comment
# exists to prevent.
#
# WHAT THE TIMER DOES AND DOES NOT BUY, stated plainly so nobody over-trusts it: a local mongodump
# on the same volume protects against a bad migration, a dropped collection, an application bug
# that corrupts documents -- the failures where the disk is fine and the DATA is wrong. It does NOT
# protect against losing the volume, because it is ON the volume. OFF-BOX REPLICATION OF THESE
# DUMPS IS NOT IMPLEMENTED AND IS THE LARGEST KNOWN GAP IN THIS ROLE; the shape it should take is a
# pull from somewhere that is not this machine, so that a host compromise cannot delete both copies.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.mongodb;

  # ADDRESSES THIS ROLE IS WILLING TO BIND. The assertion below is the only thing standing between
  # a typo and a MongoDB on the public internet, so it is a whitelist of PREFIXES rather than a
  # blacklist of the public address: a blacklist has to be kept in step with whatever Hetzner hands
  # this machine next, and a whitelist does not.
  #
  # 172.16.0.0/12 IS DELIBERATELY NOT HERE, even though it is RFC1918. Matching it by string prefix
  # is the sloppy part of this trick -- "172.2" would admit the entirely public 172.200.0.0/16 --
  # and this fleet uses 10.20.0.0/24, so the range would buy nothing but a hole. If a host ever
  # needs it, add the sixteen exact prefixes (172.16. ... 172.31.), not a short one.
  #
  # `fd` covers fc00::/7 ULA, which is where Fly's 6PN `fdaa:` addresses live.
  privatePrefixes = [ "127." "10." "192.168." "fd" "::1" ];
  isPrivate = address: lib.any (p: lib.hasPrefix p address) privatePrefixes;

  # JSON IS VALID YAML, and mongod's config file is YAML. Generated rather than hand-written for
  # the reason the bitcashier prometheus role gives for the same choice: a hand-indented YAML
  # document assembled from options is one space away from a key mongod reads as something else,
  # and mongod's response to a key it does not recognise is to refuse to start -- which is at least
  # loud, unlike the nesting mistakes that leave a setting silently unset.
  mongodConf = (pkgs.formats.yaml { }).generate "mongod.conf" {
    storage = {
      dbPath = cfg.dbPath;
      wiredTiger.engineConfig.cacheSizeGB = cfg.wiredTigerCacheSizeGB;
    };

    systemLog = {
      destination = "file";
      path = "${cfg.logPath}";
      logAppend = true;
      # Not "syslog": the journal would then carry every slow-query line, and this box's journal is
      # also where a person looks for why mongod would not start. Separate files keep the second
      # question answerable while the first is noisy.
      logRotate = "reopen";
    };

    net = {
      port = cfg.port;
      bindIp = lib.concatStringsSep "," cfg.bindAddresses;

      # REQUIRED THE MOMENT AN IPv6 ADDRESS APPEARS IN `bindIp`, AND ITS ABSENCE IS NOT A WARNING.
      # mongod is IPv4-only unless told otherwise; asked to bind `fdaa:...` without this it fails
      # at startup rather than falling back. That matters here because the Fly tunnel address IS
      # IPv6 -- 6PN is IPv6-only -- so this line and wireguard-fly.nix stand or fall together.
      ipv6 = true;
    };

    security = {
      # SCRAM-SHA-256. Set explicitly rather than left to the server default, because the default
      # has changed across major versions and this file is also the answer to "what authentication
      # does this database actually use?" -- a question that should not require starting the
      # server to answer.
      authorization = "enabled";

      # INTERNAL CLUSTER AUTHENTICATION, WHICH A SINGLE-NODE REPLICA SET STILL REQUIRES. mongod
      # refuses to start with authorization enabled and a replSetName set unless internal member
      # auth is configured -- there is no "it's only one member" exemption. The keyFile is
      # therefore not optional plumbing for a future second member; it is a startup precondition
      # today. It must be 0400 and owned by the mongod user or mongod rejects it on permissions.
      keyFile = cfg.keyFile;
    };

    replication = {
      replSetName = cfg.replSetName;

      # THE OPLOG IS THE CHANGE-STREAM BUFFER, which is why this is pinned rather than left to
      # mongod's default of 5% of free disk. A consumer that is disconnected for longer than the
      # oplog WINDOW cannot resume from its stored token -- it gets ChangeStreamHistoryLost and has
      # to fall back to a full re-read, which for this application is the expensive path it keeps a
      # resume token to avoid. The default couples that window to whatever free space happens to be
      # on the volume, so it SHRINKS exactly as the database grows: the window is narrowest at the
      # moment a full re-read costs the most. Sizing it here makes the window a property of this
      # file, and a deploy or a worker restart that outlasts it is then a number somebody can look
      # at rather than a surprise.
      oplogSizeMB = cfg.oplogSizeMB;
    };

    # setParameter/diagnostics are deliberately left at their defaults. Nothing here has measured a
    # reason to change them, and a tuning knob set "because it seemed better" is indistinguishable
    # later from one set for a reason nobody wrote down.
  };

  # THE DUMP. A script rather than an ExecStart one-liner, because it does four things (dump,
  # rotate, publish a metric, fail loudly) and a one-liner would do the first and silently skip the
  # rest on any non-zero exit.
  mongodumpScript = pkgs.writeShellScript "kinowo-mongodump" ''
    set -euo pipefail

    stamp="$(${pkgs.coreutils}/bin/date -u +%Y%m%dT%H%M%SZ)"
    target="${cfg.backup.directory}/$stamp"

    # --config FOR THE PASSWORD, never --password. Everything on a command line is readable by
    # every local user through /proc, and `ps` on a database host is the first place a curious
    # process looks. The config file is rendered by sops-nix into /run (0400) and never enters the
    # store. The USERNAME is on the command line on purpose -- it is not a secret, and the mongo
    # tools' config file does not accept it anyway.
    ${cfg.toolsPackage}/bin/mongodump \
      --config=${cfg.backup.credentialsFile} \
      --username="${cfg.backup.username}" \
      --authenticationDatabase=admin \
      --host="${cfg.backup.host}" \
      --port=${toString cfg.port} \
      --gzip \
      --oplog \
      --out="$target"

    # ROTATION BY COUNT, NOT BY AGE. `find -mtime +N -delete` deletes the old dumps whether or not
    # any NEW ones succeeded -- so a fortnight of silently failing dumps ends with a backup
    # directory that is empty and a disk that looks healthy. Keeping the N most recent by name
    # (they sort chronologically because the stamp is ISO-8601 UTC) cannot delete the last copy,
    # whatever the timer has been doing.
    ${pkgs.coreutils}/bin/ls -1d ${cfg.backup.directory}/*/ 2>/dev/null \
      | ${pkgs.coreutils}/bin/sort \
      | ${pkgs.coreutils}/bin/head -n "-${toString cfg.backup.keep}" \
      | ${pkgs.findutils}/bin/xargs -r ${pkgs.coreutils}/bin/rm -rf --

    # PUBLISH THE FACT THAT IT WORKED, so that "the backup timer stopped" is an alert rather than a
    # discovery. Written to node_exporter's textfile directory; nix/files/monitoring/rules/
    # mongodb.rules alerts on the age of this timestamp, AND on the series being absent -- because
    # a stale-timestamp alert cannot fire if the timestamp stops being published at all.
    #
    # Written to a temporary file and renamed: node_exporter reads this directory on every scrape,
    # and a half-written .prom is a parse error that discards the whole file.
    size_bytes="$(${pkgs.coreutils}/bin/du -sb "$target" | ${pkgs.coreutils}/bin/cut -f1)"
    tmp="${cfg.backup.textfileDirectory}/mongodump.prom.$$"
    {
      echo "# HELP kinowo_mongodump_last_success_timestamp_seconds Unix time of the last mongodump that completed."
      echo "# TYPE kinowo_mongodump_last_success_timestamp_seconds gauge"
      echo "kinowo_mongodump_last_success_timestamp_seconds $(${pkgs.coreutils}/bin/date +%s)"
      echo "# HELP kinowo_mongodump_last_size_bytes Size on disk of the last completed mongodump."
      echo "# TYPE kinowo_mongodump_last_size_bytes gauge"
      echo "kinowo_mongodump_last_size_bytes $size_bytes"
    } > "$tmp"
    ${pkgs.coreutils}/bin/mv "$tmp" ${cfg.backup.textfileDirectory}/mongodump.prom
  '';
in
{
  options.fleet.mongodb = {
    enable = lib.mkEnableOption "mongod, kinowo's database";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.mongodb-ce;
      defaultText = "pkgs.mongodb-ce";
      description = ''
        THE SERVER. Named as an option and not taken implicitly because a MongoDB MAJOR VERSION IS A
        ONE-WAY DOOR: the server upgrades the data files on first start and an older binary will not
        read them back. A rollback of this host's closure is therefore NOT a rollback of the
        database, which is the one place where NixOS's usual "switch back to the last generation"
        answer does not hold. Pin it, move it deliberately, and take a dump first.
      '';
    };

    toolsPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.mongodb-tools;
      defaultText = "pkgs.mongodb-tools";
      description = "mongodump/mongorestore. Versioned separately from the server by upstream.";
    };

    port = lib.mkOption { type = lib.types.port; default = 27017; };

    dbPath = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/mongodb";
      description = ''
        The data directory, which IS the attached volume's mountpoint.

        THE HAZARD IS THE SAME ONE THE BITCASHIER PROMETHEUS ROLE RECORDS FOR ITS TSDB: a volume
        mounted with `nofail` that fails to mount leaves an empty directory on the root disk, and
        mongod will happily initialise a brand-new empty database in it. The service below carries
        `RequiresMountsFor` for exactly that reason -- an empty database that starts cleanly is
        worse than a service that refuses to.
      '';
    };

    logPath = lib.mkOption { type = lib.types.str; default = "/var/log/mongodb/mongod.log"; };

    keyFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."mongodb/keyfile".path;
      defaultText = ''config.sops.secrets."mongodb/keyfile".path'';
      description = ''
        Path to the replica-set key file, from sops-nix. A PATH, NEVER A VALUE -- it is a shared
        secret that authenticates a member into the set, and a value here would be written into the
        world-readable Nix store by the config generator above.
      '';
    };

    replSetName = lib.mkOption {
      type = lib.types.str;
      description = ''
        The replica set's name. NO DEFAULT, deliberately: see the module header. This is what gives
        the server an oplog, and the oplog is what makes the application's change streams work at
        all. It is also written into the set's own persisted config by `rs.initiate()`, so changing
        it later is not an edit, it is a new replica set.
      '';
    };

    oplogSizeMB = lib.mkOption {
      type = lib.types.int;
      default = 4096;
      description = ''
        4 GB. NOT a tuning number taken from anywhere -- it is a deliberate over-provision of the
        change-stream resume window on a volume where four gigabytes is cheap, chosen so that a
        worker outage measured in hours still resumes from its token rather than falling back to a
        full re-read. The right way to revise it is to measure the real window
        (`db.getReplicationInfo().timeDiff`) under normal write load and compare it against the
        longest outage this fleet actually has; until somebody does that, this is a guess made in
        the safe direction and labelled as one.
      '';
    };

    wiredTigerCacheSizeGB = lib.mkOption {
      type = lib.types.float;
      default = 1.0;
      description = ''
        THE WIREDTIGER CACHE, PINNED. THIS FLEET HAS BEEN OOM-KILLED BY AN UNPINNED ONE BEFORE, and
        that is the whole reason this option exists rather than being left to the server.

        WHY THE DEFAULT IS THE TRAP. Left alone, WiredTiger sizes its cache as roughly half of
        (RAM - 1 GB) -- about 1.5 GB on this 4 GB cx23 -- and that is only the cache: the server's
        own connection, index and aggregation memory sits ON TOP of it, as does everything else on
        the box. The kernel does not reclaim the cache under pressure the way it reclaims page
        cache, so the machine walks up to the OOM killer instead of slowing down, and what it kills
        is the largest process, which is mongod. The failure is a database that disappears
        mid-transaction, not one that gets slow first.

        1 GB ON A 4 GB BOX leaves roughly: 1 GB cache, ~1 GB for mongod's non-cache working set,
        and ~2 GB for the operating system, the page cache that WiredTiger's compressed blocks are
        read through, and headroom for a mongodump running alongside the server. Raise it only
        together with the machine's RAM, and never to "half the box" -- that is the number that
        produced the kill.
      '';
    };

    bindAddresses = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = ''
        EXACTLY WHERE mongod LISTENS, and the most consequential list in this file.

        THREE ADDRESSES AND NO MORE: loopback (for `mongosh` on the box and for the dump timer),
        this host's PRIVATE Hetzner address (for anything else in this fleet -- monitoring, an
        exporter, an operator on the private network), and the WireGuard tunnel address, which is
        how the Fly-hosted applications reach it. THE PUBLIC ADDRESS IS NEVER IN THIS LIST. An
        internet-facing mongod is a scanned-and-found-in-minutes proposition regardless of how good
        the password is, and the assertion below refuses any address outside the loopback, RFC1918
        and ULA ranges rather than trusting review to catch it.

        BARE ADDRESSES, NO PREFIX LENGTHS. `fleet.wireguardFly.address` carries its `/120` because
        the kernel needs it to decide what is on-link; mongod's `bindIp` does not take one and will
        not start if given one. The same address is therefore written twice, in two spellings --
        which is worth a moment's care at build time and is the kind of thing to check first if
        mongod refuses to start after a tunnel change.

        NOTE WHAT THIS IS NOT: it is not a firewall. The firewall rules below also exist, and
        neither one is redundant -- binding is what the process does, filtering is what the kernel
        does, and the pair is what survives one of them being edited by somebody in a hurry.
      '';
    };

    backup = {
      enable = lib.mkOption { type = lib.types.bool; default = true; };

      host = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = ''
          LOOPBACK, DELIBERATELY. The dump runs on this machine and has no reason to leave it -- and
          dumping over the tunnel or the private network would put a full copy of the database on a
          wire, every night, for nothing.
        '';
      };

      username = lib.mkOption {
        type = lib.types.str;
        default = "kinowo-backup";
        description = ''
          The dump's own user, which should hold `backup` AND NOTHING ELSE.

          A SEPARATE USER FROM THE APPLICATION'S, deliberately: the application's credential can
          write, and a backup job has no business holding a credential that can. It is on the
          command line rather than in the credentials file because it is not a secret and because
          the mongo tools' config file does not accept it -- see the template's own comment.

          THE USER IS NOT CREATED BY THIS ROLE. Creating it means a `db.createUser()` against a
          running mongod, which is a one-time build step alongside `rs.initiate()` (see the module
          header) and not something to attempt on every activation.
        '';
      };

      credentialsFile = lib.mkOption {
        type = lib.types.str;
        default = config.sops.templates."mongodump.conf".path;
        defaultText = ''config.sops.templates."mongodump.conf".path'';
        description = ''
          A mongodump `--config` YAML file (a `password:` key), rendered by sops-nix into /run.

          THE FILE FORM IS THE POINT: `--password` on the command line is visible in /proc to every
          local user for as long as the dump runs, and a nightly dump is a long time.
        '';
      };

      directory = lib.mkOption {
        type = lib.types.str;
        default = "/var/lib/mongodb-backups";
        description = ''
          ON THE DATA VOLUME'S HOST, AND THAT IS STATED SO NOBODY MISTAKES IT FOR OFF-SITE. See the
          module header: this survives a bad migration, not a lost volume.

          Kept OUT of `dbPath` on purpose -- a dump written inside the data directory is a dump
          mongod scans, and a restore that copies it back in place is then ambiguous.
        '';
      };

      keep = lib.mkOption {
        type = lib.types.int;
        default = 7;
        description = ''
          How many dumps to retain, BY COUNT. Seven daily dumps is a week of logical history, which
          is the window in which a data bug is usually noticed. See the rotation comment in the
          script for why this is a count and not an age.
        '';
      };

      onCalendar = lib.mkOption {
        type = lib.types.str;
        default = "03:20";
        description = ''
          Nightly, in the quietest hour for a Polish-audience site. `RandomizedDelaySec` below
          spreads it, which matters only if this fleet ever grows a second database host -- stated
          now so it is not "fixed" later by somebody who reads a fixed time as more correct.
        '';
      };

      textfileDirectory = lib.mkOption {
        type = lib.types.str;
        default = "/var/lib/node_exporter/textfile";
        description = ''
          Where the dump publishes its success timestamp for node_exporter to pick up.

          THIS ROLE CANNOT ENFORCE THAT node_exporter IS ACTUALLY READING IT. The directory is only
          scraped if node_exporter runs with `--collector.textfile.directory` pointing here, which
          is the host's node_exporter configuration and not this file. If the two disagree the
          metric is written and read by nobody -- which is why mongodb.rules alerts on the series
          being ABSENT as well as on it being stale.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.replSetName != "";
        message = ''
          fleet.mongodb.replSetName is required. A standalone mongod has no oplog, so the
          application's change streams cannot start -- and the web tier keeps serving its projected
          read model from memory while it stops updating, so nothing looks broken.
        '';
      }
      {
        assertion = lib.all isPrivate cfg.bindAddresses;
        message = ''
          fleet.mongodb.bindAddresses contains an address outside loopback, RFC1918 and the ULA
          range: ${lib.concatStringsSep " " (lib.filter (a: !isPrivate a) cfg.bindAddresses)}.
          mongod must never listen on this machine's public address. Reach it over the private
          network or the Fly WireGuard tunnel (nix/modules/roles/wireguard-fly.nix).
        '';
      }
      {
        assertion = lib.any (a: lib.hasPrefix "127." a) cfg.bindAddresses;
        message = ''
          fleet.mongodb.bindAddresses must include 127.0.0.1: the mongodump timer connects over
          loopback so that a full copy of the database never crosses a wire, and dropping loopback
          would break the backup without touching anything the applications can see.
        '';
      }
    ];

    # THE TWO SECRETS THIS ROLE NEEDS, declared here rather than in the host file, so that enabling
    # the role is the whole of what it takes to have them -- a role whose secrets are declared
    # somewhere else is a role that half-starts on a new host and fails at mongod's permission
    # check rather than at evaluation.
    #
    # 0400 AND OWNED BY mongodb IS NOT TIDINESS FOR THE KEYFILE: mongod REFUSES TO START if the
    # replica-set key file is group- or world-readable. That check is a good one and it produces a
    # startup error that names permissions, so it is at least loud -- but only after a deploy.
    sops.secrets."mongodb/keyfile" = { owner = "mongodb"; mode = "0400"; };

    # The dump's credentials, as a mongodump `--config` document. Rendered into /run by sops-nix at
    # start; never in the store, and never on a command line. See `backup.credentialsFile`.
    sops.templates."mongodump.conf" = {
      owner = "mongodb";
      mode = "0400";
      # ONLY THE PASSWORD. The mongo tools' `--config` file accepts a short, fixed set of keys
      # (`password`, `uri`, `sslPEMKeyPassword`) and NOT a username -- a `username:` key here is
      # silently not what you think it is. The username goes on the command line below, where it
      # belongs: it is not a secret, and putting it in the file would look like it was protected.
      content = ''
        password: ${config.sops.placeholder."mongodb/backup-password"}
      '';
    };
    sops.secrets."mongodb/backup-password" = { };

    users.users.mongodb = {
      isSystemUser = true;
      group = "mongodb";
      home = cfg.dbPath;
      description = "MongoDB";
    };
    users.groups.mongodb = { };

    environment.etc."mongod.conf".source = mongodConf;

    # mongosh on the box. An operator asking the database what it thinks -- "is this actually a
    # replica set?", "what is the oplog window?" -- should not need to build a shell first.
    environment.systemPackages = [ cfg.package cfg.toolsPackage ];

    systemd.tmpfiles.rules = [
      "d ${builtins.dirOf cfg.logPath} 0750 mongodb mongodb -"
      "d ${cfg.backup.directory} 0700 mongodb mongodb -"
      # 0755 rather than 0700: node_exporter runs as its own user and has to READ this.
      "d ${cfg.backup.textfileDirectory} 0755 mongodb mongodb -"
    ];

    systemd.services.mongodb = {
      description = "MongoDB (kinowo)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      # THE VOLUME. Without this, a volume that fails to mount leaves mongod initialising a fresh,
      # empty database on the root disk -- which starts cleanly, passes every check, and serves an
      # empty site. See `dbPath`'s description.
      unitConfig.RequiresMountsFor = [ cfg.dbPath ];

      # THE CONFIG IS IN /etc AND THE UNIT DOES NOT CHANGE WHEN IT DOES. Activation replaces the
      # file, systemd sees an unchanged unit, and mongod keeps running with whatever it parsed at
      # its last start -- the closure hash advances and every signal says the change is live. This
      # is the same gap the bitcashier prometheus role documents at length; the fix is the same.
      #
      # A RESTART, NOT A RELOAD: mongod re-reads only a subset of its config on SIGHUP (log
      # rotation), and the settings that matter here -- bindIp, the cache size, the replica set --
      # are start-time only. A restart of the fleet's only database is not free, which is the
      # honest cost of putting the config in /etc: an unrelated activation that happens to touch
      # this file will bounce the database.
      restartTriggers = [ mongodConf ];

      serviceConfig = {
        User = "mongodb";
        Group = "mongodb";
        ExecStart = "${cfg.package}/bin/mongod --config /etc/mongod.conf";
        Restart = "on-failure";
        RestartSec = 10;

        # SIGTERM IS A CLEAN SHUTDOWN AND IT IS NOT INSTANT: mongod flushes its journal and its
        # dirty cache pages. Killing it mid-flush is survivable (that is what the journal is for)
        # but costs a recovery on start, so it is given room. 300s rather than systemd's 90s
        # default, for the same reason the bitcashier prometheus role widens its own.
        KillSignal = "SIGTERM";
        TimeoutStopSec = 300;

        # THE DATABASE IS THIS FLEET'S POINT. It should not be the process the kernel picks when
        # something else on the box goes wrong -- and note that this does NOT license an unpinned
        # cache: OOMScoreAdjust changes WHO gets killed, `wiredTigerCacheSizeGB` changes WHETHER
        # anyone does. The cache pin is the fix; this is a tiebreak.
        OOMScoreAdjust = -500;

        # NO MemoryMax. A hard cgroup limit turns memory pressure into the kernel killing mongod --
        # which is precisely the failure this role is built around avoiding. The lever that bounds
        # mongod's memory is the cache size, applied inside the process where it can be respected
        # rather than outside it where it can only be enforced.

        # mongod holds a file per collection and per index, plus a connection each. The distro
        # default of 1024 is reached by an ordinary working set, and what it produces is a
        # confusing "too many open files" under load rather than at start.
        LimitNOFILE = 64000;

        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.dbPath (builtins.dirOf cfg.logPath) ];
      };
    };

    systemd.services.mongodump = lib.mkIf cfg.backup.enable {
      description = "mongodump of kinowo's database, with rotation";
      # Not `requires`: a dump attempted while mongod is down should FAIL and be seen, not be
      # skipped silently by a dependency that never became active.
      after = [ "mongodb.service" ];
      unitConfig.RequiresMountsFor = [ cfg.backup.directory ];
      serviceConfig = {
        Type = "oneshot";
        User = "mongodb";
        Group = "mongodb";
        ExecStart = mongodumpScript;
        # A dump that runs into the working day is worse than one that did not run: it competes
        # with live traffic for the same disk. Two hours is generous for this corpus; if it is ever
        # hit, the answer is to look at why, not to raise it.
        TimeoutStartSec = "2h";
        # Below mongod, on both axes. The dump is the thing that yields when the two collide --
        # serving the site matters more than last night's copy being finished by 04:00.
        Nice = 10;
        IOSchedulingClass = "idle";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.backup.directory cfg.backup.textfileDirectory ];
      };
    };

    systemd.timers.mongodump = lib.mkIf cfg.backup.enable {
      description = "Nightly mongodump";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.backup.onCalendar;
        RandomizedDelaySec = "20m";
        # A MISSED RUN IS RUN LATE RATHER THAN SKIPPED. A machine that was down at 03:20 is exactly
        # the machine whose last dump is worth having.
        Persistent = true;
      };
    };

    # THE FIREWALL, WHICH IS THE SECOND HALF OF `bindAddresses` AND NOT A DUPLICATE OF IT.
    #
    # Binding decides what the process listens on; this decides what the kernel delivers. Both are
    # stated because either one alone is one careless edit away from being the only thing left --
    # and this is the port that must never be reachable from the internet.
    #
    # The tunnel's own port is opened in wireguard-fly.nix, alongside the interface it belongs to.
    networking.firewall.interfaces.${config.fleet.privateInterface}.allowedTCPPorts = [ cfg.port ];
  };
}
