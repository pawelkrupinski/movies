# Prometheus and Alertmanager -- this fleet's alarm, on the same box as its k3s control plane.
#
# WHAT IT WATCHES, AND WHAT WATCHES IT. Three node_exporters over the private network (mongo-1,
# this host, k3s-worker-1); two exporters that speak for something the host cannot -- mongodb_exporter
# on mongo-1 for what mongod thinks it is, and kube-state-metrics through a NodePort for what the
# k3s cluster thinks it is; its own three processes; and the application pods themselves, over
# their NodePorts. NOTHING WATCHES THIS HOST FROM OUTSIDE. That is the standing weakness
# of a single monitoring node and it is not solved here; what IS done is that Alertmanager's own
# delivery path is exercised by a rule (see monitoring-self.rules), so a Telegram route that has
# quietly stopped working is discovered by a heartbeat rather than by the first real incident.
#
# ------------------------------------------------------------------------------------------------
# WHY THIS PROCESS OUTRANKS ITS NEIGHBOUR
# ------------------------------------------------------------------------------------------------
#
# monitoring-1 is a cx23 running Prometheus, Alertmanager, Grafana AND a k3s server. That is a
# deliberate density decision for a small fleet, and it has one consequence worth being explicit
# about: when the box is starved, the thing that must keep working is the thing that RECORDS the
# starvation. A monitoring stack that stops scraping at the exact moment its neighbour goes wrong
# leaves a hole in the history precisely where the evidence would have been, and "the graph just
# stops" is the least useful shape an incident can have.
#
# So both units below carry `CPUWeight` well above the systemd default of 100, and an I/O class
# above k3s's. k3s-server.nix states the other side of the same decision on its own unit, so that
# neither file is the only place the ordering is written down. This is not a guarantee -- CPUWeight
# is a proportional share under contention, not a reservation -- and it does nothing about memory
# pressure, where the OOM killer takes the largest process regardless of weight.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.prometheus;

  # SUBSTITUTE QUIETLY, THEN REFUSE ANY PLACEHOLDER THAT SURVIVED. Taken from the bitcashier
  # prometheus role, including the reasoning for why the OUTPUT is checked rather than the input:
  # asserting that a pattern was present in the source fails on files that legitimately contain
  # none (most of the rule files name no address at all), whereas asserting that no `@TOKEN@`
  # remains in the result catches both a placeholder that stopped matching and one somebody adds
  # later and forgets to render. An unsubstituted `@LISTEN_ADDRESS@` would otherwise reach
  # Prometheus as a literal hostname, and a scrape of a host that does not resolve is a target
  # that is simply down -- which reads as a machine problem.
  render = name: src: pkgs.runCommand name { } ''
    substitute ${src} $out \
      --replace-quiet '@LISTEN_ADDRESS@' '${cfg.listenAddress}' \
      --replace-quiet '@GRAFANA_PORT@' '${toString cfg.grafanaPort}' \
      --replace-quiet '@TELEGRAM_BOT_TOKEN_FILE@' '${cfg.telegramBotTokenFile}' \
      --replace-quiet '@SMTP_SMARTHOST@' '${cfg.smtpSmarthost}' \
      --replace-quiet '@SMTP_USERNAME@' '${cfg.smtpUsername}' \
      --replace-quiet '@SMTP_PASSWORD_FILE@' '${cfg.smtpPasswordFile}' \
      --replace-quiet '@ALERT_EMAIL_FROM@' '${cfg.alertEmailFrom}' \
      --replace-quiet '@ALERT_EMAIL_TO@' '${cfg.alertEmailTo}'
    if grep -nE '@[A-Z0-9_]+@' $out; then
      echo "render: ${name} still carries an unsubstituted placeholder (above)." >&2
      exit 1
    fi
  '';

  # THE NODE TARGETS, GENERATED FROM AN OPTION RATHER THAN WRITTEN INTO THE YAML.
  #
  # Hetzner Cloud service discovery would be the other way to do this (it is what the bitcashier
  # fleet uses, with an API token). Three machines do not justify it: SD buys you the machines you
  # forgot to declare, and on a fleet this size the flake already knows all of them. What it WOULD
  # buy is discovering a machine somebody created outside Terraform -- which on this fleet should
  # not happen and, if it does, is a bigger problem than a missing scrape target.
  #
  # WRITTEN WITH toJSON BECAUSE JSON IS VALID YAML. Hand-rendering a YAML list out of an option is
  # one indentation mistake away from a job Prometheus reads as something else, and this file is
  # generated rather than reviewed by eye.
  nodeTargetsYaml = builtins.toJSON {
    scrape_configs = lib.optional (cfg.nodeTargets != [ ]) {
      job_name = "node";
      static_configs = map
        (t: {
          targets = [ "${t.address}:${toString t.port}" ];
          labels = { inherit (t) host role; };
        })
        cfg.nodeTargets;
    };
  };

  # THE TWO EXPORTERS THAT ARE NOT node_exporter, EACH IN ITS OWN scrape.d FILE.
  #
  # WRITTEN LIKE `nodeTargetsYaml` ABOVE -- toJSON, one job per file, `lib.optional` so an empty
  # list produces an empty `scrape_configs` rather than a half-written document the glob would
  # still match. The alternative was two more `@PLACEHOLDER@` jobs inside
  # files/monitoring/prometheus.yaml; this way "not deployed" is an empty list rather than a job
  # pointed at nothing.
  #
  # WHY EACH GETS A JOB OF ITS OWN RATHER THAN JOINING THE `node` JOB: the `node` job's targets all
  # speak the same metric namespace and share `TargetDown`'s "job is a machine" reading. These two
  # do not -- one describes a database process and one describes a whole Kubernetes cluster -- and
  # a rule that says `job="node"` should keep meaning "a machine in this fleet".
  mongodbTargetsYaml = builtins.toJSON {
    scrape_configs = lib.optional (cfg.mongodbExporterTargets != [ ]) {
      job_name = "mongodb";
      # THIRTY SECONDS AND A TWENTY-SECOND TIMEOUT, against the global 15s/10s. Every scrape of
      # this exporter runs `getDiagnosticData` and `replSetGetStatus` against a live mongod on a
      # 2-core box that is also serving the application. Under the global 10s timeout a busy
      # moment turns the target red, `TargetDown` pages, and the page reads as "the database is
      # unreachable" when the database was merely busy. Nothing this exporter publishes changes
      # meaningfully inside thirty seconds.
      scrape_interval = "30s";
      scrape_timeout = "20s";
      static_configs = map
        (t: {
          targets = [ "${t.address}:${toString t.port}" ];
          labels = { inherit (t) host role; };
        })
        cfg.mongodbExporterTargets;
    };
  };

  kubeStateMetricsYaml = builtins.toJSON {
    scrape_configs = lib.optional (cfg.kubeStateMetricsTargets != [ ]) {
      job_name = "kube-state-metrics";
      # NO `host` OR `role` LABEL, DELIBERATELY, AND IT IS THE ONE INTERESTING THING ABOUT THIS JOB.
      # The address scraped is a NodePort on whichever node answers, but the SERIES describe the
      # whole cluster -- every node, every pod, wherever it runs. A `host` label here would be
      # true of the scrape and false of the data, and the first rule written against it would
      # quietly be about the wrong machine. The series carry their own `node`, `namespace` and
      # `pod` labels; use those.
      static_configs = map (t: { targets = [ t ]; }) cfg.kubeStateMetricsTargets;
    };
  };

  # FLUX, WHICH IS NOW THE DEPLOY PATH AND THEREFORE HAS TO BE WATCHED LIKE ONE.
  #
  # Four controllers have to agree for a commit to reach production, and any of them can stop
  # WITHOUT ANYTHING GOING RED -- a custom resource turning `Ready=False` is not an event anybody
  # sees. That is survivable while `kubectl set image` is the real deploy and Flux only reconciles
  # config; it stops being survivable the moment Flux owns the rollout, which is why this job
  # landed before that switch rather than after it.
  #
  # A `controller` LABEL, UNLIKE kube-state-metrics ABOVE. The distinction is whether the label is
  # true of the DATA or only of the connection: kube-state-metrics describes the whole cluster from
  # whichever node answers, so a per-target label there would lie. These series are each
  # controller's own reconciliation counters, so naming the controller is the one label that makes
  # an alert able to say WHICH half of the deploy path stopped.
  fluxYaml = builtins.toJSON {
    scrape_configs = lib.optional (cfg.fluxTargets != [ ]) {
      job_name = "flux";
      static_configs = map (t: {
        targets = [ t.address ];
        labels = { inherit (t) controller; };
      }) cfg.fluxTargets;
    };
  };

  # A RULE FILE HAS TO BE IN TWO PLACES: this list, which INSTALLS it into /etc, and the
  # `rule_files` list in files/monitoring/prometheus.yaml, which LOADS it. Present in one and
  # absent from the other, it is either a file nothing reads (silent -- the alerts simply never
  # fire) or a path Prometheus cannot find (loud -- it refuses to start). The silent half is the
  # dangerous one, so infra/test/test_alert_rules.sh compares the three lists -- the files on disk,
  # this list, and prometheus.yaml's `rule_files` -- and fails if any name is missing from one of
  # them. It was written after jvm-heap.rules shipped installed-but-never-loaded: promtool passed
  # against the file, Prometheus reloaded clean, and the alert simply did not exist.
  #
  # ALPHABETICAL, matching the order in prometheus.yaml's `rule_files`, so the two lists can be
  # compared by eye without sorting one of them in your head. The comparison is the only check
  # there is until the guard script exists.
  ruleNames = [
    # IS EVERY CINEMA STILL BEING SCRAPED. The application's own roster-freshness census, and the
    # only file here that watches the product rather than the machines it runs on. Its failure is
    # the silent kind: a cinema nobody scrapes emits no series at all, so every traffic counter
    # stays clean while the roster goes dark.
    "cinema-scrape"
    "filesystem-capacity"
    # IS THE DEPLOY PATH STILL RUNNING. Flux owns the rollout now, and its failure mode is not an
    # outage -- it is the cluster serving the last image it received while every CI run stays
    # green. Added as a CONDITION of retiring `kubectl set image`, not as a follow-up to it:
    # trading a red CI job for an unwatched NotReady condition would have been a downgrade.
    "flux"
    # THE HOSTS THEMSELVES -- memory, OOM, CPU, failed units, read-only filesystems. NEW at the
    # post-migration audit: on Fly the host was somebody else's problem, and nothing had replaced
    # the host agent's view with one of these three machines.
    "host-health"
    # IS ANY JVM ABOUT TO RUN OUT OF HEAP. Added after web-us OOMed on 2026-09-03 while a merged,
    # green, `deployed-web`-tagged heap raise sat unapplied: the CI deploy rolls images only, so a
    # JAVA_OPTS change never reaches the cluster on its own and nothing anywhere went red.
    "jvm-heap"
    # The k3s units, watched through node_exporter's systemd collector. There is no
    # kube-state-metrics on this fleet, so node conditions and workloads are NOT covered; the file
    # says so at length rather than writing rules against series nothing produces.
    "k3s"
    "mongodb"
    "monitoring-self"
    # Whether the fleet is still deploying itself. The failure this covers is completely silent:
    # an applier that has stopped leaves every service running and every other alert green.
    "nixos-deploy"
    # CAN THE READ MODEL'S PROJECTION RATE STILL BE EXPLAINED. The projector is fed by two change
    # streams and only one of them was counted, so a 40x climb on 2026-09-04 sat against a flat
    # "projection trigger" line and could not be attributed for an evening. Alphabetically after
    # nixos-deploy and before web-errors.
    "read-model-projection"
    # IS THE SITE ANSWERING. Every other file here watches a machine or a process, all of which can
    # be green while the web tier serves a 500 to every visitor. Added after the "Error share" panel
    # spent an afternoon at 25% with nothing failing -- the file explains why the 4xx half of that
    # panel cannot be alerted on, so that nobody adds the obvious rule and gets paged for a working
    # site.
    "web-errors"
    # IS THE WEB TIER STILL FAST. Per-ROUTE, because the tier-wide quantile is noisier on 2-3 req/s
    # than the regression it would have to see: on 2026-09-04 one route at a 2s p95 owned ~70% of
    # the tier's request-seconds while error share, heap and restarts all stayed green.
    "web-latency"
    # HOW MANY BYTES A PAGE HANDS THE VISITOR, which the latency rules cannot see: they stop at the
    # response header, so a page answering in 50 ms and then shipping 1.66 MB reads perfectly
    # healthy. Four city listings were over a megabyte gzipped when this was written.
    "web-page-weight"
  ];
in
{
  options.fleet.prometheus = {
    enable = lib.mkEnableOption "Prometheus and Alertmanager";

    package = lib.mkOption { type = lib.types.package; default = pkgs.prometheus; defaultText = "pkgs.prometheus"; };
    alertmanagerPackage = lib.mkOption { type = lib.types.package; default = pkgs.prometheus-alertmanager; defaultText = "pkgs.prometheus-alertmanager"; };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = config.fleet.privateAddress;
      defaultText = "config.fleet.privateAddress";
      description = ''
        THE PRIVATE ADDRESS, AND NEVER 0.0.0.0. Prometheus's own web UI has no authentication and
        its query API can read every metric this fleet has; Alertmanager's API can silence every
        alert. Both are reached over the private network or not at all.

        Substituted into the vendored config as well as passed on the command line, so the two
        cannot name different addresses -- the config scrapes Prometheus and Alertmanager by
        literal address, and a disagreement would leave the alarm not watching itself.
      '';
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/monitoring/prometheus";
      description = ''
        On the monitoring volume, which is mounted at the PARENT (/var/lib/monitoring).

        THE SAME HAZARD AS mongodb.nix's dbPath: with the volume absent this is an empty directory
        on the root disk and Prometheus starts a new, empty TSDB in it perfectly happily. The unit
        below carries `RequiresMountsFor` for that reason.
      '';
    };

    alertmanagerDataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/monitoring/alertmanager";
      description = ''
        Alertmanager's notification log and silences. On the volume as well -- losing it is not
        catastrophic, but it means every silence somebody set during an incident evaporates on a
        rebuild, which is exactly when they were relied on.
      '';
    };

    retention = lib.mkOption {
      type = lib.types.str;
      default = "90d";
      description = ''
        HOW MUCH HISTORY, AS AN INTENT. Ninety days covers "is this worse than the same week last
        quarter?", which is the question this fleet's seasonal traffic actually raises.

        IT IS NOT WHAT BOUNDS THE DISK. See `retentionSize`: time-based retention says how old data
        may be, not how large it may get, so a doubling of the target count or of series
        cardinality silently doubles the volume used with this value unchanged. Both are set here
        precisely because only one of them can be violated by adding a scrape target.
      '';
    };

    retentionSize = lib.mkOption {
      type = lib.types.str;
      default = "20GB";
      description = ''
        THE HARD BOUND ON DISK, and the one of the two retention settings that cannot be undone by
        somebody adding a job. When it bites, Prometheus drops the OLDEST blocks -- so the failure
        mode is a shorter history, which is recoverable, rather than a full volume, which takes the
        whole monitoring stack (and Grafana's sqlite beside it) down with it.

        20GB IS A GUESS SIZED AGAINST A VOLUME NOBODY HAS MEASURED HERE. It must stay comfortably
        under the monitoring volume's real size, with room for Grafana's database and the WAL --
        the correct way to revise it is to read `prometheus_tsdb_storage_blocks_bytes` after a
        month of real ingest, not to raise it because a graph looks short.
      '';
    };

    grafanaPort = lib.mkOption {
      type = lib.types.port;
      default = 3000;
      description = ''
        Where Grafana serves, so that Prometheus can scrape ITS metrics -- Grafana publishes alert
        rule evaluation and notification failures, which is the only way this stack notices that
        its own alerting has stopped. Kept in step with `fleet.grafana.port` by the assertion below
        rather than by two literals agreeing out of luck.
      '';
    };

    telegramBotTokenFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."alertmanager/telegram-bot-token".path;
      defaultText = ''config.sops.secrets."alertmanager/telegram-bot-token".path'';
      description = "Path to the @kinowobot bot token, from sops-nix. Alertmanager reads it at send time.";
    };

    # ── THE EMAIL PATH ─────────────────────────────────────────────────────────────────────────
    #
    # A SECOND DESTINATION IS A SECOND THING TO NOTICE HAS STOPPED WORKING, which is the reason
    # this file's header gives for everything going to one Telegram channel. The disk alerts get
    # email ANYWAY, and the trade is made honestly rather than talked out of: a filling disk is
    # the one class of alert here whose useful response is days away rather than minutes, so it
    # is the one worth having somewhere that survives a chat channel being muted or missed.
    #
    # THEY GO TO BOTH, NOT INSTEAD. An SMTP relay that starts rejecting is silent in exactly the
    # way an alerting path must not be, and email is the newer of the two paths here. Telegram
    # stays the one that is known to work; email is additional until it has earned otherwise.
    smtpSmarthost = lib.mkOption {
      type = lib.types.str;
      default = "smtp.resend.com:587";
      description = ''
        `host:port` of the SMTP submission endpoint. Resend by default: an API key is the
        password, so there is no mailbox password to rotate and no account whose 2FA settings
        can silently break sending -- which is what rules a personal Gmail out for this.
        587 (submission + STARTTLS), not 465: Alertmanager negotiates TLS on 587 and `require_tls`
        below refuses to send if the upgrade does not happen.
      '';
    };

    smtpUsername = lib.mkOption {
      type = lib.types.str;
      default = "resend";
      description = ''
        SMTP username. For Resend this is the literal string `resend` for every account -- the API
        key in `smtpPasswordFile` is what identifies you. Change both together if the relay moves.
      '';
    };

    smtpPasswordFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."alertmanager/smtp-password".path;
      defaultText = ''config.sops.secrets."alertmanager/smtp-password".path'';
      description = ''
        Path to the relay's API key, from sops-nix. Read at SEND time like the Telegram token, so
        it never reaches the world-readable Nix store.
      '';
    };

    alertEmailFrom = lib.mkOption {
      type = lib.types.str;
      default = "alerts@kinowo.net";
      description = ''
        Envelope sender. IT MUST BE AT A DOMAIN THE RELAY HAS VERIFIED or the relay accepts the
        submission and drops the message, which is the failure mode this whole stack exists to
        avoid. kinowo.net is on Cloudflare, so verifying it is a DNS record rather than a project.
      '';
    };

    alertEmailTo = lib.mkOption {
      type = lib.types.str;
      default = "pawel.krupinski@gmail.com";
      description = "Where the disk alerts land. One operator, one mailbox.";
    };

    externalUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://alertmanager.kinowo.internal:9093";
      description = ''
        What the links inside a Telegram alert point at. A name that resolves only on the private
        network, deliberately: the person clicking it is expected to be on the VPN, and publishing
        a working public link to the thing that can silence every alert is not a convenience worth
        having.
      '';
    };

    nodeTargets = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          address = lib.mkOption { type = lib.types.str; description = "Private address to scrape."; };
          host = lib.mkOption { type = lib.types.str; description = "The `host` label -- the machine's own hostname."; };
          role = lib.mkOption { type = lib.types.str; description = "The `role` label: mongodb, monitoring, k3s-worker."; };
          port = lib.mkOption {
            type = lib.types.port;
            default = 9100;
            description = ''
              node_exporter's port ON THE TARGET HOST -- which is `fleet.nodeExporterPort` over
              there, not here, so it is a plain default rather than a reference. 9100 is what
              modules/fleet/default.nix defaults that option to; if a host ever overrides it, this
              entry has to say so, and the symptom of forgetting is one machine sitting down while
              the other two are green.
            '';
          };
        };
      });
      default = [ ];
      description = ''
        THE FLEET'S node_exporters.

        THE ADDRESSES ARE NOT WRITTEN HERE AND SHOULD NOT BE. Each entry is expected to be read in
        flake.nix off the target host's OWN `fleet.privateAddress`, so this list carries references
        rather than literals and the two cannot drift. That is the bitcashier rule about address
        literals, and it earns its keep on a cloud fleet for a specific reason: a provider will
        eventually hand a decommissioned machine's address to the next one built, and a literal in
        a manifest keeps pointing at it.

        A MACHINE MISSING FROM THIS LIST IS SIMPLY NOT WATCHED, and nothing says so -- there is no
        service discovery here to notice it (see `nodeTargetsYaml` for why not). The backstop is
        that the list is short and lives beside the host declarations that produce it.
      '';
      example = lib.literalExpression ''
        [{
          address = self.nixosConfigurations.mongo-1.config.fleet.privateAddress;
          host = "mongo-1";
          role = "mongodb";
        }]
      '';
    };

    # ---------------------------------------------------------------------------------------------
    # THE TWO DEFAULTS BELOW ARE ADDRESS LITERALS, WHICH THIS FLEET OTHERWISE REFUSES TO WRITE.
    # ---------------------------------------------------------------------------------------------
    #
    # `nodeTargets` above spends a paragraph on why an address belongs to the host that owns it and
    # should be read off `fleet.privateAddress` in flake.nix rather than typed twice -- a cloud
    # provider eventually hands a decommissioned machine's address to the next one built, and a
    # literal keeps pointing at it.
    #
    # THESE TWO CARRY LITERALS ANYWAY, and it is the same compromise `fleet.logs.serverAddress`
    # makes in hosts/mongo-1: the reference form needs an edit to flake.nix (or to
    # hosts/monitoring-1), which the change that added these could not make. The defaults are
    # therefore a WORKING configuration rather than a correct one, and the fix is one override:
    #
    #     fleet.prometheus.mongodbExporterTargets = [{
    #       address = self.nixosConfigurations.mongo-1.config.fleet.privateAddress;
    #       host = "mongo-1";
    #       role = "mongodb";
    #     }];
    #     fleet.prometheus.kubeStateMetricsTargets = [
    #       "${self.nixosConfigurations.k3s-worker-1.config.fleet.privateAddress}:30080"
    #     ];
    #
    # Until then, a machine that moves has to be found in two files, and the symptom of missing one
    # is a target that sits down and reads as a dead exporter.

    mongodbExporterTargets = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          address = lib.mkOption { type = lib.types.str; description = "Private address of the host running mongodb_exporter."; };
          host = lib.mkOption { type = lib.types.str; description = "The `host` label -- the machine's own hostname."; };
          role = lib.mkOption { type = lib.types.str; description = "The `role` label, matching the same machine's entry in `nodeTargets`."; };
          port = lib.mkOption {
            type = lib.types.port;
            default = 9216;
            description = ''
              mongodb_exporter's port ON THE TARGET HOST, which is `fleet.mongodbExporter.port`
              over there and not here -- so this is a plain default rather than a reference, the
              same shape as `nodeTargets`'s 9100.
            '';
          };
        };
      });
      default = [{ address = "10.20.0.13"; host = "mongo-1"; role = "mongodb"; }];
      description = ''
        Where mongodb_exporter runs. See roles/mongodb-exporter.nix for why it exists at all: it is
        the only thing on this fleet that can tell a mongod which is `active` and refusing writes
        from one which is `active` and healthy, and the application's change streams depend on the
        difference.

        EMPTY MEANS NO JOB, NOT A BROKEN ONE -- the scrape.d file is written with an empty
        `scrape_configs`, so turning the exporter off does not leave a target sitting red for ever.
        It does leave mongodb.rules' exporter-based alerts unable to fire, which is why each of
        them has an `absent()` companion.
      '';
    };

    kubeStateMetricsTargets = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "10.20.0.12:30080" ];
      description = ''
        `address:port` of kube-state-metrics, reached through a NodePort on the private subnet.

        A BARE `address:port` RATHER THAN A SUBMODULE, because there are no useful target labels to
        attach: kube-state-metrics describes the whole cluster, so labelling the job with the host
        that happens to answer the scrape would produce series whose `host` label is true of the
        connection and false of the data. See `kubeStateMetricsYaml` above.

        WHY A NodePort AND NOT SERVICE DISCOVERY. Prometheus runs OUTSIDE the cluster on
        monitoring-1 and holds no kubeconfig, which is deliberate -- `kubernetes_sd_configs` would
        mean giving the monitoring stack a credential that can read every object in the cluster
        (Secrets included) in exchange for discovering one endpoint that never moves. The trade is
        in movies-gitops/kube-state-metrics/, in full, including what a NodePort costs.

        THE PORT IS FIXED AT 30080 AND MUST AGREE WITH THE Service MANIFEST. Nothing here can check
        that; a disagreement is a target that sits down.
      '';
    };

    fluxTargets = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          address = lib.mkOption { type = lib.types.str; description = "`address:port` of one Flux controller's metrics NodePort."; };
          controller = lib.mkOption { type = lib.types.str; description = "The `controller` label -- which of the four this is."; };
        };
      });
      default = [
        { address = "10.20.0.12:30081"; controller = "source-controller"; }
        { address = "10.20.0.12:30082"; controller = "kustomize-controller"; }
        { address = "10.20.0.12:30083"; controller = "image-reflector-controller"; }
        { address = "10.20.0.12:30084"; controller = "image-automation-controller"; }
      ];
      description = ''
        The Flux controllers' metrics endpoints, reached through NodePorts on the private subnet.

        WHY ALL FOUR AND NOT JUST THE IMAGE ONES. A deploy needs every one of them:
        source-controller fetches the commit, image-reflector-controller scans the registry,
        image-automation-controller commits the winning tag, kustomize-controller applies it.
        Watching only the two with "image" in the name would leave the halves that fetch and apply
        unwatched, which is the "rule that looks like coverage" failure k3s.rules warns about.

        THE PORTS ARE FIXED AND MUST AGREE WITH movies-gitops/flux-metrics/services.yaml, for
        the same reason kube-state-metrics' 30080 is fixed. A disagreement is a target that sits
        down and reads as Flux being broken.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # CREATE THE DIRECTORIES ON THE VOLUME. Neither service does this for itself and `StateDirectory=`
    # cannot help: it creates /var/lib/<name>, whereas these live at a NESTED path on a SEPARATE
    # MOUNT, which systemd will not create on the unit's behalf.
    #
    # WITHOUT THESE, PROMETHEUS DOES NOT FAIL IN A WAY THAT NAMES THE PROBLEM. Its sandbox tries to
    # bind-mount a directory that is not there and the unit dies at `status=226/NAMESPACE` with
    # "Failed to set up mount namespacing", which reads as a systemd hardening fault rather than a
    # missing folder. Hit on monitoring-1's first deploy, 2026-08-29, on a freshly formatted volume
    # whose only content was lost+found.
    #
    # 0700 and owned by the service user: the TSDB and the alert silences are not readable by
    # anything else on the box, which on this host includes a k3s control plane and whatever it runs.
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0700 prometheus prometheus -"
      "d ${cfg.alertmanagerDataDir} 0700 alertmanager alertmanager -"
    ];

    assertions = [
      {
        assertion = !config.fleet.grafana.enable || cfg.grafanaPort == config.fleet.grafana.port;
        message = ''
          fleet.prometheus.grafanaPort (${toString cfg.grafanaPort}) does not match
          fleet.grafana.port (${toString config.fleet.grafana.port}). Prometheus would scrape a
          closed port and the `grafana` target would sit down -- which is indistinguishable from
          Grafana being down, on the one host where that distinction matters.
        '';
      }
    ];

    # OWNED BY THE PROCESS THAT READS IT, 0400. Alertmanager reads the bot token at send time
    # (`bot_token_file`) -- it is never interpolated into a config file, so it never reaches the
    # store, and a file the wrong process owns fails at the moment it is needed rather than at
    # start. A `prometheus/fly-token` sat beside it, read at scrape time by the Fly federation job,
    # until both went on 2026-09-04.
    sops.secrets."alertmanager/telegram-bot-token" = { owner = "alertmanager"; mode = "0400"; };
    # Same shape, same reason, for the SMTP relay's API key: owned by the process that reads it,
    # read at send time, never interpolated into a config file.
    sops.secrets."alertmanager/smtp-password" = { owner = "alertmanager"; mode = "0400"; };

    users.users.prometheus = { isSystemUser = true; group = "prometheus"; description = "Prometheus"; };
    users.groups.prometheus = { };
    users.users.alertmanager = { isSystemUser = true; group = "alertmanager"; description = "Alertmanager"; };
    users.groups.alertmanager = { };

    # THE PATHS ARE /etc/prometheus AND /etc/alertmanager, not store paths, and that is a choice
    # about the people reading them rather than about Nix. Every runbook and every alert annotation
    # that says "check /etc/prometheus/rules" is read by somebody at an unsociable hour while the
    # thing it describes is misbehaving; a store hash in that sentence would be correct and useless.
    environment.etc = {
      "prometheus/prometheus.yaml".source = render "prometheus.yaml" ../../files/monitoring/prometheus.yaml;
      "alertmanager/alertmanager.yaml".source = render "alertmanager.yaml" ../../files/monitoring/alertmanager.yaml;
      "prometheus/scrape.d/node-targets.yaml".text = nodeTargetsYaml;
      "prometheus/scrape.d/mongodb-targets.yaml".text = mongodbTargetsYaml;
      "prometheus/scrape.d/kube-state-metrics-targets.yaml".text = kubeStateMetricsYaml;
      "prometheus/scrape.d/flux-targets.yaml".text = fluxYaml;

      # THE APPLICATION'S OWN METRICS. Every tier of every country, reached by NodePort over the
      # private subnet, needing no credential anyone can revoke. A `scrape-fly.yaml` sat beside
      # this behind a `scrapeFly` flag, federating Fly's managed Prometheus; it was never turned on
      # -- an org-wide token must not sit on the host that also runs the k3s control plane, and the
      # read-only ones were revoked -- and it went on 2026-09-04 with the platform.
      "prometheus/scrape.d/kinowo-apps.yaml".source =
        render "kinowo-apps.yaml" ../../files/monitoring/scrape-kinowo-apps.yaml;
    } // lib.listToAttrs (map
      (n: lib.nameValuePair "prometheus/rules/${n}.rules" {
        source = render "${n}.rules" (../../files/monitoring/rules + "/${n}.rules");
      })
      ruleNames);

    systemd.services.prometheus = {
      description = "Prometheus";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      unitConfig.RequiresMountsFor = builtins.dirOf cfg.dataDir;

      # WITHOUT THIS, A CONFIG CHANGE IS WRITTEN AND NEVER READ, and every signal says otherwise.
      # These files are in `environment.etc`, so activation replaces them -- but the UNIT is
      # unchanged, systemd restarts nothing, and Prometheus goes on serving the config it parsed at
      # its last start while the closure hash advances and the host reports itself current. The
      # bitcashier fleet measured exactly this: a scrape target correct on disk and absent from
      # /api/v1/targets until a restart. A new alert rule that never loads is silent in precisely
      # the way an unloaded rule file is.
      #
      # `restartTriggers` and not `reloadTriggers`: `--web.enable-lifecycle` is deliberately NOT set
      # below, so POST /-/reload answers 404 and there is no reload path to use. A restart costs a
      # few seconds of scrape gap and loses nothing -- the TSDB is on disk and the WAL is replayed.
      restartTriggers = [
        config.environment.etc."prometheus/prometheus.yaml".source
        config.environment.etc."prometheus/scrape.d/node-targets.yaml".text
        config.environment.etc."prometheus/scrape.d/mongodb-targets.yaml".text
        config.environment.etc."prometheus/scrape.d/kube-state-metrics-targets.yaml".text
        config.environment.etc."prometheus/scrape.d/kinowo-apps.yaml".source
      ]
      ++ map (n: config.environment.etc."prometheus/rules/${n}.rules".source) ruleNames;

      serviceConfig = {
        User = "prometheus";
        Group = "prometheus";
        Restart = "on-failure";
        RestartSec = 5;
        ExecStart = lib.concatStringsSep " " [
          "${cfg.package}/bin/prometheus"
          "--config.file=/etc/prometheus/prometheus.yaml"
          "--storage.tsdb.path=${cfg.dataDir}"
          "--storage.tsdb.retention.time=${cfg.retention}"
          "--storage.tsdb.retention.size=${cfg.retentionSize}"
          "--web.listen-address=${cfg.listenAddress}:9090"
          # NOT --web.enable-admin-api, and not --web.enable-lifecycle. The first exposes
          # POST /api/v1/admin/tsdb/delete_series -- a delete button on the only copy of this
          # fleet's metric history, reachable by anything on the private network. The second
          # exposes /-/quit beside the reload it would provide, and the restartTriggers above make
          # reload unnecessary.
        ];

        # SEE THE HEADER. This is the process that has to still be recording when its neighbour
        # starves the box. 400 against systemd's default of 100 and k3s's, which k3s-server.nix
        # leaves at the default deliberately and says so.
        CPUWeight = 400;
        IOSchedulingClass = "best-effort";
        # 0 is the highest best-effort priority. NOT the `realtime` class: that one can starve
        # every other reader on the device, including the k3s control plane's etcd, and an alarm
        # that wins by taking down what it is watching has not helped.
        IOSchedulingPriority = 0;

        # Prometheus checkpoints its head block on shutdown; killing it mid-checkpoint costs a WAL
        # replay on the next start.
        KillSignal = "SIGTERM";
        TimeoutStopSec = 600;

        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.dataDir ];
      };
    };

    systemd.services.alertmanager = {
      description = "Alertmanager";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      unitConfig.RequiresMountsFor = builtins.dirOf cfg.alertmanagerDataDir;

      # The same gap as Prometheus's, and it matters more here: this file decides where an alert is
      # DELIVERED. A changed route or receiver that is written and never read means the page goes to
      # the old place, or nowhere, while everything reports the change as applied.
      restartTriggers = [ config.environment.etc."alertmanager/alertmanager.yaml".source ];

      serviceConfig = {
        User = "alertmanager";
        Group = "alertmanager";
        Restart = "on-failure";
        RestartSec = 5;
        ExecStart = lib.concatStringsSep " " [
          "${cfg.alertmanagerPackage}/bin/alertmanager"
          "--config.file=/etc/alertmanager/alertmanager.yaml"
          "--storage.path=${cfg.alertmanagerDataDir}"
          "--web.external-url=${cfg.externalUrl}"
          "--web.listen-address=${cfg.listenAddress}:9093"
          # NO --cluster.listen-address. A single Alertmanager needs no gossip, and leaving the
          # cluster listener on its default would open a port that only exists to talk to peers
          # this fleet does not have. If a second one is ever added, this line comes back and 9094
          # goes into the firewall list below -- both, or neither: a clustered Alertmanager whose
          # peers cannot reach each other sends every notification twice.
        ];

        # Above k3s, for the reason in the header. Slightly below Prometheus: if only one of the
        # two can run, recording the incident is worth more than the notification about it, and the
        # notification is retried while a scrape gap is permanent.
        CPUWeight = 300;
        IOSchedulingClass = "best-effort";
        IOSchedulingPriority = 1;

        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.alertmanagerDataDir ];
      };
    };

    # NO FIREWALL RULES HERE, BY THIS FLEET'S CONVENTION. `fleet.firewall.monitoring = true;` in the
    # host file opens 9090, 9093 and 3000 on the private interface, and
    # modules/fleet/firewall.nix keeps every port this fleet opens in one readable list. A role
    # that opened its own would be a second, invisible source of the same rule.
    #
    # WHAT IS DELIBERATELY NOT IN THAT LIST is 9094, Alertmanager's cluster port -- there is one
    # Alertmanager and it is not started with a cluster listener (see the ExecStart above). If a
    # second one is ever added, the flag and the port move together.
  };
}
