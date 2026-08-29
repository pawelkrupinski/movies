# Where this host's logs go -- the shipper half of roles/victoria-logs.nix.
#
# THIS IS THE FILE THAT DECIDES WHETHER ANYBODY CAN INVESTIGATE A MACHINE THEY CANNOT SSH TO.
# modules/fleet/default.nix's header names the gap this fills: "VICTORIALOGS / promtail -- nothing
# ships logs off these boxes yet. `services.journald.storage = "persistent"` is the whole of the
# log retention story, and it is deliberately the first thing to revisit when an incident here
# needs evidence from a host that has rebooted." A persistent journal answers "what did this box
# do" only while the box is reachable, and it cannot answer "what did all three do in the same
# minute" at all.
#
# ==================================================================================================
# VECTOR, NOT PROMTAIL -- THE ONE REAL DIVERGENCE FROM bitcashier, AND WHY
# ==================================================================================================
#
# bitcashier ships with TWO promtail processes per host plus rsyslog, and every part of that shape
# is load-bearing OVER THERE and buys nothing HERE:
#
#   * TWO PROCESSES exist because promtail fans out to its clients from one goroutine over
#     UNBUFFERED channels, so a slow VictoriaLogs would stall shipping to Loki. There is no Loki
#     here and never was -- one destination, so nothing to fan out to and nothing to stall.
#   * rsyslog exists because that fleet's label set is a WIRE CONTRACT with dashboards that select
#     `{job="syslog", filename="/var/log/syslog"}`, and its pinned promtail 3.3.2 is built without
#     libsystemd and physically cannot read a journal. This fleet has no existing log dashboards to
#     be compatible with, so paying for a syslog daemon, a second copy of every line on disk, and a
#     logrotate rule to buy a label nobody queries yet would be porting the workaround without the
#     problem.
#   * THE PINNED PACKAGE (nix/packages/promtail-pinned.nix) exists to match a Puppet-managed half of
#     the same estate version for version. There is no second half here.
#
# So the shape is one agent reading the journal directly. Vector is chosen over VictoriaLogs' own
# `-journald.*` collector for one reason that decides it: THAT COLLECTOR ONLY READS THE LOCAL
# JOURNAL. It would cover monitoring-1 and leave mongo-1 and k3s-worker-1 -- the two machines you
# actually cannot ssh to when they are the problem -- shipping nothing. It also cannot read
# /var/log/pods at all, and see the Kubernetes section below for why that matters.
#
# WHAT IS KEPT IDENTICAL, so the two fleets stay comparable: the same store, on the same port, with
# the same `_stream_fields` discipline and the same "drop rather than retry forever" answer to the
# store being down. A query written against one fleet's VictoriaLogs means the same thing against
# the other's.
#
# ==================================================================================================
# HOW THIS SURVIVES VictoriaLogs BEING DOWN, WHICH IS A REQUIREMENT AND NOT A NICE-TO-HAVE
# ==================================================================================================
#
# The rule this whole file is written against: LOG SHIPPING MUST NEVER BE ABLE TO WEDGE A HOST.
# A monitoring component that can take down the thing it monitors is worse than no monitoring, and
# a log shipper is the component most likely to try -- its input is unbounded, its output is a
# machine that might be down, and the naive arrangement (block until the store accepts) turns an
# outage of one host into an outage of three. Four mechanisms, each covering a different half of it:
#
#   1. A DISK BUFFER on the sink, sized `bufferMaxSizeBytes`, under /var/lib/vector. While
#      VictoriaLogs is unreachable, events accumulate on local disk and are replayed when it comes
#      back -- so a monitoring-1 reboot costs nothing but a delay. It is a BOUNDED file, so the
#      worst case is a known number of megabytes on the root disk, not a full one.
#   2. `when_full: drop_newest` ON THAT BUFFER. This is the load-bearing line. Vector's default is
#      `block`, which applies backpressure all the way to the source -- and a blocked source is a
#      stopped source, which stops advancing its journal checkpoint, and the journal then rotates
#      underneath it and the logs are lost ANYWAY, silently, with a hung process to explain. Once
#      the buffer is full, dropping the newest events keeps the agent reading and keeps it current.
#      This is the same decision bitcashier makes with its VictoriaLogs client's five-retry backoff
#      and states in the same terms: for THIS store, dropping is the right answer.
#   3. `healthcheck.enabled: false`. Vector checks a sink's health at STARTUP; with it on, a host
#      that reboots while VictoriaLogs is down comes up with a shipper that refused to start, which
#      is precisely the moment its logs matter most. VictoriaLogs' own Vector documentation turns
#      this off for its own reason (its endpoint does not answer Elasticsearch's health probe), and
#      the availability argument is the better one.
#   4. THE JOURNAL IS THE SOURCE OF TRUTH, NOT THIS AGENT. Nothing here writes, rotates or consumes
#      the journal; journald keeps its own bounded ring on disk regardless. If vector dies, is
#      OOM-killed, or is stopped for an hour, the journal is untouched and the checkpoint means the
#      agent resumes where it left off rather than skipping to now. The unit below is deliberately
#      unable to hurt the host: bounded memory, the lowest I/O priority, a CPU weight below
#      everything else on the box, and a short stop timeout so a flush that cannot complete never
#      delays a reboot.
#
# WHAT IS STILL LOST, stated because a buffer sold as "no loss" is a lie: if VictoriaLogs is down
# for longer than the buffer holds, the newest events are dropped and nothing records which. The
# journal on the host still has them for as long as journald keeps them, so the recovery is manual
# and possible rather than automatic and invisible. Nothing alerts on this yet -- an alert on
# `vector_buffer_events_dropped_total` would be the right follow-up, and it needs a scrape target
# this module does not expose (see `api.enabled` below).
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.logs;

  yaml = pkgs.formats.yaml { };

  # ------------------------------------------------------------------------------------------------
  # ONE VRL SNIPPET, USED BY BOTH SOURCES' remap TRANSFORMS -- the fleet's rule about extracting a
  # repeated multi-line shape at the SECOND use, not the third.
  # ------------------------------------------------------------------------------------------------
  #
  # WHAT IT SOLVES is the exact trap bitcashier's fleet/logs.nix records at length under
  # `_msg_field`. Services here log JSON (Logback), and a JSON line shipped as an opaque string
  # gives a store where every application log row renders as `{"@timestamp":...` and none of the
  # fields inside it -- `level`, `logger`, the message text -- can be queried at all. That store
  # looks perfectly healthy right up until somebody reads a service log.
  #
  # THE APP'S FIELDS LOSE EVERY CONFLICT (`merge(fields, .)` puts OUR event second, and the second
  # argument wins), with one deliberate exception below. That ordering is what stops an application
  # renaming this host by logging a `host` field of its own.
  #
  # THE EXCEPTION IS `message`, taken from the parsed JSON when it has one, because that is the
  # human-readable text and it is what `_msg_field=message` on the sink then files as `_msg`.
  #
  # THE TIMESTAMP IS DELIBERATELY *NOT* TAKEN FROM THE APPLICATION, and it is restored below after
  # the merge for that reason. bitcashier states the argument and it holds here: trusting an
  # application-supplied timestamp lets one service with a skewed clock file its logs in the wrong
  # time window entirely -- which is undetectable and ruins the one thing a log store is for,
  # ordering events across machines.
  #
  # UNBOUNDED FIELD NAMES ARE FINE HERE and this is worth being explicit about: VictoriaLogs
  # ingests every field, but only those named in `_stream_fields` on the sink URL shard the data.
  # An application that invents a new field per request costs storage, not stream explosion.
  mergeJsonBodyVrl = ''
    # Preserve the ingest-time timestamp across the merge -- see the note above about clock skew.
    original_timestamp = .timestamp

    parsed, parse_error = parse_json(.message)
    if parse_error == null {
      fields = object(parsed) ?? {}

      # Our own event second: on a name collision, the shipper's value wins.
      . = merge(fields, .)

      # ...except the message text itself, which is the whole reason for parsing.
      inner_message = fields.message
      if !is_null(inner_message) {
        .message = inner_message
      }
    }

    .timestamp = original_timestamp
  '';

  # ------------------------------------------------------------------------------------------------
  # THE JOURNAL
  # ------------------------------------------------------------------------------------------------
  journalSources = {
    journal = {
      type = "journald";

      # THE WHOLE JOURNAL, NOT JUST THIS BOOT AND NOT JUST FROM NOW. `current_boot_only` would drop
      # everything the PREVIOUS boot said -- which, on a host that rebooted unexpectedly, is the
      # only evidence there is; `since_now` would do the same on every agent restart. Both default
      # to the safe value in current Vector; stated anyway, because the cost of a silent default
      # flip here is exactly the log lines an incident is about.
      #
      # The one-time cost is that a fresh host ships its whole retained journal on first start.
      # That is a bounded burst against a store on the same /24, and it happens once.
      current_boot_only = false;
      since_now = false;
    };
  };

  journalTransforms = {
    journal_fields = {
      type = "remap";
      inputs = [ "journal" ];
      source = ''
        # THE SYSTEMD UNIT. `_SYSTEMD_UNIT` is the name Vector's journald source documents -- it
        # passes journal fields through under their original names -- and it is what this expects.
        # The other two branches are a deliberate hedge, not indecision: the failure of getting
        # this name wrong is NOT an error, it is a store where every line is labelled `unknown`,
        # which nobody discovers until they try to filter by unit during an incident. The third
        # branch is a real case rather than a hedge: kernel and syslog-only entries have no unit at
        # all, and `SYSLOG_IDENTIFIER` is the closest thing they carry.
        # The QUOTED path form (`."_SYSTEMD_UNIT"`) rather than the bare one, because the field name
        # begins with an underscore and quoting is the spelling VRL guarantees for a field name
        # that is not a plain identifier.
        unit = ."_SYSTEMD_UNIT"
        if is_null(unit) { unit = .systemd_unit }
        if is_null(unit) { unit = .SYSLOG_IDENTIFIER }
        if is_null(unit) { unit = "unknown" }
        .unit = unit

        # THE STREAM FIELDS, SET HERE RATHER THAN TRUSTED FROM THE JOURNAL. `host` from the journal
        # is `_HOSTNAME`, which is whatever the box called itself when the line was written -- so a
        # rename would silently split one machine's history into two streams. This is the name
        # NixOS gave it, which is the name every other part of this fleet uses.
        .host = "${config.networking.hostName}"
        .job = "journal"
        .env = "${config.fleet.environment}"
        .role = "${config.fleet.role}"

        ${mergeJsonBodyVrl}
      '';
    };
  };

  # ------------------------------------------------------------------------------------------------
  # KUBERNETES POD LOGS -- the port of bitcashier's roles/k8s-pod-logs.nix
  # ------------------------------------------------------------------------------------------------
  #
  # WHY IT IS A SEPARATE SOURCE AT ALL: k3s runs containers under containerd, and containerd writes
  # each container's stdout/stderr to /var/log/pods -- NOT to the journal. So a host with only the
  # journald source above ships k3s's own units and NOTHING THE CLUSTER RUNS. bitcashier measured
  # exactly that shape when its workloads moved from Nomad to Kubernetes and nothing replaced the
  # alloc-log scrape: a `_time:5m` query returned syslog and nothing else, for a day, and nothing
  # alerted -- because a stream that has stopped is indistinguishable from one nobody asked about.
  # k3s-worker-1 exists to run the app worker pod, so on that host this source IS the point.
  #
  # THE FORMAT IS NOT THE APPLICATION'S. The CRI runtime prefixes every line:
  #
  #     2026-08-27T16:25:59.885398633Z stdout F {"@timestamp":"...","message":"..."}
  #
  # so the application's own output is the fourth field. Without stripping that prefix the JSON
  # never parses, every field the logs are worth having for is gone, and the line still ARRIVES --
  # half-working, which is the failure mode to avoid, because it looks like success.
  #
  # `env` COMES FROM THE PATH, NOT FROM THIS HOST. A worker's own environment is the fleet's; the
  # pods on it belong to whatever namespace deployed them, and the namespace is right there in the
  # directory name (`/var/log/pods/<namespace>_<pod>_<uid>/<container>/<n>.log`).
  #
  # THE POD NAME IS DELIBERATELY NOT A STREAM FIELD. Its value carries a fresh ReplicaSet hash on
  # every deploy, so sharding on it would mint streams for ever with no ceiling and no way to
  # un-ingest them. It is still stored in `file` and still searchable; it simply does not shard.
  # `container` and `namespace` ARE stream fields: both are bounded by what is deployed.
  podLogSources = {
    pod_logs = {
      type = "file";
      include = [ "${cfg.kubernetesPodLogs.logRoot}/*/*/*.log" ];

      # ROTATED FILES ARE NOT RE-READ FROM THE TOP. Vector fingerprints by content rather than by
      # inode/path, so a kubelet log rotation does not re-ship the file.
      #
      # `read_from = "beginning"` so a pod that started before vector did is not truncated to
      # whatever it happened to say after the agent came up -- which is the log of a crash-looping
      # container, i.e. the one you want.
      read_from = "beginning";
    };
  };

  podLogTransforms = {
    pod_log_fields = {
      type = "remap";
      inputs = [ "pod_logs" ];
      source = ''
        .host = "${config.networking.hostName}"
        .job = "kubernetes"
        .role = "${config.fleet.role}"

        # STRIP THE CRI PREFIX. Vector has no dedicated CRI parser, so this is the regex form of
        # promtail's `cri` pipeline stage: timestamp, stream (stdout/stderr), a partial/full flag,
        # then the line the container actually wrote.
        #
        # A LINE THAT DOES NOT MATCH IS KEPT AS-IS rather than dropped. If the runtime's format ever
        # changes, the result should be logs that look wrong -- which somebody notices -- and not
        # an empty `job="kubernetes"` stream, which nobody does.
        cri, cri_error = parse_regex(.message, r'^(?P<ts>\S+) (?P<stream>stdout|stderr) (?P<flag>\S+) (?P<line>.*)$')
        if cri_error == null {
          .type = cri.stream
          .message = cri.line

          # The runtime's own timestamp for the line, which is closer to when the application
          # emitted it than vector's read time. Still the SHIPPING SIDE's clock, not the
          # application's -- see mergeJsonBodyVrl for why that distinction is kept.
          .timestamp = parse_timestamp(cri.ts, "%+") ?? .timestamp
        }

        # NAMESPACE AND CONTAINER OUT OF THE PATH. Anchored at the start and stopping at the first
        # `_`, because a POD name may contain `-` but a NAMESPACE may not contain `_` -- so the
        # first underscore is an unambiguous boundary.
        pod_path, pod_path_error = parse_regex(.file, r'^${cfg.kubernetesPodLogs.logRoot}/(?P<namespace>[^_/]+)_[^/]+/(?P<container>[^/]+)/')
        if pod_path_error == null {
          .namespace = pod_path.namespace
          .container = pod_path.container
          .env = pod_path.namespace
        } else {
          .env = "${config.fleet.environment}"
        }

        ${mergeJsonBodyVrl}
      '';
    };
  };

  sourceInputs =
    [ "journal_fields" ]
    ++ lib.optional cfg.kubernetesPodLogs.enable "pod_log_fields";

  # ------------------------------------------------------------------------------------------------
  # THE WHOLE CONFIGURATION
  # ------------------------------------------------------------------------------------------------
  vectorConfig = {
    # CHECKPOINTS AND THE DISK BUFFER BOTH LIVE HERE. The journal checkpoint is what makes a
    # restart resume rather than skip; losing this directory costs a re-ship of whatever the
    # journal still holds, which is noisy but not lossy.
    data_dir = cfg.stateDir;

    # NO API LISTENER. Vector's HTTP API is a playground endpoint with no authentication, and
    # nothing on this fleet reads it. Off explicitly rather than by default, because "off" is the
    # decision: if `vector_buffer_events_dropped_total` is ever wanted in Prometheus (it should be
    # -- see the header), the way to get it is a `prometheus_exporter` SINK bound to
    # `fleet.privateAddress` plus a scrape job and a firewall port, not this.
    api.enabled = false;

    sources = journalSources // lib.optionalAttrs cfg.kubernetesPodLogs.enable podLogSources;
    transforms = journalTransforms // lib.optionalAttrs cfg.kubernetesPodLogs.enable podLogTransforms;

    sinks.victoria_logs = {
      # THE ELASTICSEARCH BULK ENDPOINT, which is what VictoriaLogs documents for Vector. Note this
      # is a DIFFERENT ingestion path from bitcashier's, which pushes to the Loki-compatible
      # `/insert/loki/api/v1/push` because promtail speaks Loki and nothing else. Same store, same
      # port, same `_stream_fields` semantics on the query string -- the difference is that this
      # path carries STRUCTURED FIELDS, where Loki's carries labels plus one opaque line. Given the
      # JSON handling above, that is the whole reason this endpoint is the right one here.
      type = "elasticsearch";
      inputs = sourceInputs;
      endpoints = [ "http://${cfg.serverAddress}:${toString cfg.serverPort}/insert/elasticsearch/" ];

      # NO `mode`, MATCHING VictoriaLogs' DOCUMENTED EXAMPLE. Vector's Elasticsearch sink defaults
      # to `bulk`, which is the only mode VictoriaLogs' endpoint accepts, and a key omitted cannot
      # be set to something the store does not implement.
      api_version = "v8";

      # OVER A PRIVATE /24 THIS IS ABOUT THE STORE'S DISK AND CPU, NOT THE WIRE: gzip trades a
      # little of the shipping host's CPU (which is throttled anyway, see the unit) for less to
      # receive on a 2-core box that is also running Prometheus.
      compression = "gzip";

      # SEE MECHANISM 3 IN THE HEADER. A startup health check would mean a host that reboots while
      # VictoriaLogs is down comes up with no shipper at all.
      healthcheck.enabled = false;

      # HOW VictoriaLogs IS TOLD TO INTERPRET WHAT ARRIVES. These three are the ingestion contract
      # and they are the same three bitcashier encodes in its push URL.
      query = {
        # FIRST-NON-EMPTY-WINS, and the order matters. `message` is what both remaps above leave the
        # human-readable text in; `_msg` catches anything that already used VictoriaLogs' own name.
        # WITHOUT THIS, rows store the literal placeholder "missing _msg field; see ..." and the UI
        # renders that instead of the log line -- a store that looks healthy unless you read it.
        _msg_field = "message,_msg";

        # The shipper's clock, deliberately -- never a field the application supplied. See
        # mergeJsonBodyVrl.
        _time_field = "timestamp";

        # WHAT SHARDS THE DATA, AND THE ONE SETTING HERE THAT IS NOT OPTIONAL. Unset, VictoriaLogs
        # makes EVERY field part of the stream identity -- and several of ours are unbounded (a
        # pod name, an application's own fields), so the default would mint streams for ever with
        # no ceiling and no way to un-ingest them.
        #
        # EVERY NAME BELOW IS BOUNDED BY CONSTRUCTION, which is the only test that matters:
        #   host      -- three machines
        #   job       -- `journal` or `kubernetes`
        #   env       -- `prod`, plus one per k8s namespace
        #   role      -- mongo | monitoring | k3s-worker
        #   unit      -- systemd units on a host: tens, and they are named by the closure
        #   namespace -- k8s namespaces: a handful
        #   container -- container names, NOT pod names: bounded by what is deployed
        #   type      -- stdout | stderr
        # A field that never arrives (a journal line has no `container`) simply does not
        # participate, so one list serves both sources.
        _stream_fields = "host,job,env,role,unit,namespace,container,type";
      };

      # SEE MECHANISMS 1 AND 2 IN THE HEADER. This is the part that makes VictoriaLogs being down a
      # delay instead of an incident.
      buffer = {
        type = "disk";
        max_size = cfg.bufferMaxSizeBytes;
        when_full = "drop_newest";
      };

      request = {
        # RETRY FOREVER IS SAFE *BECAUSE* OF THE BUFFER ABOVE, and only because of it: the sink
        # retries its head batch while the source keeps reading into the buffer, and the buffer's
        # `drop_newest` is what bounds the whole thing. Without the buffer this would be the
        # classic shipper wedge.
        retry_max_duration_secs = 60;

        # Enough that a busy pod does not queue behind one slow round-trip; small enough that a
        # 2-core store is not asked to absorb the whole fleet at once.
        concurrency = 4;
      };
    };
  };

  configFile = yaml.generate "vector.yaml" vectorConfig;
in
{
  options.fleet.logs = {
    enable = lib.mkEnableOption "shipping this host's journal to the fleet's VictoriaLogs";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.vector;
      defaultText = "pkgs.vector";
      description = "From the pinned nixpkgs, for the reason roles/victoria-logs.nix gives for its own package option.";
    };

    serverAddress = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        The address of the host running `fleet.victoriaLogs` -- monitoring-1 -- as a bare IPv4.

        AN ADDRESS, NOT A NAME, for the same reason roles/k3s-agent.nix gives for `serverAddr`:
        this is read on a freshly built host before anything resolves reliably, and a shipper that
        failed for want of a resolver is diagnosed as a firewall or credential problem for far
        longer than it deserves. bitcashier uses a flat Consul name here and needs a whole
        systemd-resolved stanza to make it resolvable; there is no Consul on this fleet.

        NO DEFAULT, so that a host cannot half-enable this and ship nowhere. Like `k3sAgent.serverAddr`
        it is EXPECTED TO BE READ IN flake.nix off monitoring-1's own `fleet.privateAddress`, so
        that no literal address exists to outlive the machine it named. Until that wiring exists the
        host files carry the literal, exactly as hosts/k3s-worker-1 already does for its cluster
        join -- which is the precedent, and also the reason the wiring is worth doing once for both.
      '';
    };

    serverPort = lib.mkOption {
      type = lib.types.port;
      default = 9428;
      description = ''
        VictoriaLogs' port ON monitoring-1 -- which is `fleet.victoriaLogs.port` over there, not
        here, so it is a plain default rather than a reference. If that host ever moves off 9428,
        every shipper has to be told, and the symptom of forgetting is a fleet that quietly stops
        logging while every unit stays green.
      '';
    };

    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/vector";
      description = ''
        Checkpoints and the sink's disk buffer. ON THE ROOT DISK, DELIBERATELY, on every host
        including monitoring-1: this must work on hosts that have no volume at all, and its size is
        bounded by `bufferMaxSizeBytes` rather than by whatever disk it lands on. Created and owned
        by `StateDirectory=` in the unit below -- unlike the monitoring volume's directories, this
        is a plain path under /var/lib and systemd can make it.
      '';
    };

    bufferMaxSizeBytes = lib.mkOption {
      type = lib.types.int;
      default = 268435488;
      description = ''
        THE CEILING ON WHAT AN OUTAGE COSTS THE HOST -- the size of the on-disk sink buffer, in
        bytes. ~256 MiB, which is Vector's documented MINIMUM for a disk buffer (268435488 exactly,
        not 268435456 -- it refuses to start on a smaller value, and the odd number is not a typo).

        THE MINIMUM IS ALSO THE RIGHT VALUE HERE, which is convenient rather than lucky: the point
        of the buffer is to ride out a monitoring-1 reboot or a VictoriaLogs restart -- minutes --
        not to be an offline log store. A bigger buffer does not buy a longer outage of any
        interesting length, and it does put more of the ROOT disk of a database host at the disposal
        of a log shipper. The journal is still on the host either way; see the header for what a
        longer outage actually loses.

        HOW LONG 256 MiB LASTS IS A GUESS AND NOT A MEASUREMENT. Three hosts' journals are a few
        hundred MB/day uncompressed between them, which puts a single host's share at hours -- but
        the app worker pod on k3s-worker-1 is application output and could be an order of magnitude
        either side of that. Revisit against `vector_buffer_byte_size` once there is one to read.
      '';
    };

    kubernetesPodLogs = {
      enable = lib.mkEnableOption ''
        reading container logs from /var/log/pods as well as the journal.

        ON FOR ANY HOST THAT RUNS PODS. containerd does NOT write container output to the journal,
        so without this a k3s node ships k3s's own units and nothing the cluster runs -- see the
        long note above podLogSources
      '';

      logRoot = lib.mkOption {
        type = lib.types.str;
        default = "/var/log/pods";
        description = ''
          Where the CRI runtime writes container logs; k3s uses the kubelet default. The layout is
          `<logRoot>/<namespace>_<pod>_<uid>/<container>/<restart-count>.log`, and BOTH regexes in
          the transform above depend on that shape -- a different root is fine, a different layout
          is not.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.serverAddress != "";
        message = ''
          fleet.logs.serverAddress is required: with nowhere to ship to, vector starts, reads the
          journal, and discards it -- which looks exactly like a working shipper from the host's
          side. Set it to monitoring-1's private address.
        '';
      }
    ];

    # THE CONFIG LIVES AT /etc/vector/vector.yaml, NOT AT A STORE PATH, and that is a choice about
    # the person reading it rather than about Nix -- the same one roles/prometheus.nix makes and
    # for the same reason: a runbook that says "check /etc/vector/vector.yaml" is read at an
    # unsociable hour while the thing it describes is misbehaving, and a store hash in that
    # sentence would be correct and useless.
    environment.etc."vector/vector.yaml".source = configFile;

    systemd.services.vector = {
      description = "Vector -- ships this host's logs to VictoriaLogs";
      documentation = [ "https://vector.dev/docs/" ];
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      # WITHOUT THIS, A CONFIG CHANGE IS WRITTEN AND NEVER READ. The file is in `environment.etc`,
      # so activation replaces it -- but the UNIT is unchanged, systemd restarts nothing, and
      # vector goes on running the topology it parsed at its last start while the closure hash
      # advances and the host reports itself current. roles/prometheus.nix carries the same trigger
      # against the same trap, which bitcashier measured on its own fleet.
      restartTriggers = [ configFile ];

      # VECTOR'S journald SOURCE SHELLS OUT TO `journalctl`. Left to the ambient PATH this works by
      # accident on NixOS (systemd is in systemPackages) and fails on any host where it is not --
      # as a source that produces no events, not as an error. Naming the closure's own systemd is
      # one line and removes the accident.
      path = [ pkgs.systemd ];

      serviceConfig = {
        ExecStart = "${cfg.package}/bin/vector --config /etc/vector/vector.yaml";

        # `always`, NOT `on-failure`. A shipper that exits cleanly for any reason must come back;
        # the state that must never persist is "this host stopped logging and nothing said so".
        # RestartSec is long enough that a genuinely broken config presents as a slow restart loop
        # in `systemctl status` rather than as a hot one that hides the error message.
        Restart = "always";
        RestartSec = "10s";

        # ROOT, AND NOT BY OVERSIGHT -- the same conclusion bitcashier's promtail unit reaches and
        # writes down. Two things need it and neither can be had another way: the journal
        # (`SupplementaryGroups=systemd-journal` would cover this half) and /var/log/pods, whose
        # directories are root-owned 0755 with no group anything else can join. `DynamicUser` would
        # additionally change the ownership of the checkpoint directory on every rebuild.
        #
        # What narrows it is the sandbox below: the process may write exactly one directory, has no
        # device access, and cannot gain privileges.
        User = "root";
        Group = "root";

        StateDirectory = builtins.baseNameOf cfg.stateDir;
        StateDirectoryMode = "0700";

        # ------------------------------------------------------------------------------------
        # THE HOST MUST ALWAYS WIN. See mechanism 4 in the header.
        # ------------------------------------------------------------------------------------
        #
        # A HARD MEMORY CEILING, so that if this process ever misbehaves the kernel kills THIS and
        # not mongod, Prometheus or the k3s control plane -- on a 4GB box the OOM killer takes the
        # largest process, and left unbounded a shipper replaying a large buffer could become it.
        # Losing the shipper costs log lines that are still in the journal; losing the database
        # costs the service. 512M is a ceiling, not a budget: normal use is a small fraction of it.
        MemoryMax = "512M";

        # BELOW EVERYTHING. systemd's default is 100; Prometheus runs at 400 and Alertmanager at
        # 300 on monitoring-1, and k3s and mongod sit at the default. Shipping logs is the least
        # urgent thing any of these machines does, and unlike a Prometheus scrape gap a delay here
        # is recoverable -- the events wait in the buffer.
        CPUWeight = 50;
        IOSchedulingClass = "best-effort";
        # 7 is the LOWEST best-effort priority. Reading the journal and flushing a buffer must never
        # compete with mongod's writes or etcd's fsyncs for the disk.
        IOSchedulingPriority = 7;

        # A FLUSH THAT CANNOT COMPLETE MUST NOT DELAY A REBOOT. If VictoriaLogs is unreachable,
        # vector's graceful shutdown waits on in-flight requests; without a short bound, `reboot`
        # on a host during an incident hangs for systemd's 90s default on the log shipper, of all
        # things. The disk buffer is durable, so what a SIGKILL costs here is at most the batch in
        # flight -- which is the same thing the outage was already costing.
        KillSignal = "SIGTERM";
        TimeoutStopSec = "30s";

        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        # `strict`, with exactly one writable path. Note that read access is UNAFFECTED by this --
        # /var/log/journal and /var/log/pods stay readable, which is all this process wants from
        # them.
        ProtectSystem = "strict";
        ReadWritePaths = [ cfg.stateDir ];
        UMask = "0077";
      };
    };

    # NO FIREWALL PORTS. This agent listens on nothing (`api.enabled = false`) and only makes
    # OUTBOUND connections to monitoring-1:9428, which is an established flow that conntrack passes
    # without any rule naming it. Said out loud because bitcashier's promtail DOES bind ports and a
    # reader comparing the two files would otherwise read the absence as an omission.
  };
}
