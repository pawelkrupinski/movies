# What this host tells monitoring-1 about itself.
#
# PORTED FROM bitcashier's `nix/modules/fleet/observability.nix`, MINUS THE HALF THAT WAS ABOUT
# PARITY. There, most of this file exists because the same fleet is half Puppet-managed and half
# NixOS, so its publishers are the SAME SHELL SCRIPTS the Puppet classes run, vendored byte for
# byte and guarded by a checker that fails when the two copies drift -- and its Consul registration
# is how Prometheus finds the host at all. Neither applies here: there is no second manager to
# agree with, and Prometheus on monitoring-1 has three static targets. So the publishers below are
# written as `writeShellApplication` inline, which is what vendoring was preventing, and the
# Consul/Vault-shaped stanzas are gone entirely rather than stubbed.
#
# WHAT SURVIVED THE PORT, AND WHY EACH ONE EARNED IT:
#
#   * THE TEXTFILE DIRECTORY, as an OPTION rather than a `let` binding, so ./nix-gc.nix and
#     ./auto-apply.nix write into it by reading one value instead of restating a path. Two copies
#     of a path is one rename away from a publisher writing somewhere nothing scrapes, which
#     produces no error anywhere: the file is written, the unit succeeds, and the metric simply
#     never appears.
#   * THE INVENTORY GAUGE. `machine{role,env,hostname} 1` is the fleet's own answer to "what hosts
#     exist and what is each one for", carried in labels rather than in a list somebody maintains.
#     Three hosts is small enough that this looks pointless and exactly small enough that a fourth
#     one silently missing from a dashboard would go unnoticed for months.
#   * THE REBOOT GAUGE, which is the one metric here that is hard to get right and easy to get
#     wrong -- see its own comment.
#   * node_exporter BOUND TO THE PRIVATE ADDRESS, with the two accommodations that a single-address
#     bind needs and that cost bitcashier a silent monitoring outage to discover.
#
# WHAT IS NEW HERE RATHER THAN PORTED: the auto-apply state publisher at the bottom. bitcashier
# keeps that in its staging module; it is here because on this fleet it is the ONLY series an alert
# can read when auto-apply itself is the thing that has stopped, which is the case it exists for.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.observability;
  textfileDir = cfg.textfileDirectory;

  # WHETHER THERE IS SOMETHING STAGED HERE THAT NOBODY HAS SWITCHED TO, and what this host is
  # actually running. See the unit below for why this is a timer rather than a value baked into
  # the closure.
  statePublisher = pkgs.writeShellApplication {
    name = "fleet-auto-apply-state";
    runtimeInputs = with pkgs; [ coreutils gnused ];
    text = ''
      metrics_file="''${METRICS_FILE:?METRICS_FILE is not set}"
      pin="''${STAGED_SYSTEM:?STAGED_SYSTEM is not set}"

      staged="$(readlink -f "''${pin}" 2>/dev/null || true)"
      running="$(readlink -f /run/current-system 2>/dev/null || true)"

      revision_of() {
          # The closure carries its own nixos-version; read the STAGED one's rather than this
          # host's, which is the whole point -- this host's is what it is already running.
          closure="$1"
          [ -x "''${closure}/sw/bin/nixos-version" ] || { echo ""; return; }
          # `|| true` ON THE WHOLE PIPELINE, because writeShellApplication runs this under
          # `set -euo pipefail` and a staged closure whose nixos-version cannot run would
          # otherwise abort the publisher -- which writes NO metrics file at all, so the one
          # series that says "something is staged here" disappears exactly when the staged thing
          # is the broken one.
          { "''${closure}/sw/bin/nixos-version" --json 2>/dev/null \
              | sed -n 's/.*"configurationRevision":"\([^"]*\)".*/\1/p'; } || true
      }

      tmp="$(mktemp)"
      {
        echo "# HELP nixos_staged_pending 1 when a closure is staged on this host that it is not running"
        echo "# TYPE nixos_staged_pending gauge"
        # `0` IS PUBLISHED DELIBERATELY WHEN NOTHING IS PENDING. A metric that simply vanishes when
        # all is well cannot be told from a host that stopped publishing, and those two need
        # opposite responses.
        if [ -z "''${staged}" ] || [ ! -e "''${staged}" ]; then
          echo "nixos_staged_pending 0"
        elif [ "''${staged}" = "''${running}" ]; then
          echo "nixos_staged_pending 0"
        else
          echo "nixos_staged_pending 1"
        fi

        if [ -n "''${staged}" ] && [ -e "''${staged}" ]; then
          revision="$(revision_of "''${staged}")"
          if [ -n "''${revision}" ]; then
            echo "# HELP nixos_staged_revision_info The git commit of the closure staged on this host"
            echo "# TYPE nixos_staged_revision_info gauge"
            echo "nixos_staged_revision_info{revision=\"''${revision}\"} 1"
          fi
        fi

        # THE STORE PATH HASH IS THE VERSION. It hashes the whole evaluated configuration, so two
        # hosts carrying the same value are running the same config byte for byte, and a host whose
        # path differs from what this flake evaluates to has not been deployed. One label answers
        # "is this host current" without needing a git revision at all -- which matters because a
        # closure built from a tree with no `.git` has no revision to report.
        echo "# HELP nixos_current_system_info The closure this host is running, by store path"
        echo "# TYPE nixos_current_system_info gauge"
        echo "nixos_current_system_info{store_path=\"''${running}\"} 1"

        # THE HEARTBEAT, and it is the reason this publisher has a timer at all rather than only
        # running on activation. A textfile is NOT a heartbeat: its last good numbers go on being
        # scraped and reported as current for ever, so a publisher that quietly stopped is
        # indistinguishable from a host with nothing pending. Everything above changes only on a
        # deploy; this changes every five minutes and is what an alert keys staleness on.
        echo "# HELP fleet_auto_apply_state_last_run_timestamp_seconds When this publisher last ran."
        echo "# TYPE fleet_auto_apply_state_last_run_timestamp_seconds gauge"
        echo "fleet_auto_apply_state_last_run_timestamp_seconds $(date +%s)"
      } > "''${tmp}"

      # Renamed into place, never written in place: node_exporter reads this directory whenever it
      # likes, and a truncated final line does not make it skip one series -- it makes the whole
      # file fail to parse and drops all of them.
      mv "''${tmp}" "''${metrics_file}"
      chmod 0644 "''${metrics_file}"
    '';
  };
in
{
  options.fleet.observability.textfileDirectory = lib.mkOption {
    type = lib.types.str;
    default = "/etc/node-exporter/textfile.d";
    description = ''
      Where every publisher on this host writes its `.prom` file, and where node_exporter's
      textfile collector reads them from.

      AN OPTION SO THAT OTHER MODULES CAN READ IT rather than restate it -- ./nix-gc.nix and
      ./auto-apply.nix both write here, and a second copy of this path is one rename away from a
      publisher writing somewhere nothing scrapes. That failure produces no error anywhere: the
      file is written, the unit succeeds, and the metric simply never appears.

      The value is bitcashier's, kept rather than chosen afresh, so that an operator who knows one
      fleet knows the other. It is NOT either of the two paths upstream documentation suggests.
    '';
  };

  config = {
    # ---------------------------------------------------------------------------------------------
    # The textfile directory every publisher on this host writes into
    # ---------------------------------------------------------------------------------------------
    #
    # Created UNCONDITIONALLY, and every publisher below runs unconditionally too, even though
    # node_exporter is the only reader. That is deliberate: the publishers cost nothing, and on the
    # day the exporter is what has broken, the `.prom` files are still there to be read over ssh.
    systemd.tmpfiles.rules = [
      "d ${textfileDir} 0755 root root -"
    ];

    # ---------------------------------------------------------------------------------------------
    # The fleet inventory gauge, and which commit this host RUNS
    # ---------------------------------------------------------------------------------------------
    #
    # BAKED INTO THE CLOSURE rather than written by a timer, and that is the point: the file IS part
    # of the system it describes, so it cannot report a revision the machine is not running. A
    # publisher that regenerated this would answer for whatever tree it last read.
    #
    # An info-style gauge (the shape of `node_uname_info`): the value is always 1 and the fact is in
    # the label, so `nixos_configuration_revision_info` joined against the revision main is on says
    # which hosts are behind.
    #
    # `role` and `env` are literals here because there is no fact source to supply them. They must
    # agree with the labels infra/terraform/server.*.tf puts on the same server, or this fleet's
    # inventory says one thing and Hetzner says another -- and the Hetzner labels are what an
    # `hcloud` sweep or the ansible inventory selects on.
    environment.etc."node-exporter/textfile.d/machine.prom".text =
      let
        revision =
          if config.system.configurationRevision != null
          then config.system.configurationRevision
          else "unknown-no-revision-recorded";
      in
      ''
        # HELP machine Server liveness
        # TYPE machine gauge
        machine{role="${config.fleet.role}",env="${config.fleet.environment}",hostname="${config.networking.hostName}"} 1
        # HELP nixos_configuration_revision_info The git commit this host's RUNNING configuration was built from
        # TYPE nixos_configuration_revision_info gauge
        nixos_configuration_revision_info{revision="${revision}",role="${config.fleet.role}",env="${config.fleet.environment}",hostname="${config.networking.hostName}"} 1
      '';

    # ---------------------------------------------------------------------------------------------
    # Does this host owe a reboot?
    # ---------------------------------------------------------------------------------------------
    #
    # THE ONLY METRIC IN THIS FILE THAT IS EASY TO GET WRONG, and the wrong version is the obvious
    # one. `/run/booted-system != /run/current-system` is NOT this question: a switch replaces the
    # closure while the machine keeps running the kernel it booted, so those two paths differ after
    # EVERY switch. Measured on bitcashier on 2026-08-25, seven hosts looked like they owed a reboot
    # on that test and only four actually did.
    #
    # What decides it is the BOOT CHAIN -- kernel, initrd, kernel-modules, systemd. Anything else
    # differing is ordinary post-switch residue and needs nothing. This is the same four-component
    # comparison ../../files/nixos-auto-apply.py makes before it refuses to activate, and the two
    # must stay in step: this one is what an operator sees, that one is what actually blocks.
    #
    # WHY AN ACTIVATION SCRIPT AND NOT ANOTHER `environment.etc` ENTRY, which is where the revision
    # above lives: a closure's store path is a hash of its own contents, so nothing INSIDE it can
    # name it without defining itself in terms of itself. A revision is an input and can be a
    # literal; a store path is an output and cannot.
    #
    # `$systemConfig` RATHER THAN /run/current-system, deliberately. The activation script is handed
    # the toplevel being activated, so this records the closure this activation IS -- with no
    # dependence on whether `switch-to-configuration` has repointed /run/current-system by the time
    # this runs, which is an ordering detail that would otherwise decide whether the metric is right
    # or one generation stale.
    system.activationScripts.closureMetrics = {
      deps = [ "etc" ];
      text = ''
        closureMetricsFile=${textfileDir}/nixos-closure.prom
        if [ -d ${textfileDir} ]; then
          running=$(basename "$systemConfig")
          booted=$(basename "$(readlink -f /run/booted-system 2>/dev/null || echo unknown)")

          # Missing on either side counts as DIFFERING: not being able to read one is not evidence
          # they match, and the safe direction for "should somebody reboot this" is yes.
          rebootRequired=0
          for component in kernel initrd kernel-modules systemd; do
            bootedComponent=$(readlink -f "/run/booted-system/$component" 2>/dev/null || echo missing-booted)
            candidateComponent=$(readlink -f "$systemConfig/$component" 2>/dev/null || echo missing-candidate)
            if [ "$bootedComponent" != "$candidateComponent" ]; then rebootRequired=1; fi
          done

          cat > "$closureMetricsFile.tmp" <<METRICS
        # HELP nixos_closure_info The system closure this host is running, as its store path basename
        # TYPE nixos_closure_info gauge
        nixos_closure_info{closure="$running",hostname="${config.networking.hostName}"} 1
        # HELP nixos_booted_closure_info The system closure this host actually BOOTED, which after any switch is normally older
        # TYPE nixos_booted_closure_info gauge
        nixos_booted_closure_info{closure="$booted",hostname="${config.networking.hostName}"} 1
        # HELP nixos_reboot_required 1 when the BOOT CHAIN differs from what is booted -- kernel, initrd, kernel-modules or systemd. Not merely that the closure path changed.
        # TYPE nixos_reboot_required gauge
        nixos_reboot_required{hostname="${config.networking.hostName}"} $rebootRequired
        METRICS
          mv "$closureMetricsFile.tmp" "$closureMetricsFile"
        fi
      '';
    };

    # ---------------------------------------------------------------------------------------------
    # node_exporter, BOUND TO THE PRIVATE ADDRESS AND NOWHERE ELSE
    # ---------------------------------------------------------------------------------------------
    #
    # THE BIND ADDRESS IS THE ACCESS CONTROL, not the firewall rule. node_exporter has no
    # authentication of any kind and publishes a full inventory of the machine -- filesystems,
    # network addresses, running units, every textfile above. ./firewall.nix opens 9100 on ens10,
    # and Hetzner's edge firewall never names it, but the thing that makes it unreachable from the
    # internet even if BOTH of those were wrong is that there is no listener on the public address.
    #
    # A wildcard bind here would be a defect that nothing on this fleet would report: the exporter
    # would work, Prometheus would scrape it, and the extra listener is visible only to somebody
    # running `ss -ltn` on the host.
    services.prometheus.exporters.node = {
      enable = true;
      listenAddress = config.fleet.privateAddress;
      port = config.fleet.nodeExporterPort;
      enabledCollectors = [ "systemd" "textfile" ];
      extraFlags = [
        "--collector.textfile.directory=${textfileDir}"
        # `mount` units churn a new series per container volume, and `scope` units are left behind
        # by dead ssh logins; the rest of these cannot fail at all. Excluding them keeps the series
        # count bounded on the host that will one day run k3s workloads.
        "--collector.systemd.unit-exclude=.+[.](automount|device|mount|scope|slice)"
      ];
    };

    # IT BINDS ONE ADDRESS, SO IT HAS TO WAIT FOR THAT ADDRESS. node_exporter EXITS rather than
    # retrying if the address is not configured yet:
    #
    #     listen tcp 10.20.0.10:9100: bind: cannot assign requested address
    #
    # `network-online.target` is what "the addresses are configured" is spelled as, and `wants` is
    # required as well as `after` -- the target is inert unless something pulls it in. The private
    # NIC here comes up over DHCP on a second interface, which is later than the public one, so
    # this race is not theoretical.
    #
    # AND THE START LIMIT IS THE HALF THAT MAKES IT PERMANENT. On bitcashier the default five
    # restarts inside ten seconds were spent in under two, all of them losing the same race, and
    # systemd then gave up for good: `start-limit-hit`, and no metrics from that host until a
    # person noticed and ran `systemctl reset-failed`. A MONITORING AGENT THAT DISABLES ITSELF is
    # the worst shape this can fail in, because the thing that would have reported the gap is the
    # thing that is gone. Ten attempts at five-second intervals is a minute of patience for an
    # interface, and a unit that cannot start for a real reason still ends up failed -- just after
    # a minute rather than two seconds.
    #
    # GOMAXPROCS=1 is carried over from that fleet and is not a tuning preference: see
    # prometheus/node_exporter#2500, which is why the pin exists at all.
    systemd.services.prometheus-node-exporter = {
      environment.GOMAXPROCS = "1";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig.RestartSec = 5;
      startLimitIntervalSec = 120;
      startLimitBurst = 10;
    };

    # ---------------------------------------------------------------------------------------------
    # THE AUTO-APPLY STATE, published independently of the applier
    # ---------------------------------------------------------------------------------------------
    #
    # WHY THIS IS NOT ANSWERABLE FROM ANYTHING ELSE ALREADY PUBLISHED. `nixos_auto_apply_*` is
    # written BY ./auto-apply.nix's own pass, so it says nothing at all when that pass is
    # what has stopped -- and worse, a node_exporter textfile is not a heartbeat, so the last
    # values it wrote go on being scraped as current for ever. The alert rules that matter are the
    # ones about the applier being dead, and an applier cannot be the source of evidence that it is
    # alive.
    #
    # `nixos_configuration_revision_info` above cannot answer it either: it is baked into the
    # closure, so it describes what the machine RUNS and can say nothing about what is waiting for
    # it. And a fleet-wide "are all hosts on the same revision" check has its hole exactly where it
    # matters -- if NOBODY activates, all three hosts sit on the same old revision, the fleet looks
    # perfectly consistent, and a pipeline staging into a void for a fortnight is invisible.
    #
    # WHY A TIMER RATHER THAN A FILE IN THE CLOSURE. The staged pin changes AFTER the closure is
    # built -- that is what staging is -- so a value baked in at build time would describe the
    # deploy before last. This has to be read at scrape time.
    #
    # UNGATED, deliberately: it runs on a host with `fleet.autoApply.enable = false` too, and that
    # is the host it is most useful on. Being exempt from activating is not being exempt from being
    # counted.
    systemd.services.fleet-auto-apply-state = {
      description = "Publish whether a staged closure is waiting to be activated on this host";
      # ALSO ON ACTIVATION, not only on the timer, so the reading is right IMMEDIATELY after a
      # deploy rather than up to five minutes later -- the window in which an operator checking
      # whether their change is live would be told, correctly and uselessly, about the state before
      # it. `wantedBy = multi-user.target` on a oneshot that does NOT set `RemainAfterExit` is what
      # buys that: every active target is restarted by a switch, which pulls in a wanted unit that
      # is inactive, and a finished oneshot is inactive.
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        # Root only because the textfile directory is not writable by anyone else. Resolving two
        # symlinks and running `nixos-version` needs no privilege at all.
        User = "root";
        TimeoutStartSec = "1min";
        Environment = [
          "METRICS_FILE=${textfileDir}/nixos-staged.prom"
          "STAGED_SYSTEM=${config.fleet.autoApply.stagedSystem}"
        ];
        ExecStart = lib.getExe statePublisher;
      };
    };

    systemd.timers.fleet-auto-apply-state = {
      description = "Publish whether a staged closure is waiting to be activated on this host";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "2min";
        # FIVE MINUTES, which is about the staleness an alert on this can meaningfully assert. It
        # is not measuring anything expensive -- two readlinks and one `nixos-version` -- and the
        # value it publishes changes only when CI stages or the applier switches, so the interval
        # is chosen for the heartbeat rather than for the measurement.
        OnUnitActiveSec = "5min";
        AccuracySec = "1min";
      };
    };
  };
}
