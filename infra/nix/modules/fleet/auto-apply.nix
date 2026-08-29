# Activating what CI has already staged, when activating it would disturb nothing.
#
# PORTED FAITHFULLY from bitcashier's `nix/modules/fleet/auto-apply.nix`. Every number, every unit
# ordering and every option below is the one that fleet arrived at by measurement, and the
# temptation when porting a thing like this is to keep the mechanism and re-derive the parameters
# from first principles -- which loses exactly the knowledge that made it safe. Where a default has
# been changed for this fleet it says so and says why; where it has not, it has not.
#
# WHERE THIS SITS. ./deploy-staging.nix lets CI put a signed closure on this host and pin it at
# `/var/lib/nixdeploy/staged-system`, and deliberately lets it do nothing else -- the key is
# restricted to a forced command, `nixdeploy` has no sudo, and nothing activates. That stop is
# correct: activating is the step that can break a running machine, and a build server should not
# be the thing that decides to take it.
#
# This module takes that step for the changes where there is nothing to decide. If the staged
# closure would stop, start, restart or reload NOTHING, it is activated. Anything else -- including
# anything the classifier cannot categorise -- is left pinned and reported as owed.
# ../../files/nixos-auto-apply.py holds the whole classification and its header is the argument.
#
# WHY A THREE-HOST FLEET WANTS THIS AT ALL, since the obvious objection is that three machines are
# few enough to deploy by hand. They are. The problem is not the deploy, it is the ACCUMULATION:
# the routine commits -- a package bump, an ssh key, a comment in a script -- are the ones nobody
# schedules a deploy for, and they pile up until a real deploy carries six months of unrelated
# change with it and nobody can say which part of it broke the host. Applying the inert ones
# continuously means the only pending change is ever the one somebody meant to make.
#
# IT PUBLISHES WHETHER OR NOT IT APPLIES. `dryRun = true` classifies and publishes without ever
# switching, which is how a host is staged into this. ./observability.nix additionally publishes
# `nixos_staged_pending` from OUTSIDE this module, which is the series that still says something on
# the day the applier itself is what has stopped.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.autoApply;

  # PYTHON RATHER THAN SHELL, and the only place in nix/files where that is true. The classifier
  # matches whole lines against a table, refuses the ones it does not know, and must still publish
  # a metric when it fails -- and a shell script that gets any of that subtly wrong fails in the
  # direction that ACTIVATES rather than the direction that refuses.
  #
  # writePython3Bin runs flake8 at build time, so a typo in a rarely-taken error path is a build
  # failure here rather than a surprise at 04:00 on a host. E501 is waived because this
  # repository's comment style is wider than 79 columns everywhere.
  applier = pkgs.writers.writePython3Bin "nixos-auto-apply"
    {
      libraries = [ ];
      flakeIgnore = [ "E501" "E722" "W503" "W504" ];
    }
    # THE SHEBANG IS DROPPED, and the reason only appears at BUILD time. writePython3Bin prepends
    # its own interpreter line, so the file's own `#!/usr/bin/env python3` lands on line 2 -- where
    # it is no longer a shebang but a block comment, and flake8 fails the build with `E265 block
    # comment should start with '# '`. Linting the source directly never sees it, because there the
    # shebang is line 1 and flake8 skips it.
    #
    # It cost bitcashier a fleet-wide staging run to find: `nix eval` of a drvPath does not run the
    # builder, so every configuration evaluating cleanly said nothing about whether one of them
    # would build. The shebang stays in the source file so it remains runnable and lintable on its
    # own.
    (lib.concatStringsSep "\n"
      (lib.drop 1 (lib.splitString "\n" (builtins.readFile ../../files/nixos-auto-apply.py))));
in
{
  options.fleet.autoApply = {
    enable = lib.mkEnableOption ''
      activating the closure CI staged here, on a timer, when doing so would disturb nothing.

      ON BY DEFAULT: ./default.nix sets it with `mkDefault`, so every host that imports the fleet
      module is covered without being named in a list. This option's own default of `false` is what
      a host overrides to OPT OUT, which is the direction that needs a written reason.

      That reversal is safe because of what the gate PERMITS, not because the risk went away: with
      `reloadableUnits` and `restartableUnits` both empty -- their defaults, and this fleet's
      setting -- a switch that would stop, start, restart or reload a single unit is refused and
      left for a person. What is left is changing files, and the residual risk in that is
      activation scripts, the one part of a switch no pre-check can predict. `dryRun = true` is how
      to watch a host classify real merges without ever switching
    '';

    excludedBecause = lib.mkOption {
      type = lib.types.str;
      default = "";
      example = "mongo-1 must not activate anything unattended during the Fly cutover";
      description = ''
        Why this host is DELIBERATELY not auto-applied. Set it, with a reason, on a host that is
        meant to be left out.

        IT EXISTS BECAUSE `enable = false` AND "NOBODY HAS WIRED THIS UP YET" ARE THE SAME THING TO
        A MONITORING SYSTEM, and they need opposite responses. An alert on "a NixOS host publishing
        no auto-apply metric" is right to fire: every other alert in that group is then silent
        about the host, which is the absence of evidence rather than evidence it is fine. But a
        host excluded ON PURPOSE is not an oversight, and leaving it firing for ever teaches people
        to ignore the alert that catches the real ones.

        A non-empty reason publishes `nixos_auto_apply_excluded`, which such an alert accepts in
        place of a timestamp. An EMPTY reason publishes nothing, so a host nobody has configured
        still alerts -- silence has to stay the default, or this option becomes a way to turn the
        alert off by accident.

        The reason is carried as a LABEL because it is read by whoever is wondering why a host is
        not covered, and "somebody decided this" is only useful with the decision attached.
      '';
    };

    attribute = lib.mkOption {
      type = lib.types.str;
      default = "";
      example = "mongo-1";
      description = ''
        The host's attribute in `nixosConfigurations`, used as the `host` label on every metric so
        that a reading can be joined to the configuration it is about.

        STATED RATHER THAN DERIVED FROM `networking.hostName`, because the two are not reliably the
        same string -- a machine bootstrapped from a Hetzner image answers to whatever that image
        called it until the activation script in ./default.nix has run at least once, and a metric
        labelled with a hostname that changed under it is worse than no metric.

        EMPTY BY DEFAULT SO THE ASSERTION BELOW IS WHAT FAILS. A new host that forgets this would
        otherwise be refused by the module system with "The option `fleet.autoApply.attribute' was
        accessed but has no value defined", which names no host, no fix and no reason. With a
        default the build still fails -- it must -- but it fails saying what to write.
      '';
    };

    stagedSystem = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/nixdeploy/staged-system";
      description = ''
        The pin CI writes. Read from here rather than from `fleet.deployStaging` so that the two
        modules stay separable -- staging is useful without this one, and this one must be able to
        say "nothing has ever been staged here" rather than assume a path exists.

        ./observability.nix reads this same option for its own publisher, which is the reason it is
        an option at all rather than a literal in two files.
      '';
    };

    interval = lib.mkOption {
      type = lib.types.str;
      default = "30min";
      description = ''
        How often to look WHEN NOTHING HAS PUSHED, set to 30 minutes deliberately rather than to
        anything near the push latency. The primary trigger is a PATH UNIT on the pin directory, so
        a merge reaches a host in about a second and this timer is a BACKSTOP -- for an inotify
        event missed while the host was down, a pin written before the watcher started, or a pass
        that failed and wants retrying.

        IT IS DELIBERATELY NOT SHORTENED TOWARDS THE PUSH LATENCY, and the numbers are why. A pass
        is cheap but not free: measured on the fleet this was ported from, 6.9s wall and 1.17s CPU
        end to end, of which `switch-to-configuration dry-activate` is 0.35s -- and that dry run
        really does execute sops-install-secrets, so it touches the host's age key every time. At
        one minute that is an 11% duty cycle and ~1,440 sops runs per host per day, buying nothing
        the path unit has not already delivered. Latency is the push's job; this is only insurance
        against the push being missed.
      '';
    };

    reloadableUnits = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "prometheus-node-exporter.service" ];
      description = ''
        Units a reload may touch without the change counting as disruptive. DEFAULT DENY, and
        deliberately per unit rather than a blanket "reloads are fine": a reload leaves the process
        running, but `sshd` reloading a configuration that refuses every key is indistinguishable
        from a healthy reload until somebody tries to log in.

        EMPTY ON THIS FLEET -- see ./default.nix, which explains why the `[ "*" ]` bitcashier
        arrived at is not the starting position here and what evidence should move it.
      '';
    };

    restartableUnits = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "grafana.service" ];
      description = ''
        Units this host accepts being STOPPED, STARTED or RESTARTED by an unattended switch. Glob
        patterns, matched case-sensitively against the unit name (`fnmatch`), so a pattern covers a
        family whose membership changes without this list changing with it.

        DEFAULT DENY, and the default is this fleet's position: without an entry here a switch that
        would disturb ANY unit is refused and left for a person, which is what this module was
        built to do. An entry is a host saying "this particular service being bounced at an
        arbitrary moment is a cost I accept", and it is worth writing that sentence out for each
        one before adding it.

        THE COST IS NOT ONLY THE UNIT NAMED. A pattern forgives every FUTURE closure that touches a
        matching unit, not just the change in front of you -- so forgiving `grafana.service` also
        forgives a nixpkgs bump that restarts it in the middle of somebody reading a dashboard
        during an incident. That is a fine trade for Grafana and a terrible one for mongod, and the
        difference is not visible in the syntax.

        A path whose unit cannot be identified is never forgiven -- creating a `.wants` directory,
        say -- so this cannot be used to wave through structural changes nobody has read.
      '';
    };

    neverDisturbUnits = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "sshd.service" ];
      description = ''
        Units NO allow-list may forgive. Glob patterns, matched exactly as the two allow-lists are,
        and checked BEFORE either of them -- so a unit named here is refused however `*` the rest of
        the configuration is.

        WHY A THIRD LIST RATHER THAN A CAREFULLY WRITTEN ALLOW-LIST. Both allow-lists are globs, and
        the useful value for each turns out to be `*`, because anything narrower goes inert the
        moment a nixpkgs bump touches half the machine. `*` with nothing to carve out of it is an
        all-or-nothing switch, and it forgives every FUTURE closure as well as the one in front of
        you. This is what makes widening survivable later.

        This is the one list whose failure mode is REFUSING a switch somebody wanted, which is the
        direction this module errs in everywhere else. Add to it freely. ./default.nix sets the
        fleet floor as a plain list rather than an `mkDefault`, so a host's own entries MERGE with
        it and lowering the floor takes `lib.mkForce`.
      '';
    };

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Classify and publish, never switch. THIS IS HOW A HOST IS STAGED INTO AUTO-APPLY: turn it
        on, watch `nixos_auto_apply_info` across a week of real merges, and only then turn it off.

        On this fleet that week is worth taking on mongo-1 before anywhere else, because it is the
        one host where an unexpected activation costs data availability rather than a graph.
      '';
    };

    rollbackOnFailure = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Roll back when a unit that was healthy before the switch is failing after it. Cheap here in
        a way it is not in general, because the gate has already established that this switch
        restarts nothing, so the only things to undo are files.
      '';
    };
  };

  config = lib.mkMerge [

    # A DISABLED APPLIER MUST TAKE ITS METRIC WITH IT.
    #
    # Everything below is behind `mkIf cfg.enable`, so turning auto-apply off stops the timer and
    # leaves nixos-auto-apply.prom sitting in the textfile directory, FROZEN at the last pass. A
    # node_exporter textfile is not a heartbeat: it keeps being scraped and keeps being reported as
    # current, so a staleness alert on `nixos_auto_apply_last_attempt_timestamp_seconds` fires
    # forever -- and NOTHING can ever resolve it, because the only thing that would advance that
    # timestamp is the applier that was deliberately turned off.
    #
    # That is not hypothetical. bitcashier had a host alert continuously for two days on a metric
    # still labelled with the role it had been repurposed away from. An alert nobody can clear is
    # how a channel gets muted, which costs the alerts that mean something.
    #
    # THIS BRANCH IS REACHED BY NO HOST TODAY, and it stays regardless: it is what makes turning one
    # OFF later a clean operation rather than one that leaves a frozen metric alerting for ever. A
    # cleanup path that only runs on the day somebody needs it must already be there on that day.
    #
    # `r` rather than deleting it in an activation script: tmpfiles rules run on switch and on boot,
    # need no unit of their own, and are a no-op when the file is already gone.
    (lib.mkIf (!cfg.enable) {
      systemd.tmpfiles.rules = [
        "r ${config.fleet.observability.textfileDirectory}/nixos-auto-apply.prom"
      ];

      # ONLY WHEN A REASON WAS GIVEN. Publishing this unconditionally would silence the
      # not-covered alert on every host that has never heard of the module, which is precisely the
      # case it exists to catch.
      environment.etc."node-exporter/textfile.d/nixos-auto-apply-excluded.prom" =
        lib.mkIf (cfg.excludedBecause != "") {
          text = ''
            # HELP nixos_auto_apply_excluded This host is deliberately not auto-applied, and why.
            # TYPE nixos_auto_apply_excluded gauge
            nixos_auto_apply_excluded{reason="${cfg.excludedBecause}"} 1
          '';
        };
    })

    (lib.mkIf cfg.enable {
      assertions = [
        {
          assertion = cfg.attribute != "";
          message = "fleet.autoApply.attribute must name this host's entry in nixosConfigurations.";
        }
        {
          # WITHOUT STAGING THERE IS NO PIN TO READ, and every pass would report "nothing has ever
          # been staged here" for ever. Better to refuse to build than to ship a timer that can
          # only ever publish an error.
          #
          # NOTE FOR THIS TREE: `fleet.deployStaging.authorizedKey` is UNSET until the CI keypair
          # exists, so this assertion is what currently stops a host evaluating. That is
          # deliberate and it is the honest state -- a fleet whose auto-apply is on but whose
          # staging channel is unconfigured is a fleet that silently never applies anything, which
          # is the exact failure this module exists to end. Mint the pair
          # (`ssh-keygen -t ed25519 -C ci-nix-stage` and
          # `nix key generate-secret --key-name kinowo-infra-1`), set both halves in the fleet
          # module, and the assertion goes quiet.
          assertion = config.fleet.deployStaging.authorizedKey != "";
          message = ''
            fleet.autoApply needs fleet.deployStaging enabled: it activates the closure CI stages,
            and stages nothing itself. Set fleet.deployStaging.authorizedKey (and
            trustedPublicKeys) fleet-wide, or set fleet.autoApply.enable = false on this host with
            fleet.autoApply.excludedBecause saying why.
          '';
        }
      ];

      systemd.services.nixos-auto-apply = {
        description = "Activate the staged closure if doing so would disturb nothing";

        # NOT restartIfChanged. A switch that changed this unit would restart the very process
        # running it, killing the pass mid-flight and leaving the lock file's mtime lying about how
        # long a holder has held it. The gate makes that nearly unreachable -- a changed unit blocks
        # the switch -- but "nearly" is the wrong standard for the thing that performs switches, and
        # nixpkgs' own `system.autoUpgrade` sets the same flag for the same reason.
        restartIfChanged = false;

        path = with pkgs; [
          nix # nix-env, to set the system profile before activating -- see the script
          systemd
          coreutils
        ];

        serviceConfig = {
          Type = "oneshot";
          User = "root";
          # BOUNDED, and no longer generous: nothing here builds. What must not happen is an
          # unbounded hang holding the lock, which would make every later pass report "already
          # running" for ever.
          TimeoutStartSec = "30min";
          ExecStart = lib.concatStringsSep " " ([
            (lib.getExe applier)
            "--host ${lib.escapeShellArg cfg.attribute}"
            "--staged-system ${lib.escapeShellArg cfg.stagedSystem}"
            "--metrics-file ${config.fleet.observability.textfileDirectory}/nixos-auto-apply.prom"
          ]
          ++ lib.optional (cfg.reloadableUnits != [ ])
            "--reloadable ${lib.escapeShellArg (lib.concatStringsSep "," cfg.reloadableUnits)}"
          ++ lib.optional (cfg.restartableUnits != [ ])
            "--restartable ${lib.escapeShellArg (lib.concatStringsSep "," cfg.restartableUnits)}"
          ++ lib.optional (cfg.neverDisturbUnits != [ ])
            "--never-disturb ${lib.escapeShellArg (lib.concatStringsSep "," cfg.neverDisturbUnits)}"
          ++ lib.optional cfg.dryRun "--dry-run"
          ++ lib.optional (!cfg.rollbackOnFailure) "--no-rollback-on-failure");

          # 1 IS "A CHANGE IS PENDING THAT A PERSON MUST DECIDE ON", which is an ordinary state for
          # a host to be in and must not paint the unit failed -- failed units are alarmed on
          # fleet-wide, and burying a real fault under a routine one is how an alarm stops being
          # read. 2 (nothing could be determined) IS a failure of this unit -- EXCEPT 3, which is
          # the one undetermined case that is not open-ended: this pass's own dry-activate lost the
          # Nix lock to the switch that is installing or updating it. EVERY HOST HITS THAT ON ITS
          # FIRST ADOPTION of this module, and it resolves itself by the next tick. See
          # LockContended in nixos-auto-apply.py.
          SuccessExitStatus = [ 0 1 3 ];
        };
      };

      # PUSH, NOT POLL. The pin is what changes, so the pin is what triggers -- this unit starts the
      # applier within about a second of CI writing `staged-system`, instead of up to `interval`
      # later.
      #
      # WHY A PATH UNIT IS THE RIGHT PUSH AND CI TRIGGERING IT IS NOT. The obvious alternative is to
      # let the staging job start the applier over the same ssh key, and that would spend the whole
      # security boundary this mechanism is built on: nix-stage-endpoint.sh is a forced command that
      # can ONLY receive and pin a signed closure, `nixdeploy` has no sudo, and nothing CI can say
      # activates anything. A path unit changes none of that. The trigger is a LOCAL consequence of
      # the pin already being written, so the trust boundary is exactly where it was and CI gains no
      # new verb.
      #
      # WATCHING THE DIRECTORY RATHER THAN THE SYMLINK, because the symlink is REPLACED rather than
      # written: `ln -sfn` in the endpoint unlinks and recreates it, which is a change to the
      # directory entry and not to the link. /var/lib/nixdeploy contains `staged-system` and nothing
      # else -- no temp files, no partial writes -- so this cannot self-trigger, and a copy that
      # never reaches the pin step produces no event at all.
      #
      # HARMLESS IF IT FIRES SPURIOUSLY. The applier holds /run/nixos-auto-apply.lock and reports
      # "already running" rather than overlapping, and a pass against an unchanged pin is
      # `up_to_date` -- one symlink read and a directory comparison.
      systemd.paths.nixos-auto-apply = {
        description = "Activate as soon as CI pins a closure, rather than at the next poll";
        wantedBy = [ "paths.target" ];
        pathConfig = {
          PathChanged = builtins.dirOf cfg.stagedSystem;
          Unit = "nixos-auto-apply.service";
        };
      };

      # THE BACKSTOP. Silence is not success: a push that is missed leaves a host silently behind,
      # so the timer stays even though it is no longer how a merge normally arrives.
      systemd.timers.nixos-auto-apply = {
        description = "Backstop poll, in case the pin change was never seen";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          # NOT OnBootSec. That fires relative to actual boot time, and on a host that has already
          # been up for a while when this module is FIRST adopted, "boot + 5min" is already in the
          # past the instant the timer unit is created -- so systemd fires it essentially
          # immediately, which is exactly the pass that races the `nixos-rebuild switch` still
          # installing it and holding the Nix lock it needs. `OnActiveSec` instead counts from when
          # THIS unit was last activated, which is always in the future relative to itself: on first
          # install it is "5 minutes from now", and on a genuine reboot it is "5 minutes after
          # timers.target is reached", which is early in boot and close enough to the OnBootSec
          # behaviour it replaces. A later switch that leaves this unit's definition unchanged does
          # not restart it and so cannot re-arm this race either.
          OnActiveSec = "5min";
          OnUnitActiveSec = cfg.interval;
          # Hosts waking together would activate in lockstep. Nothing here is disruptive by
          # construction, but a fleet-wide simultaneous switch is still a fleet-wide simultaneous
          # change, and staggering costs nothing.
          RandomizedDelaySec = "5min";
          # Deliberately NOT Persistent: a host that was off does not need to catch up the instant
          # it boots -- the path unit fires on the next pin, and OnActiveSec covers the rest.
          Persistent = false;
          AccuracySec = "1min";
        };
      };
    })
  ];
}
