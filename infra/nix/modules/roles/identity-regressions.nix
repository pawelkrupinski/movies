# PRODUCTION MATCH REGRESSIONS, FROM THE TAKE-CHANGE LOG: one film, one build, one flip-flopping listing.
#
# Every five minutes `identity-regressions publish` reads the last 24h of `identity take change` lines
# from this host's VictoriaLogs and writes per-film / per-build / per-film-pair gauges into
# node_exporter's textfile directory; rules/identity-regressions.rules alerts on them. The script, the
# definitions and the backtest are ../../files/identity-regressions.py; infra/test/test_identity_regressions.py
# pins its arithmetic and keeps its THRESHOLDS equal to the rule file's.
#
# The gauges it writes (the `# TYPE` lines the worker's GrafanaMetricCoverageSpec reads for fleet families):
#   # TYPE kinowo_identity_film_net_lost gauge
#   # TYPE kinowo_identity_build_net_lost gauge
#   # TYPE kinowo_identity_listing_flips gauge
#   # TYPE kinowo_identity_cures gauge
#   # TYPE kinowo_identity_regressions_lines gauge
#   # TYPE kinowo_identity_regressions_last_success_timestamp_seconds gauge
#
# ON FAILURE THE FILE IS LEFT AS IT WAS and its timestamp ages: IdentityRegressionWatchStale fires at 30
# minutes. Rewriting it empty would resolve every firing alert on a VictoriaLogs hiccup.
#
# ⚠️ THE FIRST SWITCH IS BY HAND: both units are new, and auto-apply refuses a switch that starts a unit
# the RUNNING closure has not listed in `restartableUnits` (see roles/synthetic-probes.nix).
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.identityRegressions;
  textfileDir = config.fleet.observability.textfileDirectory;

  # Also on PATH, so the backtest runs on the host that can reach the store:
  #   identity-regressions backtest --url http://10.20.0.11:9428 --days 14
  script = pkgs.writers.writePython3Bin "identity-regressions" {
    # The file follows the repository's line length, not flake8's default; and it keeps its own
    # `#!/usr/bin/env python3` (it runs standalone for a local backtest), which lands on line 2 here.
    flakeIgnore = [ "E501" "E265" ];
  } (builtins.readFile ../../files/identity-regressions.py);
in
{
  options.fleet.identityRegressions = {
    enable = lib.mkEnableOption "the identity regression watch (take-change log -> textfile gauges)";
    victoriaLogsUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://${config.fleet.victoriaLogs.listenAddress}:${toString config.fleet.victoriaLogs.port}";
      defaultText = "http://<fleet.victoriaLogs.listenAddress>:<port>";
      description = "Where the take-change lines are read from.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ script ];

    # ROOT, SO IT CAN PUBLISH INTO node_exporter's root-owned textfile directory; otherwise locked
    # down -- all it does is one HTTP query and one file rename.
    systemd.services.identity-regressions = {
      description = "Publish identity match-regression gauges from the take-change log";
      after = [ "victoria-logs.service" "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs [
          "${script}/bin/identity-regressions" "publish"
          "--url" cfg.victoriaLogsUrl
          "--out" "${textfileDir}/identity-regressions.prom"
          # A LOSS OF A TAKE JUDGED WRONG IS THE CURE, NOT A REGRESSION: DE "Iconic Movie" (31 listings) and
          # "Spieleabend" paged as losses on the first run, 2026-10-22. The file is copied into the closure, so
          # a judgement edit reaches the watch with the next staged closure (an existing oneshot: auto-apply).
          "--judgements" "${../../../../test/resources/fixtures/identity-judgements.tsv}"
        ];
        TimeoutStartSec = "4min";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ textfileDir ];
      };
    };

    systemd.timers.identity-regressions = {
      description = "Publish identity match-regression gauges from the take-change log";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "2min";
        OnUnitActiveSec = "5min";
      };
    };

    fleet.autoApply.restartableUnits = [
      "identity-regressions.service"
      "identity-regressions.timer"
    ];
  };
}
