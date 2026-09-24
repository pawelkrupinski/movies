# THE SITE, FETCHED FROM OUTSIDE THE CLUSTER, THROUGH CLOUDFLARE, THE WAY A VISITOR FETCHES IT.
#
# Every other signal on this fleet is the application describing itself: request counters, served
# gauges, a pod's readiness. None of them sees a request that never reaches a pod -- an expired
# origin certificate, a Caddy vhost that stopped matching, a Cloudflare rule blocking real
# browsers, a country prefix routed to the wrong NodePort. On each of those the pods stay green and
# the counters stay quiet, because the failure is that nothing arrives to be counted.
#
# So blackbox_exporter on this host fetches the public URLs over the public internet, through
# Cloudflare's edge, and Prometheus scrapes the verdicts as the `blackbox` job. synthetic-probes.rules
# alerts on them (ProbeFailing, ProbeSlow).
#
# WHAT IS PROBED. The static list below is the doors that must always answer: each brand's front
# door, each country's root, one big city per country. A FILM PAGE AND ITS SHARE CARD cannot be
# written down -- a film's page 404s the day its last screening passes -- so a timer asks each
# country's city page which film it lists now and writes those as a file_sd document
# (nix/files/synthetic-probe-targets.sh says how, and why it is all-or-nothing).
#
# ⚠️ CLOUDFLARE CHALLENGES THIS HOST UNTIL IT IS ALLOW-LISTED. Bot Fight Mode is on for both zones
# and answers a Hetzner address with a 403 challenge page whatever the User-Agent (measured
# 2026-09-24 from the fleet's egress: 403 for every URL below, with curl's, blackbox's and a
# browser's UA alike), and it has no WAF skip. An IP Access Rule with action `allow` for this
# host's public address on BOTH zones is what lets the probes through; until that exists every
# probe fails and ProbeFailing says so. The rule is a hand step -- see the commit that added this.
#
# ⚠️ THE FIRST SWITCH IS BY HAND. Both units are new, and auto-apply refuses a switch that STARTS a
# unit it has not been told it may disturb -- and it reads that permission from the closure it is
# running, not the one it is judging, so the `restartableUnits` entries below only help from the
# second switch on. `AutoApplyRefusingChange` fires until somebody runs the switch.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.syntheticProbes;

  # WHERE THE DISCOVERED TARGETS LIVE. Root-owned and world-readable: Prometheus (its own user)
  # only reads it, and re-reads it on change without a restart.
  stateDir = "/var/lib/synthetic-probes";
  discoveredFile = "${stateDir}/targets.json";

  modulesYaml = (pkgs.formats.yaml { }).generate "blackbox.yaml" {
    modules =
      let
        http = extra: {
          prober = "http";
          # UNDER THE SCRAPE TIMEOUT BELOW, so a slow answer is a failed probe with a duration
          # rather than a scrape that timed out and says nothing.
          timeout = "12s";
          http = {
            # IPv4, because this host's egress is: a probe that picks AAAA and fails would be a
            # statement about the resolver, not the site.
            preferred_ip_protocol = "ip4";
            ip_protocol_fallback = false;
            follow_redirects = true;
            headers."User-Agent" = cfg.userAgent;
          } // extra;
        };
      in
      {
        # A PAGE MUST ARRIVE WHOLE. A 200 that stops short of `</html>` is a truncated render or an
        # error page served with the wrong status, and both are what a visitor would see.
        http_page = http { fail_if_body_not_matches_regexp = [ "</html>" ]; };
        # A share card is an image: any 2xx will do.
        http_asset = http { };
      };
  };

  # THE SCRAPE JOB, written like roles/prometheus.nix writes its other scrape.d files: toJSON,
  # because JSON is valid YAML and a generated document is not reviewed by eye.
  #
  # `instance` BECOMES THE URL, so an alert names the page that failed rather than 127.0.0.1:9115.
  # `__param_module` rides on each target, so one job serves both modules.
  blackboxTargetsYaml = builtins.toJSON {
    scrape_configs = [{
      job_name = "blackbox";
      metrics_path = "/probe";
      # ONE FETCH A MINUTE PER URL. Enough for a 5m hold to mean five failures in a row, and a
      # few requests a minute is nothing to the site.
      scrape_interval = "60s";
      scrape_timeout = "15s";
      static_configs = map
        (t: {
          targets = [ t.url ];
          labels = { inherit (t) country kind; __param_module = "http_page"; };
        })
        cfg.targets;
      file_sd_configs = [{ files = [ discoveredFile ]; refresh_interval = "1m"; }];
      relabel_configs = [
        { source_labels = [ "__address__" ]; target_label = "__param_target"; }
        { source_labels = [ "__param_target" ]; target_label = "instance"; }
        { target_label = "__address__"; replacement = "127.0.0.1:${toString cfg.exporterPort}"; }
      ];
    }];
  };

  discover = pkgs.writeShellScript "synthetic-probe-targets"
    (builtins.readFile ../../files/synthetic-probe-targets.sh);
in
{
  options.fleet.syntheticProbes = {
    enable = lib.mkEnableOption "blackbox_exporter probing the public site from this host";

    exporterPort = lib.mkOption {
      type = lib.types.port;
      default = 9115;
      description = "blackbox_exporter's port, on loopback only: Prometheus is on the same host.";
    };

    userAgent = lib.mkOption {
      type = lib.types.str;
      default = "kinowo-synthetic-probe/1 (+https://kinowo.net)";
      description = ''
        Sent on every probe and every discovery fetch, so the requests are easy to find -- and to
        exclude -- in Caddy's access logs and Cloudflare's analytics.
      '';
    };

    targets = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          url = lib.mkOption { type = lib.types.str; };
          country = lib.mkOption { type = lib.types.str; description = "`all` for a door that is no one country's."; };
          kind = lib.mkOption { type = lib.types.enum [ "front-door" "country-root" "city" ]; };
        };
      });
      default = [ ];
      description = "The pages that must always answer. Each becomes one probe.";
    };

    discoverFrom = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = { pl = "https://kinowo.net/warszawa/"; };
      description = ''
        country -> a city page whose first listed film (and that film's share card) is probed.
        Re-discovered every two minutes, because a film page stops existing when the film does.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.blackbox = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = cfg.exporterPort;
      configFile = modulesYaml;
    };

    environment.etc."prometheus/scrape.d/blackbox-targets.yaml".text = blackboxTargetsYaml;
    # A SCRAPE FILE PROMETHEUS NEVER RE-READS IS A PROBE THAT NEVER RUNS. roles/prometheus.nix
    # restarts the unit on each of its own files; this one is added to the same list.
    systemd.services.prometheus.restartTriggers =
      [ config.environment.etc."prometheus/scrape.d/blackbox-targets.yaml".text ];

    # ROOT, SO IT CAN PUBLISH INTO node_exporter's root-owned textfile directory; everything else
    # about the unit is locked down, and all it does is two curls per country.
    systemd.services.synthetic-probe-targets = {
      description = "Find a live film page and share card per country for the synthetic probes";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      path = [ pkgs.curl pkgs.jq pkgs.gnugrep pkgs.gnused pkgs.coreutils ];
      environment = {
        PROBE_USER_AGENT = cfg.userAgent;
        PROBE_TEXTFILE = "${config.fleet.observability.textfileDirectory}/synthetic-probes.prom";
      };
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs ([ "${discover}" discoveredFile ]
          ++ lib.mapAttrsToList (country: url: "${country}=${url}") cfg.discoverFrom);
        StateDirectory = "synthetic-probes";
        StateDirectoryMode = "0755";
        TimeoutStartSec = "5min";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ config.fleet.observability.textfileDirectory ];
      };
    };

    # EVERY TWO MINUTES, because a film leaving the schedule 404s its page at once and ProbeFailing
    # holds for five: two minutes to be replaced (file_sd picks the new file up on write) leaves
    # three of margin. Ten requests per run is nothing to the site.
    systemd.timers.synthetic-probe-targets = {
      description = "Find a live film page and share card per country for the synthetic probes";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "2min";
      };
    };

    # CHEAP TO BOUNCE, AND NAMED SO A CHANGE TO THEM DOES NOT HOLD BACK EVERY OTHER MERGE STAGED
    # FOR THIS HOST. A blackbox restart is one missed probe; the discovery is a oneshot.
    fleet.autoApply.restartableUnits = [
      "prometheus-blackbox-exporter.service"
      "synthetic-probe-targets.service"
      "synthetic-probe-targets.timer"
    ];
  };
}
