# Grafana, provisioned as code -- the port of the `kinowo-grafana` Fly app.
#
# ------------------------------------------------------------------------------------------------
# WHERE THE PROVISIONING COMES FROM, AND WHY MOST OF IT IS NOT COPIED
# ------------------------------------------------------------------------------------------------
#
# The alert rules and the dashboards are read STRAIGHT OUT of `fly/grafana/provisioning/` -- the
# same files the Fly machine serves today, in the same repository as this one. Not vendored, not
# transcribed. That is the bitcashier practice inverted: over there the Grafana content lives in a
# Puppet tree and is read from it so that one copy serves two fleets; here it lives in the
# application repository and is read from it so that one copy serves the Fly instance and this one
# through the whole of the migration, however long that takes.
#
# The alternative was to copy 1,279 lines of alert rules and 3,569 lines of dashboard JSON into
# nix/files/. A transcription error in an alert rule is INVISIBLE -- a threshold that is wrong does
# not fail, it stays quiet -- and two copies of an alert file drift the first time somebody fixes a
# rule on the Fly side during an incident. Read it once.
#
# THESE PATHS REACH OUT OF infra/ INTO fly/, WHICH REQUIRES THE FLAKE ROOT TO BE THE REPOSITORY
# ROOT rather than `infra/`. That is exactly what bitcashier's flake.nix says about its own root and
# for exactly this reason. IF THE FLAKE IS ROOTED AT infra/ THESE PATHS DO NOT EXIST IN THE CLOSURE
# AND THE BUILD FAILS -- loudly, at evaluation, naming the path. The fix is to move the flake root,
# not to copy the files across; copying is how the drift starts.
#
# WHAT IS VENDORED, AND ONLY THIS: the contact points and notification policy
# (nix/files/monitoring/grafana-contactpoints.yaml). They had to change -- the Fly copy carries the
# worker-throttle webhooks and a different chat id -- so they are a separate file with its own
# reasoning written into it.
#
# ------------------------------------------------------------------------------------------------
# THE TWO GOTCHAS THAT MUST SURVIVE THIS PORT. BOTH ARE VERIFIED, BOTH COST A DEBUGGING SESSION.
# ------------------------------------------------------------------------------------------------
#
# (a) THE FLY DATASOURCE'S AUTH HEADER IS `FlyV1 <token>`, NOT `Bearer <token>`. A Bearer header
#     401s with "resolving organization" -- which reads like a revoked or wrong token, so the first
#     instinct is to issue a new one, and a new token behaves identically. See the header of
#     roles/prometheus.nix, which carries the same value in its scrape config for the same reason.
#     The token itself is a read-only org token: `fly tokens create readonly --org personal`.
#
# (b) THE TELEGRAM `chatid` MUST BE A QUOTED LITERAL, NEVER `${VAR}`. Grafana expands environment
#     variables in provisioning files, and that expansion RE-TYPES a numeric chatid as a YAML
#     number regardless of how it was quoted -- so `chatid: '${TELEGRAM_CHAT_ID}'` produces a
#     number where Grafana's schema wants a string, it fails to unmarshal ("cannot unmarshal number
#     into ... string"), and it EXITS BEFORE EVER BECOMING HEALTHY. Not a warning, not a broken
#     contact point on an otherwise-running instance: no Grafana at all, which on this box means no
#     alerting at all. The chat id is not a secret (it is useless without the bot token), so the
#     fix is simply to write it into the vendored file as a quoted literal. It is there, with the
#     same warning, in grafana-contactpoints.yaml. KEEP THE QUOTES; do not "improve" it into a
#     variable.
#
# ------------------------------------------------------------------------------------------------
# WHAT THIS INSTANCE CANNOT DO YET, NAMED SO NOBODY THINKS IT WAS FORGOTTEN
# ------------------------------------------------------------------------------------------------
#
# The Fly instance runs a VictoriaMetrics sidecar that scrapes the apps' /metrics endpoints
# DIRECTLY over Fly 6PN every 15s, wired as the `app-metrics-live` datasource, because the same
# series read back from Fly's managed Prometheus run 15-25 minutes behind. THIS HOST HAS NO PATH TO
# THOSE ENDPOINTS: only mongo-1 is a 6PN peer (roles/wireguard-fly.nix), and monitoring-1 is not.
# So `app-metrics-live` is provisioned here pointing at the LOCAL Prometheus -- see the datasource
# block below for why it is provisioned at all rather than dropped.
#
# Grafana's own SQLITE DATABASE DOES NOT COME ACROSS either. Alert state, silences and the
# CI-posted deploy annotations start empty; every rule re-evaluates from Normal, so anything firing
# at cutover fires again as new. That is noise, once. Carrying a database written by one Grafana
# version into another is not something to discover during a cutover.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.grafana;

  # THE APPLICATION REPOSITORY'S OWN PROVISIONING, read by Nix. See the header: this reaches out of
  # infra/ and that is deliberate.
  #
  # THESE FILES ARE SHIPPED BY NIX AND NOT BY FLY, and the path is the only thing that says so.
  # Editing one and running `fly deploy` changes nothing on this host; it takes a
  # `nixos-rebuild switch`. During the migration BOTH are true of the same file, which is the
  # awkward part of sharing it and still better than two copies that disagree.
  flyProvisioning = ../../../../fly/grafana/provisioning;
in
{
  options.fleet.grafana = {
    enable = lib.mkEnableOption "Grafana";

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = config.fleet.privateAddress;
      defaultText = "config.fleet.privateAddress";
      description = ''
        THE PRIVATE ADDRESS. This Grafana holds a read token for the whole Fly org and can query
        every metric this fleet has; it is reached over the VPN or the private network, never from
        the internet. The Fly instance it replaces was public because on Fly there was no private
        alternative -- that was a constraint, not a preference, and it does not come across.
      '';
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 3000;
      description = "Grafana's own default. Kept in step with fleet.prometheus.grafanaPort by an assertion there.";
    };

    rootUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://monitoring-1.kinowo.internal:3000";
      description = ''
        What Grafana writes into the links it generates -- alert notifications above all, which is
        where a wrong value shows up: a Telegram alert whose "view rule" link points at the old
        kinowo-grafana.fly.dev is worse than one with no link, because it lands on an instance that
        may still be running and showing a different truth.
      '';
    };

    secretKeyFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."grafana/secret-key".path;
      defaultText = ''config.sops.secrets."grafana/secret-key".path'';
      description = ''
        Grafana's `security.secret_key`, from sops-nix.

        NOT DECORATIVE: it is the key Grafana encrypts the secrets in its own database with, and
        the historic default (`SW2YcwTIb9zpOOhoPsMm`) is a published constant. The provisioned
        Telegram contact point puts the bot token into grafana.db encrypted under it.

        Read through Grafana's `$__file{}` indirection rather than set as a value, so it never
        enters the world-readable store -- which is where `settings` would otherwise put it.
      '';
    };

    adminPasswordFile = lib.mkOption {
      type = lib.types.str;
      default = config.sops.secrets."grafana/admin-password".path;
      defaultText = ''config.sops.secrets."grafana/admin-password".path'';
      description = ''
        The admin password, from sops-nix, read with `$__file{}` for the same reason.

        THIS INSTANCE HAS NO ANONYMOUS ACCESS (see `settings` below), unlike the bitcashier one --
        the Fly deployment ran with `GF_AUTH_ANONYMOUS_ENABLED=false` and a real admin login, and
        that is carried rather than relaxed. Behind a private address it would be defensible to
        drop the login; it is not free, though, because "on the private network" includes every
        pod k3s will ever schedule on this box.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # `$__file{}` READS THESE AT RUNTIME, so both must be readable by grafana and neither is ever
    # interpolated into a settings value -- `settings` is rendered into the world-readable store.
    sops.secrets."grafana/secret-key" = { owner = "grafana"; mode = "0400"; };
    sops.secrets."grafana/admin-password" = { owner = "grafana"; mode = "0400"; };

    # THE TWO THAT GO THROUGH THE ENVIRONMENT INSTEAD, because Grafana's provisioning files expand
    # environment variables but have no `$__file{}`: the Fly token lands in a datasource header and
    # the bot token in the Telegram contact point.
    #
    # NOTE THE ASYMMETRY WITH THE CHAT ID, which is the whole of gotcha (b) in the header. The bot
    # TOKEN is a secret and is non-numeric, so Grafana's environment expansion keeps it a string
    # and it can safely be a variable reference. The CHAT ID is not a secret and IS numeric, so the
    # same expansion re-types it to a number and Grafana exits at startup unable to unmarshal it --
    # which is why it is a quoted literal in grafana-contactpoints.yaml and does NOT appear here.
    # That is not an inconsistency; it is the two halves of one trap.
    sops.secrets."grafana/fly-token" = { owner = "grafana"; mode = "0400"; };
    sops.secrets."grafana/telegram-bot-token" = { owner = "grafana"; mode = "0400"; };

    # RENDERED INTO /run BY sops-nix, NEVER INTO THE STORE. Grafana reads this as an EnvironmentFile
    # and expands the two names where the provisioning references them.
    sops.templates."grafana.env" = {
      owner = "grafana";
      content = ''
        FLY_PROMETHEUS_TOKEN=${config.sops.placeholder."grafana/fly-token"}
        TELEGRAM_BOT_TOKEN=${config.sops.placeholder."grafana/telegram-bot-token"}
      '';
    };

    services.grafana = {
      enable = true;

      settings = {
        # Carried from the Fly app's [env] block, which is where these were decided.
        analytics = {
          check_for_plugin_updates = false;
          check_for_updates = false;
          enabled = false;
          reporting_enabled = false;
        };
        "auth.anonymous".enabled = false;
        users.allow_sign_up = false;

        server = {
          http_addr = cfg.listenAddress;
          http_port = cfg.port;
          root_url = cfg.rootUrl;
        };

        security = {
          secret_key = "$__file{${cfg.secretKeyFile}}";
          admin_password = "$__file{${cfg.adminPasswordFile}}";
        };

        # THE POINT OF SELF-HOSTING GRAFANA AT ALL. Fly's hosted fly-metrics.net Grafana cannot
        # evaluate alert rules -- that is why the kinowo-grafana app exists, and the reason survives
        # the move to Hetzner unchanged. Stated explicitly rather than left to a default, because a
        # default that flips turns 24 alert rules into 24 dashboards nobody is watching.
        unified_alerting.enabled = true;
      };

      # NO `declarativePlugins`. The Fly instance needs none, and the datasources below are all
      # `type: prometheus`, which is built in. Named so that its absence reads as "nothing needs
      # one" rather than as an oversight -- installing a plugin at runtime is the failure mode the
      # bitcashier grafana role documents at length, and it is avoided here by not needing one.

      provision = {
        enable = true;

        datasources.settings = {
          apiVersion = 1;

          # CARRIED FROM THE FLY PROVISIONING, and harmless until the day it is not. Grafana refuses
          # to change the uid of a datasource that already EXISTS under the same name, and it
          # refuses fatally -- taking the service down. On a fresh database this list does nothing;
          # on a restored one it is what stops a rename from becoming an outage.
          #
          # `deleteDatasources` RUNS BEFORE DATASOURCES ARE PROVISIONED, so an entry here does not
          # delete the datasource declared below it -- the delete happens first, then the create.
          deleteDatasources = [
            { name = "Fly Prometheus"; orgId = 1; }
            { name = "App Metrics (live)"; orgId = 1; }
          ];

          datasources = [
            {
              # FLY'S MANAGED PROMETHEUS. Still the ONLY source of fly_instance_* / fly_edge_*
              # (CPU credit, steal, memory, edge 5xx and p95): those are produced by Fly's own host
              # agent and cannot be scraped from anywhere else. Every one of the alert rules read
              # out of fly/grafana/provisioning/alerting/alert-rules.yaml queries this uid, so the
              # uid is a CONTRACT with that file, not a name.
              name = "Fly Prometheus";
              uid = "fly-prometheus";
              orgId = 1;
              type = "prometheus";
              access = "proxy";
              url = "https://api.fly.io/prometheus/personal";
              # Default, as on the Fly instance: the ported dashboards' panels that omit an
              # explicit datasource resolve here.
              isDefault = true;
              editable = false;
              jsonData = {
                httpMethod = "POST";
                prometheusType = "Prometheus";
                timeInterval = "15s";
                httpHeaderName1 = "Authorization";
              };
              secureJsonData = {
                # GOTCHA (a). `FlyV1`, NOT `Bearer`. See the module header -- a Bearer header 401s
                # with "resolving organization", which reads as a bad token and sends whoever is
                # debugging it off to issue a new one.
                #
                # `$FLY_PROMETHEUS_TOKEN` without braces, deliberately: Grafana accepts both forms,
                # and the braced form has to be escaped in Nix, where an unescaped `${...}` is
                # string interpolation and would put the token's VALUE in the store if it ever
                # resolved to one.
                httpHeaderValue1 = "FlyV1 $FLY_PROMETHEUS_TOKEN";
              };
            }

            {
              # THIS HOST'S OWN PROMETHEUS -- the node_exporters, mongod's dump freshness, the
              # WireGuard handshake age, and this stack watching itself. Everything in
              # nix/files/monitoring/rules/ is queried through here.
              name = "Prometheus (Hetzner)";
              uid = "local-prometheus";
              orgId = 1;
              type = "prometheus";
              access = "proxy";
              # 127.0.0.1 rather than the private address: Grafana and Prometheus are the same box,
              # so this query never needs to touch a wire -- and it keeps working if the private
              # address changes. Not `localhost`, which can resolve to ::1 and produce a
              # connection-refused against a service listening on IPv4 only.
              url = "http://127.0.0.1:9090";
              isDefault = false;
              editable = false;
              jsonData = {
                httpMethod = "POST";
                prometheusType = "Prometheus";
                # Matches the scrape_interval in files/monitoring/prometheus.yaml. Wrong here, and
                # rate() over short windows silently returns nothing.
                timeInterval = "15s";
              };
            }

            {
              # A COMPATIBILITY ENTRY, AND THE MOST DEBATABLE LINE IN THIS FILE. Read the header
              # first.
              #
              # On Fly, `app-metrics-live` is a VictoriaMetrics sidecar scraping the apps over 6PN.
              # There is no such thing on this host. But the uid is a QUERY SURFACE: 92 panel
              # targets across the three ported dashboards name it, and a uid that resolves to
              # nothing does not degrade -- every one of those panels renders "Datasource
              # app-metrics-live was not found", which makes the dashboards unreadable rather than
              # empty.
              #
              # So it is provisioned, pointing at the local Prometheus, and WHAT THAT BUYS IS
              # HONESTY ABOUT THE GAP RATHER THAN THE DATA: those panels will show "No data" until
              # this Prometheus actually scrapes the apps' /metrics endpoints, which needs a 6PN
              # path from THIS host -- a second `fly wireguard create` peer, exactly as
              # roles/wireguard-fly.nix does for mongo-1 -- or the apps moving here. That is a
              # known, named gap and it is not fixed in this change.
              #
              # THE OTHER OPTION WAS TO REWRITE THE DASHBOARDS' uid REFERENCES, which is 92 edits in
              # a file shared with the still-live Fly instance, where they would then be wrong. Not
              # while both run.
              name = "App Metrics (live)";
              uid = "app-metrics-live";
              orgId = 1;
              type = "prometheus";
              access = "proxy";
              url = "http://127.0.0.1:9090";
              isDefault = false;
              editable = false;
              jsonData = { httpMethod = "POST"; prometheusType = "Prometheus"; timeInterval = "15s"; };
            }
          ];
        };

        dashboards.settings = {
          apiVersion = 1;
          providers = [{
            name = "kinowo";
            orgId = 1;
            folder = "Fly";
            type = "file";
            # BOTH FALSE, WHICH IS A CHANGE FROM THE FLY PROVISIONING (it allows UI updates and
            # deletion). There, the dashboards were the only copy anyone could edit; here the file
            # in fly/grafana/provisioning/dashboards IS the source and a UI edit would be silently
            # reverted on the next provisioning pass -- which is the confusing outcome. Making it
            # refuse the edit is kinder than letting it be made and lost.
            disableDeletion = true;
            allowUiUpdates = false;
            updateIntervalSeconds = 60;
            options.path = "${flyProvisioning}/dashboards";
          }];
        };

        alerting = {
          # THE 24 ALERT RULES, in four groups (fly-resources, fly-serving, app-uptime,
          # worker-throttle), read from the application repository. Every expression in them was
          # validated against the live Fly endpoint when it was written, and every one queries the
          # `fly-prometheus` uid above -- which is why that uid is not renamed here.
          rules.path = "${flyProvisioning}/alerting/alert-rules.yaml";

          # THE CONTACT POINT AND THE NOTIFICATION POLICY, vendored because they had to change.
          # Both live in one file: a route naming a receiver that was never written is FATAL at
          # startup, not inert, so the two belong where they can be read together.
          contactPoints.path = ../../files/monitoring/grafana-contactpoints.yaml;
        };
      };
    };

    systemd.services.grafana.serviceConfig = {
      EnvironmentFile = config.sops.templates."grafana.env".path;

      # Below Prometheus and Alertmanager (400/300 in roles/prometheus.nix), above k3s's default of
      # 100. Grafana is how a person LOOKS at the incident; Prometheus is what makes there be
      # anything to look at afterwards. If the box can only run some of them, that is the order.
      CPUWeight = 200;
    };

    # PRIVATE INTERFACE ONLY. See `listenAddress` -- this instance holds a token for the whole Fly
    # organisation.
    networking.firewall.interfaces.${config.fleet.privateInterface}.allowedTCPPorts = [ cfg.port ];
  };
}
