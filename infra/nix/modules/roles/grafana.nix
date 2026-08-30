# Grafana, provisioned as code -- the port of the `kinowo-grafana` Fly app.
#
# ------------------------------------------------------------------------------------------------
# WHERE THE PROVISIONING COMES FROM, AND WHY IT IS VENDORED RATHER THAN READ IN PLACE
# ------------------------------------------------------------------------------------------------
#
# The alert rules and the dashboards STARTED as the Fly instance's -- fly/grafana/provisioning/ --
# copied into nix/files/monitoring/grafana/. Not transcribed, not rewritten: copied, and the
# dashboards byte for byte.
#
# ------------------------------------------------------------------------------------------------
# THEY ARE NO LONGER BYTE-FOR-BYTE COPIES. POST-MIGRATION AUDIT, 2026-08-29.
# ------------------------------------------------------------------------------------------------
#
# The copy was audited against the world that exists after the move off Fly, and four things
# changed. Anyone running the `diff -r` this header recommends will see exactly these and nothing
# else; that is the intended state, not drift.
#
#   1. FOUR ALERT RULES DELETED AND ONE CHANGED in alerting/alert-rules.yaml -- two that named Fly
#      volume mountpoints belonging to machines that no longer exist, one ("Mongo down") that was
#      `noDataState: Alerting` on a retired app and therefore firing permanently, and one whose only
#      consumer was a webhook this host cannot reach. The one change is `Serving app down`'s
#      no-data handling, for the same permanent-firing reason. Every deletion leaves a tombstone
#      comment in place of the rule, and every group carries a written verdict for each of its
#      surviving rules. Twenty rules remain, not twenty-four.
#   2. THE DASHBOARDS ARE SPLIT INTO TWO DIRECTORIES, dashboards/fleet and dashboards/apps, and
#      provisioned into two Grafana folders by two providers (below). See the note on those
#      providers for why quarantine beats a mixed folder.
#   3. EACH FLY DASHBOARD GAINED A BANNER PANEL AND A RETITLE saying it is quarantined, why, and
#      what would bring it back. A page full of "No data" with no explanation is indistinguishable
#      from a page that is broken.
#   4. A NEW DASHBOARD, dashboards/fleet/kinowo-fleet.json, which has no counterpart on the Fly
#      side at all -- it is about the three Hetzner machines, which did not exist there.
#   5. TWO NEW ROWS AT THE FOOT OF dashboards/apps/fly-overview.json, about mongo-1 and
#      monitoring-1. They are the second place the copy diverges by ADDITION rather than deletion,
#      and they could not have existed on Fly: the database was a Fly app whose internals nothing
#      scraped, and there was no monitoring box to describe. They read the `node` and `mongodb`
#      jobs, which exist only on this fleet. The `diff -r` this header recommends will show them.
#
# COPIED RATHER THAN REFERENCED BECAUSE OF THE FLAKE ROOT, which is infra/ and not the repository
# root (infra/flake.nix's header sets out why: rooting it at the top drags ~18k tracked files of a
# Scala application into the store on every evaluation). A Nix path outside the flake root does not
# exist under pure evaluation, so reading `../fly/grafana/...` from here is not an option, however
# much this file would prefer one copy over two. bitcashier's equivalent role DOES read its
# Grafana content in place, out of a Puppet tree, because its flake is rooted at the repository
# root -- that is the one structural difference between the two fleets, and it is why this file
# and that one differ on the one point they otherwise agree about.
#
# WHAT THE COPY COSTS, so nobody is surprised by it: the Fly instance is KEPT as the rollback, so
# these files exist twice for the length of the migration and a rule fixed on the Fly side -- which
# is when rules get fixed -- is not fixed here. infra/flake.nix names
# `infra/bin/sync-grafana-provisioning` as what keeps the copy honest; THAT SCRIPT DOES NOT EXIST
# YET, so today the answer is `diff -r` before trusting either copy. It should exist, and CI should
# run it.
#
# TRANSCRIBING WAS NEVER THE ALTERNATIVE. A wrong threshold in an alert rule does not fail, it goes
# quiet -- indistinguishable from a good night -- so 1,279 lines of rules retyped by hand is a set
# of alerts that is 95% right, which is worse than none because it is trusted.
#
# THE CONTACT POINTS AND THE NOTIFICATION POLICY ARE DIFFERENT: they are vendored because they had
# to CHANGE (a different chat id, and the worker-throttle webhooks dropped), so they live in
# nix/files/monitoring/grafana-contactpoints.yaml with their own reasoning written into them.
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
# DIRECTLY over Fly 6PN, which is how the web tier is monitored now that Fly's managed Prometheus
# is unreachable. THIS HOST HOLDS ITS OWN 6PN PEER (roles/wireguard-fly.nix on monitoring-1), so the
# `kinowo-web` scrape job reads the app's /metrics at `kinowo.internal:9000` directly. The
# `app-metrics-live` datasource that used to stand in for that gap is gone -- the gap is closed.
#
# THE CONSEQUENCE IS A WHOLE FOLDER OF EMPTY DASHBOARDS, and the post-migration audit's answer to it
# is quarantine rather than deletion: the three Fly dashboards live in their own Grafana folder,
# named for the reason they are empty, each opening with a banner panel that says what would bring
# it back. The one dashboard that works -- dashboards/fleet/kinowo-fleet.json, about the three
# Hetzner machines -- is in a folder of its own so that "is the fleet healthy" is answerable without
# walking past four pages of "No data". See the two dashboard providers below.
#
# Grafana's own SQLITE DATABASE DOES NOT COME ACROSS either. Alert state, silences and the
# CI-posted deploy annotations start empty; every rule re-evaluates from Normal, so anything firing
# at cutover fires again as new. That is noise, once. Carrying a database written by one Grafana
# version into another is not something to discover during a cutover.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.grafana;

  # THE VENDORED COPY of fly/grafana/provisioning. See the header for why it is a copy.
  #
  # THESE FILES ARE SHIPPED BY NIX, NOT BY FLY, and nothing about the path says so. Editing the
  # copy and running `fly deploy` changes nothing on this host; editing the ORIGINAL and running
  # `nixos-rebuild switch` changes nothing either. During the migration both files exist and each
  # is deployed by a different tool -- which is the trap this comment exists to name.
  grafanaProvisioning = ../../files/monitoring/grafana;
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

    # GRAFANA'S STATE GOES ON THE VOLUME, not the root disk where nixpkgs defaults it
    # (/var/lib/grafana). Almost everything Grafana holds here is provisioned as code and therefore
    # disposable -- datasources, dashboards, alert rules, the contact point -- but its sqlite is
    # also where ALERT STATE, SILENCES and ANNOTATIONS live, and those are not in the repository.
    # Losing them means every firing alert re-notifying and every silence lifting at once, which is
    # a bad thing to discover during the incident that caused the rebuild.
    #
    # It also makes terraform/server.monitoring.tf true: that block says the volume carries
    # "Prometheus TSDB and Grafana's sqlite", and until this line it carried only the first.
    systemd.tmpfiles.rules = [
      "d /var/lib/monitoring/grafana 0700 grafana grafana -"
    ];

    services.grafana = {
      dataDir = "/var/lib/monitoring/grafana";

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
        # default that flips turns 20 alert rules into 20 dashboards nobody is watching.
        unified_alerting.enabled = true;
      };

      # ONE PLUGIN, DECLARATIVELY, AND NEVER AT RUNTIME. Every datasource here is `type: prometheus`
      # -- built in -- except VictoriaLogs, which needs its own. `declarativePlugins` puts it in the
      # closure, so it is present before Grafana starts and identical on a rebuilt host.
      #
      # The alternative, letting Grafana install it at first run, is the failure the bitcashier role
      # documents at length: a plugin fetched at runtime makes startup depend on a third party being
      # reachable, and a host rebuilt while that is down comes up with a datasource whose panels all
      # error and no indication why.
      declarativePlugins = [ pkgs.grafanaPlugins.victoriametrics-logs-datasource ];

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
            # REMOVED 2026-08-29, and listed here rather than merely deleted so Grafana drops it
            # from instances that already provisioned it -- a datasource left behind stays queryable
            # and keeps a stale uid resolvable, which is how a "working" panel ends up reading
            # nothing.
            { name = "App Metrics (live)"; orgId = 1; }
          ];

          datasources = [
            {
              # FLY'S MANAGED PROMETHEUS. Still the ONLY source of fly_instance_* / fly_edge_*
              # (CPU credit, steal, memory, edge 5xx and p95): those are produced by Fly's own host
              # agent and cannot be scraped from anywhere else. Every one of the alert rules read
              # out of fly/grafana/provisioning/alerting/alert-rules.yaml queries this uid, so the
              # uid is a CONTRACT with that file, not a name.
              # KEPT THOUGH NOTHING QUERIES IT. Its token is revoked and no dashboard references
              # it any more -- the apps are scraped directly over this host's 6PN peer, which is
              # better data than Fly's proxy view ever was. It stays as the declared path back: if a
              # read-only Fly token is ever minted, `fleet.prometheus.scrapeFly = true` plus this
              # entry is the whole of turning Fly's own view back on. An unreferenced datasource
              # costs nothing; rediscovering how to re-enable it costs an afternoon.
              name = "Fly Prometheus";
              uid = "fly-prometheus";
              orgId = 1;
              type = "prometheus";
              access = "proxy";
              url = "https://api.fly.io/prometheus/personal";
              # NO LONGER THE DEFAULT -- it was, on the Fly instance, where it was the only useful
              # source. Here the default belongs to the datasource that WORKS, and this one has no
              # credential: the read-only tokens were revoked and cannot be reissued (see
              # `fleet.prometheus.scrapeFly`).
              #
              # WHY THE FLIP IS SAFE, and it was checked rather than assumed: the default is what a
              # panel or an ad-hoc query resolves to when it names no datasource, and EVERY PANEL
              # in all four vendored dashboards carries an explicit `datasource` block. (Some
              # TARGETS omit one, but a target inherits its panel's, not the default.) So nothing
              # silently changes datasource because of this line; what changes is where a NEW panel
              # or a query typed into Explore points -- and pointing those at a datasource that
              # 401s is a bad first experience of this Grafana.
              isDefault = false;
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
              # THE FLEET'S LOGS.
              #
              # THE PRIVATE ADDRESS, NOT LOOPBACK, EVEN THOUGH IT IS THE SAME MACHINE. VictoriaLogs
              # follows this fleet's convention and binds `fleet.privateAddress` ONLY -- nothing is
              # listening on 127.0.0.1:9428. This line said 127.0.0.1 on the assumption that
              # same-host meant loopback; the datasource answered nothing and the panels were empty
              # while the service was perfectly healthy, which is exactly the shape of failure that
              # wastes an afternoon looking at the wrong process.
              #
              # It matches how Prometheus is addressed above, and how fleet/logs.nix ships to it.
              name = "VictoriaLogs";
              uid = "victorialogs";
              orgId = 1;
              type = "victoriametrics-logs-datasource";
              access = "proxy";
              url = "http://10.20.0.11:9428";
              isDefault = false;
              editable = false;
            }
            {
              # THIS HOST'S OWN PROMETHEUS -- the node_exporters, mongod's dump freshness, the
              # WireGuard handshake age, the deploy state, and this stack watching itself.
              # Everything in nix/files/monitoring/rules/ is queried through here, and so is every
              # panel of dashboards/fleet/kinowo-fleet.json.
              # THE PRIVATE ADDRESS, NOT LOOPBACK, EVEN THOUGH PROMETHEUS IS ON THIS SAME HOST.
              #
              # Every service on this fleet binds `fleet.privateAddress` ONLY -- that is the
              # convention, and Prometheus follows it, so NOTHING listens on 127.0.0.1:9090. A
              # loopback URL here does not fail loudly: Grafana loads, the datasource saves, the
              # dashboards import, and every panel renders "No data" while Prometheus is perfectly
              # healthy and holding all the series. It answers 502 on the datasource proxy and
              # nowhere else.
              #
              # This was wrong for both Prometheus datasources AND for VictoriaLogs, found on
              # 2026-08-29 only by querying the proxy directly. If a panel is empty, check this
              # before you check the metric name.
              name = "Prometheus (Hetzner)";
              uid = "local-prometheus";
              orgId = 1;
              type = "prometheus";
              access = "proxy";
              # 127.0.0.1 rather than the private address: Grafana and Prometheus are the same box,
              # so this query never needs to touch a wire -- and it keeps working if the private
              # address changes. Not `localhost`, which can resolve to ::1 and produce a
              # connection-refused against a service listening on IPv4 only.
              url = "http://10.20.0.11:9090";
              # THE DEFAULT, as of the post-migration audit. It is the only datasource on this
              # instance that answers, it is where every rule this fleet actually evaluates lives,
              # and it needs no credential -- so an unqualified query, a new panel or anything
              # typed into Explore should land here rather than on a 401.
              isDefault = true;
              editable = false;
              jsonData = {
                httpMethod = "POST";
                prometheusType = "Prometheus";
                # Matches the scrape_interval in files/monitoring/prometheus.yaml. Wrong here, and
                # rate() over short windows silently returns nothing.
                timeInterval = "15s";
              };
            }

          ];
        };

        dashboards.settings = {
          apiVersion = 1;

          # ------------------------------------------------------------------------------------
          # TWO PROVIDERS AND TWO FOLDERS: THE FLEET'S OWN HEALTH, AND THE APPLICATION'S.
          # ------------------------------------------------------------------------------------
          #
          # THIS SPLIT USED TO BE A QUARANTINE and is not one any more, which is worth saying
          # because the shape has not changed while the reason entirely has.
          #
          # It was introduced when three of four dashboards read datasources that could not work --
          # `fly-prometheus` (token revoked, unmintable) and a stand-in for a 6PN path this host did
          # not have -- so those pages rendered nothing. The argument was that a folder of
          # permanently-empty pages reads as "the monitoring is broken" to somebody opening Grafana
          # mid-incident, and is a folder people stop opening.
          #
          # BOTH CAUSES ARE GONE. monitoring-1 now holds its own 6PN peer, so the web tier's own
          # /metrics is scraped directly -- richer than the Fly proxy view it replaces, and with a
          # credential this fleet holds. The worker is scraped from k3s. Both dashboards were
          # rebuilt on those series and every query was verified returning data.
          #
          # WHAT THE SPLIT MEANS NOW is simply subject: `Kinowo Fleet` is about the three machines
          # (hosts, disks, closures, auto-apply), `Application` is about the software running on
          # them (queue depth, projection cost, request rate, JVM). A reader with a symptom knows
          # which to open, which is a better reason to have two folders than the one it replaced.
          #
          # filmowo-overview.json was DELETED in the same change rather than moved: it watches a
          # different application, which nothing on this fleet scrapes, so unlike these two it could
          # not be rebuilt on real data. It is one `git revert` away if that app is ever scraped.
          providers = [
            {
              # THE ONE THAT WORKS. Everything in it queries `local-prometheus` and needs no
              # credential of any kind.
              name = "kinowo-fleet";
              orgId = 1;
              folder = "Kinowo Fleet";
              type = "file";
              # FALSE, SO PROVISIONING PRUNES. It was true, and the effect was that a dashboard
              # deleted from this repository stayed in Grafana for ever -- which makes the repo the
              # source of truth for dashboards that EXIST and not for dashboards that should not.
              # Found when filmowo-overview.json was removed and went on being served.
              #
              # The risk this trades against is real but smaller: with deletion enabled, a
              # provisioning pass that somehow sees an empty directory would remove the dashboards.
              # That is bounded -- they are files in git, restored by the next deploy -- whereas a
              # stale dashboard nobody can delete without touching the box is unbounded drift, and
              # it is exactly the kind that shows wrong numbers with total confidence.
              #
              # `allowUiUpdates` stays false: the file is the source, and a UI edit that is silently
              # reverted on the next pass is more confusing than one that is refused.
              disableDeletion = false;
              allowUiUpdates = false;
              updateIntervalSeconds = 60;
              options.path = "${grafanaProvisioning}/dashboards/fleet";
            }

            {
              # THE QUARANTINE. The folder NAME is the label -- it is what somebody sees in the
              # dashboard list before they open anything, which is the only place the warning can
              # arrive early enough to be useful.
              name = "kinowo-apps";
              orgId = 1;
              # RENAMED FROM "Fly (no data without a token)" ON 2026-08-29, because both halves of that name
              # stopped being true. These dashboards no longer read Fly's managed Prometheus: the worker
              # is on k3s and scraped by NodePort, and the web tier -- still on Fly -- is scraped
              # directly over a 6PN WireGuard peer. Every query in them was verified returning data.
              #
              # The directory moved from dashboards/fly to dashboards/apps for the same reason: it
              # holds the APPLICATION's dashboards, and where each tier runs is now a label on the
              # series (`platform`), not a fact about the folder.
              folder = "Application";
              type = "file";
              # BOTH FALSE, WHICH IS A CHANGE FROM THE FLY PROVISIONING (it allows UI updates and
              # deletion). There, the dashboards were the only copy anyone could edit; here the
              # file in fly/grafana/provisioning/dashboards IS the source and a UI edit would be
              # silently reverted on the next provisioning pass -- which is the confusing outcome.
              # Making it refuse the edit is kinder than letting it be made and lost.
              # FALSE, SO PROVISIONING PRUNES. It was true, and the effect was that a dashboard
              # deleted from this repository stayed in Grafana for ever -- which makes the repo the
              # source of truth for dashboards that EXIST and not for dashboards that should not.
              # Found when filmowo-overview.json was removed and went on being served.
              #
              # The risk this trades against is real but smaller: with deletion enabled, a
              # provisioning pass that somehow sees an empty directory would remove the dashboards.
              # That is bounded -- they are files in git, restored by the next deploy -- whereas a
              # stale dashboard nobody can delete without touching the box is unbounded drift, and
              # it is exactly the kind that shows wrong numbers with total confidence.
              #
              # `allowUiUpdates` stays false: the file is the source, and a UI edit that is silently
              # reverted on the next pass is more confusing than one that is refused.
              disableDeletion = false;
              allowUiUpdates = false;
              updateIntervalSeconds = 60;
              options.path = "${grafanaProvisioning}/dashboards/apps";
            }
          ];
        };

        alerting = {
          # THE 20 ALERT RULES (24 before the post-migration audit deleted four), in the same four
          # groups: fly-resources, fly-serving, app-uptime, worker-throttle. Every expression in
          # them was validated against the live Fly endpoint when it was written, and every one
          # queries the `fly-prometheus` uid above -- which is why that uid is carried across
          # unrenamed. Renaming it here would empty all 20 rules without any of them failing.
          #
          # NONE OF THEM IS PROTECTING ANYTHING TODAY, and that is worth knowing at the composition
          # root rather than only in the file: the datasource they all query has no working token,
          # so they evaluate to an execution error and (being `execErrState: OK`, nearly all of
          # them) go quietly to Normal. A rule sitting in Normal because its datasource is
          # unreachable looks exactly like a rule sitting in Normal because everything is fine.
          #
          # THE ALERTING THAT IS ACTUALLY RUNNING ON THIS FLEET is Prometheus's, not Grafana's:
          # nix/files/monitoring/rules/*.rules, evaluated by the process next door and delivered
          # through Alertmanager. Grafana's unified alerting is kept enabled for these Fly rules
          # and for the day they work again.
          rules.path = "${grafanaProvisioning}/alerting/alert-rules.yaml";

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

    # NO FIREWALL RULE HERE. `fleet.firewall.monitoring = true;` in the host file opens 3000 on the
    # private interface, alongside Prometheus's and Alertmanager's ports; modules/fleet/firewall.nix
    # holds the whole list, and its comment on that option is candid that a human's web UI on a
    # network no human's laptop is on means reaching it through an ssh tunnel -- which is the price
    # of keeping a login page with its own user database off the public internet.
  };
}
