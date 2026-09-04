# Grafana, provisioned as code. It started life as a port of the `kinowo-grafana` Fly app.
#
# ------------------------------------------------------------------------------------------------
# WHERE THE PROVISIONING COMES FROM
# ------------------------------------------------------------------------------------------------
#
# The alert rules and the dashboards STARTED as the `kinowo-grafana` Fly app's, under
# fly/grafana/provisioning/ in the application repository, copied into
# nix/files/monitoring/grafana/. That original is DELETED: the Fly Grafana was scaled to zero when
# this host took over, its rollback workflow went unrun long enough that the copy it would have
# deployed had drifted from what actually serves, and a rollback to a Grafana nobody has seen is
# not a rollback. nix/files/monitoring/grafana/ is the only copy now -- edit it,
# `nixos-rebuild switch`, done.
#
# COPIED RATHER THAN REFERENCED, while both existed, BECAUSE OF THE FLAKE ROOT, which is infra/ and
# not the repository root (infra/flake.nix's header sets out why: rooting it at the top drags ~18k
# tracked files of a Scala application into the store on every evaluation). A Nix path outside the
# flake root does not exist under pure evaluation. bitcashier's equivalent role DOES read its
# Grafana content in place, out of a Puppet tree, because its flake is rooted at the repository
# root -- that is the one structural difference between the two fleets.
#
# TRANSCRIBING WAS NEVER THE ALTERNATIVE. A wrong threshold in an alert rule does not fail, it goes
# quiet -- indistinguishable from a good night -- so 1,279 lines of rules retyped by hand is a set
# of alerts that is 95% right, which is worse than none because it is trusted.
#
# THE CONTACT POINTS AND THE NOTIFICATION POLICY live in their own file,
# nix/files/monitoring/grafana-contactpoints.yaml, because they had to CHANGE on the way across (a
# different chat id, and the worker-throttle webhooks dropped). Their reasoning is written into
# them.
#
# ------------------------------------------------------------------------------------------------
# TWO ROUNDS OF EDITS SINCE THE COPY, BOTH RECORDED BECAUSE THE TOMBSTONES BELOW REFER TO THEM
# ------------------------------------------------------------------------------------------------
#
# 2026-08-29, THE POST-MIGRATION AUDIT. Four alert rules deleted and one changed: two named Fly
# volume mountpoints belonging to machines that no longer existed, one ("Mongo down") was
# `noDataState: Alerting` on a retired app and therefore firing permanently, and one's only
# consumer was a webhook this host cannot reach. The dashboards were split into dashboards/fleet
# and dashboards/apps, provisioned into two Grafana folders by two providers (below); the Fly-fed
# ones gained a banner panel saying they were quarantined and why, since a page full of "No data"
# with no explanation is indistinguishable from a broken one. Two dashboards were ADDED that could
# not have existed on Fly: dashboards/fleet/kinowo-fleet.json, about the three Hetzner machines,
# and two rows at the foot of dashboards/apps/application-health.json about mongo-1 and monitoring-1.
#
# 2026-09-04, THE FLY DATASOURCE REMOVED. `fly-prometheus` had no credential -- both read-only
# tokens were revoked and the org-wide one cannot mint a replacement -- so every rule pointed at it
# evaluated to an execution error and went quietly to Normal, protecting nothing. Ten rules read
# `fly_instance_*` / `fly_app_*` and were deleted with it; ELEVEN read ordinary application metrics
# this fleet's own Prometheus already scrapes, and were repointed at `local-prometheus`, where they
# work. See the header of alerting/alert-rules.yaml, which carries the split rule by rule.
#
# NOTHING FLY-SHAPED IS LEFT after that round. The folder split it created survives on its own
# merits (see below), the app dashboards read only `local-prometheus`, and no panel anywhere is
# waiting on a series this fleet cannot produce.
#
# ------------------------------------------------------------------------------------------------
# THE TWO GOTCHAS THAT MUST SURVIVE THIS PORT. BOTH ARE VERIFIED, BOTH COST A DEBUGGING SESSION.
# ------------------------------------------------------------------------------------------------
#
# (a) THE TELEGRAM `chatid` MUST BE A QUOTED LITERAL, NEVER `${VAR}`. (There used to be a gotcha
#     (a) about the Fly datasource's `FlyV1 <token>` auth header, which 401s with "resolving
#     organization" when written as `Bearer` -- an error that reads like a revoked token and sends
#     whoever is debugging it off to mint a new one. The datasource is gone; the trap is recorded
#     here in case anything ever talks to api.fly.io again.)
#
# (b) Restating that, because it is the one that takes the instance down:
#     THE TELEGRAM `chatid`# (b) THE TELEGRAM `chatid` MUST BE A QUOTED LITERAL, NEVER `${VAR}`. Grafana expands environment
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
# WHAT THIS FLEET CANNOT SEE, NAMED SO NOBODY THINKS IT WAS FORGOTTEN
# ------------------------------------------------------------------------------------------------
#
# Every app is scraped over its NodePort by the Prometheus next door
# (nix/files/monitoring/scrape-kinowo-apps.yaml), so every panel on every dashboard here reads a
# real series -- verified by infra/test/test_dashboards.py, which fails on a panel naming any
# datasource other than the two provisioned below. THERE ARE NO QUARANTINED OR EMPTY PANELS. The
# ones that used to read Fly's host agent were not left to sit blank; they were rewritten onto the
# apps' own instrumentation during the 2026-08-29 audit, which is why several panel descriptions
# still say what they replaced.
#
# WHAT GENUINELY HAS NO COUNTERPART, and no panel pretends otherwise: CPU steal, shared-CPU credit,
# and edge 5xx / edge latency. The first two were properties of somebody else's scheduler and mean
# nothing on a dedicated box; the last two were measured by a proxy this fleet does not have, and
# the apps' own request instrumentation on dashboards/apps/kinowo-http.json is the closer reading
# anyway -- it measures the work rather than the edge in front of it. For the machines,
# rules/host-health.rules and rules/jvm-heap.rules alert rather than chart.
#
# THE FOLDER SPLIT SURVIVES THAT, for a different reason than it was created for. It was
# quarantine: three Fly-fed dashboards full of "No data" kept away from the one that worked. Now it
# is simply scope -- dashboards/fleet is "is the fleet healthy" (three Hetzner machines) and
# dashboards/apps is "is the product healthy", and keeping them apart is what makes the first
# answerable without reading the second. See the two dashboard providers below.
#
# Grafana's own SQLITE DATABASE DOES NOT COME ACROSS either. Alert state, silences and the
# CI-posted deploy annotations start empty; every rule re-evaluates from Normal, so anything firing
# at cutover fires again as new. That is noise, once. Carrying a database written by one Grafana
# version into another is not something to discover during a cutover.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.grafana;

  # THE PROVISIONING, and the only copy of it -- see the header for where it came from. Shipped by
  # Nix: edit these files and `nixos-rebuild switch`. During the migration there was a second copy
  # under fly/grafana/provisioning deployed by a different tool, and editing the wrong one changed
  # nothing anywhere; that is why it is gone rather than kept "in case".
  grafanaProvisioning = ../../files/monitoring/grafana;

  # `restartableUnits` and `neverDisturbUnits` are fnmatch GLOBS -- the applier matches them with
  # Python's fnmatch, so `*` and `?` are the two wildcards and a host may well write `grafana.*` or
  # `*`. A plain `elem` would read those as literals and report a host that HAS forgiven Grafana as
  # one that has not, which is the worst direction for the assertion below to be wrong in. This is
  # the same translation, kept to the two wildcards fnmatch and Nix's regexes agree on.
  matchesUnit = unit: pattern:
    let
      escaped = lib.escapeRegex pattern;
      wildcarded = lib.replaceStrings [ "\\*" "\\?" ] [ ".*" "." ] escaped;
    in
    builtins.match wildcarded unit != null;
in
{
  options.fleet.grafana = {
    enable = lib.mkEnableOption "Grafana";

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = config.fleet.privateAddress;
      defaultText = "config.fleet.privateAddress";
      description = ''
        THE PRIVATE ADDRESS. This Grafana can query every metric this fleet has; it is reached
        over the VPN or the private network, never from the internet. The Fly instance it replaces
        was public because on Fly there was no private alternative -- that was a constraint, not a
        preference, and it did not come across.
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
        where a wrong value shows up: a Telegram alert whose "view rule" link points somewhere
        else is worse than one with no link, because it lands on an instance that may still be
        running and showing a different truth. The old value to watch for is kinowo-grafana.fly.dev,
        which is now nothing at all.
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
    # ---------------------------------------------------------------------------------------------
    # A HOST THAT PROVISIONS DASHBOARDS AND DOES NOT FORGIVE A GRAFANA RESTART WEDGES ITS APPLIER.
    # ---------------------------------------------------------------------------------------------
    #
    # The provisioning directory is copied into the store (see `grafanaProvisioning` above) and its
    # store path is baked into config.ini, which is baked into ExecStart. So EDITING ONE LINE OF ONE
    # DASHBOARD CHANGES grafana.service -- and fleet/auto-apply.nix refuses, by design, any switch
    # that would disturb a running unit it has not been told to forgive.
    #
    # THE COST OF GETTING THIS WRONG IS NOT THE STALE DASHBOARD. The applier refuses the WHOLE
    # closure, so every unrelated change staged for this host -- a scrape target, an alert rule, a
    # security patch -- stops landing too, and stops landing SILENTLY: the timer keeps firing, the
    # unit keeps completing, and the only evidence is a line in its journal. Hit on 2026-08-30,
    # where a dashboard edit blocked monitoring-1 and clearing it took a manual
    # switch-to-configuration on the box.
    #
    # TWO WAYS TO SATISFY THIS, and they are different decisions rather than two spellings of one.
    # `restartableUnits` says an unattended bounce of Grafana is a cost this host accepts -- seconds
    # of the monitoring UI, and a gap in no graph at all, since neither Prometheus nor Alertmanager
    # is Grafana. `neverDisturbUnits` says the opposite: this Grafana is deployed by a person, on
    # purpose, and the applier should keep refusing. Either is a position. Neither is the accident
    # this assertion exists to catch.
    assertions = [
      {
        assertion = !config.fleet.autoApply.enable
          || lib.any (matchesUnit "grafana.service") config.fleet.autoApply.restartableUnits
          || lib.any (matchesUnit "grafana.service") config.fleet.autoApply.neverDisturbUnits;
        message = ''
          ${config.networking.hostName} provisions Grafana dashboards and runs nixos-auto-apply, but
          neither fleet.autoApply.restartableUnits nor fleet.autoApply.neverDisturbUnits matches
          grafana.service. A dashboard edit rewrites grafana.service, so the applier will refuse the
          closure -- and with it every unrelated change staged for this host -- and will say so only
          in its own journal.

          Add "grafana.service" to restartableUnits to let dashboard changes land unattended, or to
          neverDisturbUnits to record that this Grafana is deployed by hand on purpose.
        '';
      }
    ];

    # `$__file{}` READS THESE AT RUNTIME, so both must be readable by grafana and neither is ever
    # interpolated into a settings value -- `settings` is rendered into the world-readable store.
    sops.secrets."grafana/secret-key" = { owner = "grafana"; mode = "0400"; };
    sops.secrets."grafana/admin-password" = { owner = "grafana"; mode = "0400"; };

    # THE TWO THAT GO THROUGH THE ENVIRONMENT INSTEAD, because Grafana's provisioning files expand
    # environment variables but have no `$__file{}`: the bot token lands in the Telegram contact
    # point. A `grafana/fly-token` sat beside it for the Fly datasource's `FlyV1` header until
    # 2026-09-04; the datasource is gone and so is the secret.
    #
    # NOTE THE ASYMMETRY WITH THE CHAT ID, which is the whole of gotcha (b) in the header. The bot
    # TOKEN is a secret and is non-numeric, so Grafana's environment expansion keeps it a string
    # and it can safely be a variable reference. The CHAT ID is not a secret and IS numeric, so the
    # same expansion re-types it to a number and Grafana exits at startup unable to unmarshal it --
    # which is why it is a quoted literal in grafana-contactpoints.yaml and does NOT appear here.
    # That is not an inconsistency; it is the two halves of one trap.
    sops.secrets."grafana/telegram-bot-token" = { owner = "grafana"; mode = "0400"; };

    # RENDERED INTO /run BY sops-nix, NEVER INTO THE STORE. Grafana reads this as an EnvironmentFile
    # and expands the name where the provisioning references it.
    sops.templates."grafana.env" = {
      owner = "grafana";
      content = ''
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
          #
          # BOTH ENTRIES BELOW ARE REMOVED DATASOURCES, listed here rather than merely deleted so
          # Grafana drops them from instances that already provisioned them -- a datasource left
          # behind stays queryable and keeps a stale uid resolvable, which is how a "working" panel
          # ends up reading nothing. "Fly Prometheus" (uid `fly-prometheus`) went on 2026-09-04
          # with the rest of the platform; its credential had been revoked for weeks, and the
          # eleven alert rules that were still pointed at it now read `local-prometheus`, which has
          # the data. "App Metrics (live)" went on 2026-08-29.
          deleteDatasources = [
            { name = "Fly Prometheus"; orgId = 1; }
            { name = "App Metrics (live)"; orgId = 1; }
          ];

          datasources = [
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
          # `fly-prometheus` (token revoked, unmintable) and a stand-in for a scrape path this host
          # did not have -- so those pages rendered nothing. The argument was that a folder of
          # permanently-empty pages reads as "the monitoring is broken" to somebody opening Grafana
          # mid-incident, and is a folder people stop opening.
          #
          # BOTH CAUSES ARE GONE. Every app is scraped over its NodePort by the Prometheus next
          # door -- richer than the Fly proxy view it replaces, and with a credential this fleet
          # holds. Both dashboards were rebuilt on those series and every query was verified
          # returning data.
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
              # THE APPLICATION'S DASHBOARDS. This provider was a QUARANTINE once -- the folder name
              # was the warning label, the only place it could arrive before somebody opened a page
              # of "No data". It is not one now: every panel reads a live series, and the split is
              # kept because "is the fleet healthy" and "is the product healthy" are different
              # questions, not because one of the answers is broken.
              name = "kinowo-apps";
              orgId = 1;
              # RENAMED FROM "Fly (no data without a token)" ON 2026-08-29, because both halves of that
              # name stopped being true. These dashboards do not read Fly's managed Prometheus: every
              # panel in them queries `local-prometheus`, fed by this host's own scrape of the pods'
              # NodePorts. Every query was verified returning data.
              #
              # The directory moved from dashboards/fly to dashboards/apps for the same reason: it
              # holds the APPLICATION's dashboards, and where each tier runs is now a label on the
              # series (`platform`), not a fact about the folder.
              folder = "Application";
              type = "file";
              # BOTH FALSE, WHICH IS A CHANGE FROM THE FLY PROVISIONING (it allows UI updates and
              # deletion). There, the dashboards were the only copy anyone could edit; here the
              # file in nix/files/monitoring/grafana/dashboards IS the source and a UI edit would be
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
          # ELEVEN ALERT RULES in four groups: app-processes, serving, app-uptime, worker-throttle.
          # There were 24 when this was ported, then 20 after the 2026-08-29 audit; the 2026-09-04
          # Fly removal deleted the ten that read `fly_instance_*` / `fly_app_*` and repointed the
          # eleven survivors at `local-prometheus`. All eleven query that uid, so the uid is a
          # CONTRACT with alert-rules.yaml -- renaming it here would empty every rule without any
          # of them failing.
          #
          # THEY CAN ACTUALLY FIRE NOW, which is new. Until the Fly datasource went, every one of
          # them evaluated to an execution error and (being `execErrState: OK`) went quietly to
          # Normal -- indistinguishable from everything being fine. They read the fleet's own
          # Prometheus now.
          #
          # THE OTHER HALF OF THIS FLEET'S ALERTING is Prometheus's rather than Grafana's:
          # nix/files/monitoring/rules/*.rules, evaluated by the process next door and delivered
          # through Alertmanager.
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
