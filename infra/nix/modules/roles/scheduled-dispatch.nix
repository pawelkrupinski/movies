# THE EXTERNAL SCHEDULER FOR THE REPOSITORY'S DAILY AND WEEKLY GITHUB ACTIONS WORKFLOWS.
#
# GitHub's `schedule:` events are best effort: identity-family-explorer.yml's Sunday slot was not run at
# all on 2026-11-08, and every scheduled run of 2026-10-31..11-08 started 3-10 h after its cron. One
# timer per covered workflow fires at its slot (OnCalendar, UTC); its oneshot waits a 15-minute grace,
# sends the repository_dispatch `scheduled-<workflow>` unless GitHub's own run has started, and then
# cancels GitHub's late copy of the slot for up to 14 h. The logic, the outcome lines and why it is a
# repository_dispatch rather than a workflow_dispatch: ../../files/scheduled_dispatch.py.
#
# THE SCHEDULE IS THE WORKFLOWS' OWN CRONS, never a second list: ../../files/scheduled-workflows.json is
# generated from .github/workflows/*.yml (`python3 infra/nix/files/scheduled_dispatch.py generate`), and
# scheduled_dispatch_test.py fails when it, or a workflow's wiring, drifts from them.
#
# ALERTING: a slot the dispatcher could not dispatch (the API down through its retries) fails the unit,
# and host-health.rules' SystemdUnitFailed pages. Every decision is a `scheduled-dispatch:` journal line
# in VictoriaLogs (`outcome=missed-dispatched`, `outcome=late-duplicate-cancelled`, ...).
#
# THE CREDENTIAL is the kinowo-lander GitHub App's private key (the App agentctl lands and dispatches
# as on ci-runner; Contents RW for repository_dispatch, Actions RW for the cancel), from sops as
# `scheduled-dispatch/github-app-key`, handed to a DynamicUser service with LoadCredential.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.scheduledDispatch;
  schedule = ../../files/scheduled-workflows.json;
  workflows = (lib.importJSON schedule).workflows;

  script = pkgs.writers.writePython3Bin "scheduled-dispatch" {
    flakeIgnore = [ "E501" "E265" ];
  } (builtins.readFile ../../files/scheduled_dispatch.py);

  unit = name: "scheduled-dispatch-${name}";
in
{
  options.fleet.scheduledDispatch = {
    enable = lib.mkEnableOption "the external scheduler of the repository's daily and weekly workflows";
    repository = lib.mkOption {
      type = lib.types.str;
      default = "pawelkrupinski/movies";
      description = "The repository whose workflows are dispatched.";
    };
    appId = lib.mkOption {
      type = lib.types.str;
      # kinowo-lander: bitcashier infra/nix/hosts/ci-runner/github-apps.nix. App ids are public.
      default = "5553197";
      description = "The GitHub App the dispatcher acts as; its private key is the sops secret scheduled-dispatch/github-app-key.";
    };
    graceMinutes = lib.mkOption {
      type = lib.types.int;
      default = 15;
      description = "How long after a slot GitHub's own run may start before the slot is dispatched.";
    };
    guardHours = lib.mkOption {
      type = lib.types.int;
      default = 14;
      description = "How long after a slot GitHub's late schedule run is cancelled (never past the next slot).";
    };
  };

  config = lib.mkIf cfg.enable {
    sops.secrets."scheduled-dispatch/github-app-key" = { mode = "0400"; };

    systemd.services = lib.mapAttrs' (name: workflow: lib.nameValuePair (unit name) {
      description = "Dispatch ${workflow.file}'s slot unless GitHub ran it";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      path = [ pkgs.openssl ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs [
          "${script}/bin/scheduled-dispatch" "run" name
          "--schedule" "${schedule}"
          "--repository" cfg.repository
          "--app-id" cfg.appId
          "--grace-minutes" (toString cfg.graceMinutes)
          "--guard-hours" (toString cfg.guardHours)
        ];
        LoadCredential = "github-app-key:${config.sops.secrets."scheduled-dispatch/github-app-key".path}";
        # The grace, the retries, then the guard.
        TimeoutStartSec = "${toString (cfg.guardHours * 60 + cfg.graceMinutes + 30)}min";
        DynamicUser = true;
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectHome = true;
        ProtectSystem = "strict";
      };
    }) workflows;

    systemd.timers = lib.mapAttrs' (name: workflow: lib.nameValuePair (unit name) {
      description = "${workflow.file}'s cron slots: ${lib.concatStringsSep ", " workflow.crons}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = workflow.onCalendar;
        # A slot passed while the host was down is settled at boot: the service finds the latest slot itself.
        Persistent = true;
      };
    }) workflows;

    # A restart mid-guard is harmless (the restarted run finds its own dispatch: `dispatched-earlier`), so a
    # changed schedule or script rolls out unattended even while a guard is running.
    fleet.autoApply.restartableUnits = lib.concatMap (name: [ "${unit name}.service" "${unit name}.timer" ])
      (lib.attrNames workflows);
  };
}
