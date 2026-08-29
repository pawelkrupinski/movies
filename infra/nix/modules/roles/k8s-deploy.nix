# HOW CI ROLLS A NEW WORKER IMAGE OUT, and the reason it is not simply "give CI a kubeconfig".
#
# The problem is the same one nix/modules/fleet/deploy-staging.nix solves for closures: a GitHub
# Actions runner has to be able to change production, and anything it holds is one leaked secret
# away from being an attacker's. A kubeconfig for this cluster is cluster-admin -- k3s writes one
# admin credential and no other -- so handing CI that would trade a worker rollout for the whole
# cluster, including every Secret in it.
#
# So the same shape is used instead: an ssh key pinned to a FORCED COMMAND. The holder cannot get a
# shell, cannot run kubectl, and cannot name a resource. It can send one string -- an image
# reference -- to a script that validates it and updates exactly one container in exactly one
# Deployment. That is the entire capability the key grants.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.k8sDeploy;

  endpoint = pkgs.writeShellScript "k8s-deploy-endpoint" ''
    set -euo pipefail

    # The ONLY input, and it arrives out of band precisely so that the key cannot express anything
    # else: with a forced command, whatever the client asked to run lands here as a string and is
    # never executed.
    image="''${SSH_ORIGINAL_COMMAND:-}"

    if [ -z "$image" ]; then
      echo "k8s-deploy: no image given. Send the full image reference as the ssh command." >&2
      exit 2
    fi

    # Only `.` is escaped, not `/`. A slash has no meaning in an ERE, and escaping it makes GNU grep
    # warn "stray \ before /" on every single deploy -- noise in the one output an operator reads
    # when a rollout has gone wrong.
    #
    # VALIDATE BEFORE USING, because this string reaches a command line. Anchored, and deliberately
    # narrow: one registry, one repository, and a tag of hex or dotted-alphanumerics. A digest or a
    # tag from somewhere else is refused rather than sanitised -- there is no legitimate caller that
    # needs one, and "sanitise an arbitrary string" is a much harder promise to keep than "match a
    # known shape".
    if ! printf '%s' "$image" | ${pkgs.gnugrep}/bin/grep -qE '^${builtins.replaceStrings ["."] ["\\."] cfg.allowedImageRepository}:[A-Za-z0-9._-]+$'; then
      echo "k8s-deploy: refusing '$image' -- it must be ${cfg.allowedImageRepository}:<tag>." >&2
      exit 2
    fi

    export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

    echo "k8s-deploy: rolling ${cfg.deployment} in ${cfg.namespace} onto $image"

    ${pkgs.k3s}/bin/k3s kubectl -n ${cfg.namespace} set image \
      deployment/${cfg.deployment} ${cfg.container}="$image"

    # WAIT, AND LET THE EXIT CODE MEAN SOMETHING. Without this the ssh call returns the moment the
    # Deployment is patched, so CI goes green while the new pod is still pulling -- and an image
    # that crash-loops reports as a successful deploy. The timeout is what turns "never became
    # ready" into a failure rather than a hang.
    ${pkgs.k3s}/bin/k3s kubectl -n ${cfg.namespace} rollout status \
      deployment/${cfg.deployment} --timeout=${cfg.rolloutTimeout}
  '';
in
{
  options.fleet.k8sDeploy = {
    enable = lib.mkEnableOption "a forced-command ssh endpoint that rolls a new image onto one Deployment";

    authorizedKey = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        CI's public key. Empty means the account is not created at all, so a host that has never
        been given one does not carry a dormant deploy user.
      '';
    };

    namespace = lib.mkOption { type = lib.types.str; default = "kinowo"; };
    deployment = lib.mkOption { type = lib.types.str; default = "worker-pl"; };
    container = lib.mkOption { type = lib.types.str; default = "worker"; };

    allowedImageRepository = lib.mkOption {
      type = lib.types.str;
      default = "ghcr.io/pawelkrupinski/movies-worker";
      description = ''
        The one repository this endpoint will deploy from. It is matched anchored, so a key holder
        cannot point the Deployment at an image of their own -- which is the difference between this
        being a deploy button and being remote code execution as whatever the pod runs as.
      '';
    };

    rolloutTimeout = lib.mkOption { type = lib.types.str; default = "10m"; };
  };

  config = lib.mkIf (cfg.enable && cfg.authorizedKey != "") {
    users.groups.k8sdeploy = { };

    users.users.k8sdeploy = {
      isSystemUser = true;
      group = "k8sdeploy";
      home = "/var/lib/k8sdeploy";
      createHome = true;
      shell = pkgs.bashInteractive;

      # `command=` IS THE SECURITY BOUNDARY, and the restrictions after it are what stop the shape
      # being worked around: no port forwarding, no agent forwarding, no pty, no X11. Without
      # `restrict` a forced command still permits a tunnel, which would turn this key into a route
      # onto the private subnet.
      openssh.authorizedKeys.keys = [
        ''command="${endpoint}",restrict ${cfg.authorizedKey}''
      ];
    };

    # READ ACCESS TO THE ADMIN KUBECONFIG, which is the one uncomfortable part of this design and is
    # worth stating rather than hiding. k3s writes a single cluster-admin credential; there is no
    # lesser one to grant without provisioning a ServiceAccount and RBAC, which this fleet does not
    # yet have a mechanism for.
    #
    # What bounds the account is therefore NOT the kubeconfig's permissions but the forced command:
    # the key can run one script, and that script accepts one validated image reference. A holder of
    # the key cannot read the kubeconfig, because reading it requires a shell they cannot get.
    #
    # THE FOLLOW-UP IF THIS EVER MATTERS MORE: a ServiceAccount with a Role limited to `patch` on
    # this one Deployment, and a token file readable by this user instead. Written down because the
    # right time to do it is when a second thing needs deploying, not now.
    systemd.tmpfiles.rules = [
      "z /etc/rancher/k3s/k3s.yaml 0640 root k8sdeploy -"
    ];
  };
}
