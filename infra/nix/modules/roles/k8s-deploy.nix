# ⚠️ BREAK-GLASS ONLY. CI NO LONGER USES THIS.
#
# This was how CI rolled a new image out, and every word below about how it is bounded still holds.
# What changed is that nothing calls it on a push any more: the running image is a fact in git,
# image-automation writes the winning tag into movies-gitops/<tier>/base/all.yaml, and
# kustomize-controller applies it. `K8sTierPathGatingSpec` asserts main.yml holds neither the key
# nor this endpoint's address, because CI reaching for it again would fight Flux for the image
# field -- CI writing the commit SHA tag, Flux writing the automation's, each reverting the other
# every reconcile and rolling every pod in between.
#
# IT IS KEPT ON PURPOSE, for the case where Flux itself is what is broken and an image has to be
# moved by hand. The key still exists as the `K8S_DEPLOY_SSH_KEY` secret; it is simply unused. If
# that case stops being plausible, delete the role, the secret and the account together -- an
# unused credential that still works is worse than either having it or not.
#
# WHY IT IS AN ssh FORCED COMMAND RATHER THAN "give CI a kubeconfig". The problem is the same one
# nix/modules/fleet/deploy-staging.nix solves for closures: a GitHub Actions runner has to be able
# to change production, and anything it holds is one leaked secret away from being an attacker's.
# A kubeconfig for this cluster is cluster-admin -- k3s writes one admin credential and no other --
# so handing CI that would trade a rollout for the whole cluster, including every Secret in it.
#
# So the same shape is used instead: an ssh key pinned to a FORCED COMMAND. The holder cannot get a
# shell, cannot run kubectl, and cannot name a resource. It can send one string -- an image
# reference -- to a script that validates it and updates exactly the containers that reference
# names. That is the entire capability the key grants.
#
# TWO TIERS THROUGH ONE KEY, which is the shape this grew into when the web tier joined the worker
# on the cluster. The image reference itself says which tier it is (`movies-worker` vs
# `movies-web`), so the endpoint looks it up in `targets` rather than taking a second parameter --
# there is nothing for CI to get wrong, no way to ask for a target that is not configured, and no
# second GitHub secret to rotate. An image matching no target is refused, exactly as before.
{ config, lib, pkgs, ... }:

let
  cfg = config.fleet.k8sDeploy;

  # Only `.` is escaped, not `/`. A slash has no meaning in an ERE, and escaping it makes GNU grep
  # warn "stray \ before /" on every single deploy -- noise in the one output an operator reads
  # when a rollout has gone wrong.
  ereEscape = builtins.replaceStrings [ "." ] [ "\\." ];

  rollTarget = t: ''
    if printf '%s' "$image" | ${pkgs.gnugrep}/bin/grep -qE '^${ereEscape t.imageRepository}:[A-Za-z0-9._-]+$'; then
      echo "k8s-deploy: rolling ${lib.concatStringsSep ", " t.deployments} in ${cfg.namespace} onto $image"

      for deployment in ${lib.concatStringsSep " " t.deployments}; do
        ${pkgs.k3s}/bin/k3s kubectl -n ${cfg.namespace} set image \
          "deployment/$deployment" ${t.container}="$image"
      done

      # WAIT ON EACH IN TURN, AND LET THE EXIT CODE MEAN SOMETHING. Without this the ssh call
      # returns the moment the Deployments are patched, so CI goes green while the pods are still
      # pulling -- and an image that crash-loops reports as a successful deploy. `set -e` stops at
      # the first failure, leaving the rest on their previous image rather than rolling a known-bad
      # build out across every country.
      for deployment in ${lib.concatStringsSep " " t.deployments}; do
        ${pkgs.k3s}/bin/k3s kubectl -n ${cfg.namespace} rollout status \
          "deployment/$deployment" --timeout=${cfg.rolloutTimeout}
      done

      exit 0
    fi
  '';

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

    export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

    # VALIDATE BEFORE USING, because this string reaches a command line. Each test is anchored and
    # deliberately narrow: one registry, one repository, and a tag of dotted alphanumerics. A digest
    # or a tag from somewhere else falls through to the refusal below rather than being sanitised --
    # "match a known shape" is a much easier promise to keep than "sanitise an arbitrary string".
    ${lib.concatMapStringsSep "\n" rollTarget cfg.targets}

    echo "k8s-deploy: refusing '$image' -- it must be one of ${
      lib.concatMapStringsSep ", " (t: "${t.imageRepository}:<tag>") cfg.targets
    }." >&2
    exit 2
  '';
in
{
  options.fleet.k8sDeploy = {
    enable = lib.mkEnableOption "a forced-command ssh endpoint that rolls a new image onto its Deployments";

    authorizedKey = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        CI's public key. Empty means the account is not created at all, so a host that has never
        been given one does not carry a dormant deploy user.
      '';
    };

    namespace = lib.mkOption { type = lib.types.str; default = "kinowo"; };

    targets = lib.mkOption {
      description = ''
        The image repositories this endpoint will deploy, and what each rolls.

        THE COUNTRY DEPLOYMENTS OF A TIER RUN THE SAME IMAGE and differ only in which country they
        are configured for, so there is no version of "deploy" that sensibly updates one and leaves
        the others behind on an older build -- hence a list of deployments per target rather than a
        target per deployment. The order is the order they roll in; if one fails the script stops,
        and the rest stay on the previous image, which is a coherent state to be left in.

        A repository not listed here cannot be deployed at all. That anchoring is the difference
        between this being a deploy button and being remote code execution as whatever the pod runs
        as.
      '';
      type = lib.types.listOf (lib.types.submodule {
        options = {
          imageRepository = lib.mkOption { type = lib.types.str; };
          container       = lib.mkOption { type = lib.types.str; };
          deployments     = lib.mkOption { type = lib.types.listOf lib.types.str; };
        };
      });
      default = [
        {
          imageRepository = "ghcr.io/pawelkrupinski/movies-worker";
          container       = "worker";
          deployments     = [ "worker-pl" "worker-de" "worker-uk" "worker-us" "worker-es" ];
        }
        {
          imageRepository = "ghcr.io/pawelkrupinski/movies-web";
          container       = "web";
          deployments     = [ "web-pl" "web-de" "web-uk" "web-us" "web-es" ];
        }
      ];
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
    # these Deployments, and a token file readable by this user instead.
    systemd.tmpfiles.rules = [
      "z /etc/rancher/k3s/k3s.yaml 0640 root k8sdeploy -"
    ];
  };
}
