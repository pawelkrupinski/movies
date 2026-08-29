{ config, lib, ... }:

let
  # See the long note in ../mongo-1/default.nix: this is allocated by Terraform and cannot be known
  # until it has run. `terraform output -json hosts | jq -r '."monitoring-1".volumes."monitoring-data"'`.
  volumeDevice = "/dev/disk/by-id/scsi-0HC_Volume_REPLACE_AFTER_FIRST_TERRAFORM_APPLY";
in
{
  imports = [
    ./disko.nix
    ../../modules/roles/prometheus.nix
    ../../modules/roles/grafana.nix
    ../../modules/roles/k3s-server.nix
  ];

  assertions = [
    {
      assertion = !lib.hasInfix "REPLACE_AFTER_FIRST_TERRAFORM_APPLY" volumeDevice;
      message = ''
        monitoring-1: the Hetzner volume id has not been filled in. Run

            terraform -chdir=infra/terraform output -json hosts \
              | jq -r '."monitoring-1".volumes."monitoring-data"'

        and substitute it into `volumeDevice` in infra/nix/hosts/monitoring-1/default.nix.

        Failing at eval is deliberate. If the mount is missing, Prometheus starts anyway and fills
        the 40GB ROOT disk with the TSDB -- and the point at which that becomes visible is the point
        at which the host stops being able to record the incident it is filling up during.
      '';
    }
  ];

  networking.hostName = "monitoring-1";

  fleet = {
    role = "monitoring";
    privateAddress = "10.20.0.11";
  };

  # No `nofail`, for the reason in the assertion above: a monitoring host that boots without its
  # metrics disk is worse than one that does not boot, because it looks fine.
  fileSystems."/var/lib/monitoring" = {
    device = volumeDevice;
    fsType = "ext4";
    options = [ "defaults" ];
  };

  sops.defaultSopsFile = ../../secrets/monitoring-1.yaml;

  system.stateVersion = "26.05";
}
