{ ... }:

let
  # Allocated by Terraform; read it back with
  # `terraform -chdir=infra/terraform output -json hosts | jq -r '."monitoring-1".volumes."monitoring-data"'`.
  # See the long note in ../mongo-1/default.nix for why a wrong id here is worse than a build
  # failure -- the same reasoning applies, with Prometheus filling the root disk in mongod's place.
  volumeDevice = "/dev/disk/by-id/scsi-0HC_Volume_106735107";
in
{
  imports = [
    ./disko.nix
    ../../modules/roles/prometheus.nix
    ../../modules/roles/grafana.nix
    ../../modules/roles/k3s-server.nix
  ];

  networking.hostName = "monitoring-1";

  fleet = {
    role = "monitoring";
    privateAddress = "10.20.0.11";

    # THE ADDRESS CI COPIES A CLOSURE TO, and the reason it is stated rather than inferred: this
    # fleet has no jump host, so every path onto these machines -- colmena, nixos-anywhere, the
    # staging workflow -- arrives on the public NIC. bin/stage-nixos-closures treats an empty
    # `publicAddress` as `unreachable-by-declaration` and FAILS rather than skipping, which is the
    # right direction (a host nobody can stage to silently stops tracking main) and is exactly what
    # it did on the first run after this landed on main.
    #
    # Stable because terraform/primary_ips.tf pins it with `auto_delete = false`; it is
    # `monitoring_1_ipv4` there.
    publicAddress = "2.28.52.210";
  };

  # No `nofail`, for the reason in the assertion above: a monitoring host that boots without its
  # metrics disk is worse than one that does not boot, because it looks fine.
  fileSystems."/var/lib/monitoring" = {
    device = volumeDevice;
    fsType = "ext4";
    options = [ "defaults" ];
  };

  # PROMETHEUS AND ALERTMANAGER. `nodeTargets` is written out rather than derived from the Hetzner
  # API on purpose: a scrape list that discovers its own targets cannot tell "this host was
  # decommissioned" from "this host is unreachable", and the second is the case monitoring exists
  # for. A host that disappears from this list does so because somebody edited it.
  fleet.prometheus = {
    enable = true;
    nodeTargets = [
      { address = "10.20.0.10"; host = "mongo-1"; role = "mongodb"; }
      { address = "10.20.0.11"; host = "monitoring-1"; role = "monitoring"; }
      { address = "10.20.0.12"; host = "k3s-worker-1"; role = "k3s-worker"; }
    ];
  };

  fleet.grafana.enable = true;

  # THE k3s CONTROL PLANE, ON THE SAME BOX, per the fleet design. `clusterInit` because this is the
  # first and only server; a second would join against it rather than repeat this.
  #
  # `schedulable = false` so the control plane does not also run workloads. On a 2-core box shared
  # with Prometheus and Grafana that is not a stylistic preference: a pod that pins both cores would
  # otherwise starve the apiserver AND the metrics that would explain why. k3s-worker-1 is a cx43
  # with eight cores and nothing else on it, and is where work belongs.
  fleet.k3sServer = {
    enable = true;
    clusterInit = true;
    schedulable = false;
  };

  fleet.firewall.monitoring = true;
  fleet.firewall.k3sServer = true;

  sops.defaultSopsFile = ../../secrets/monitoring-1.yaml;

  system.stateVersion = "26.05";
}
