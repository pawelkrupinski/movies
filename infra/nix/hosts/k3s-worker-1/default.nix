{ ... }:
{
  imports = [
    ./disko.nix
    ../../modules/roles/k3s-agent.nix
  ];

  networking.hostName = "k3s-worker-1";

  fleet = {
    role = "k3s-worker";
    privateAddress = "10.20.0.12";

    # THE ADDRESS CI COPIES A CLOSURE TO, and the reason it is stated rather than inferred: this
    # fleet has no jump host, so every path onto these machines -- colmena, nixos-anywhere, the
    # staging workflow -- arrives on the public NIC. bin/stage-nixos-closures treats an empty
    # `publicAddress` as `unreachable-by-declaration` and FAILS rather than skipping, which is the
    # right direction (a host nobody can stage to silently stops tracking main) and is exactly what
    # it did on the first run after this landed on main.
    #
    # Stable because terraform/primary_ips.tf pins it with `auto_delete = false`; it is
    # `k3s_worker_1_ipv4` there.
    publicAddress = "204.168.140.213";
  };

  # THE ONLY HOST IN hel1, and the only one with no `fileSystems` entry beyond disko's root -- see
  # ./disko.nix for why it holds no state. It is also the only host with Hetzner's daily backups
  # switched OFF (in ../../../terraform/server.k3s-worker.tf), which is the same decision stated in
  # the other direction.

  # JOINS THE CONTROL PLANE ON monitoring-1, over the private network. Both hosts are in Hetzner's
  # `eu-central` network zone despite sitting in different datacentres (hel1 here, nbg1 there), so
  # 10.20.0.11 is directly reachable with no peering and no routes -- see terraform/network.tf. The
  # ~20ms between Helsinki and Nuremberg is paid by kubelet heartbeats and image pulls, which is
  # why nothing latency-sensitive should be scheduled here without moving the machine first.
  fleet.k3sAgent = {
    enable = true;
    serverAddr = "https://10.20.0.11:6443";
  };

  fleet.firewall.k3sAgent = true;

  sops.defaultSopsFile = ../../secrets/k3s-worker-1.yaml;

  system.stateVersion = "26.05";
}
