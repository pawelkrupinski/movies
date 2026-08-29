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
