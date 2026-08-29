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

  sops.defaultSopsFile = ../../secrets/k3s-worker-1.yaml;

  system.stateVersion = "26.05";
}
