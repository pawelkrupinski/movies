{ ... }:
{
  # BIOS/GPT, not UEFI -- Hetzner Cloud x86 servers boot legacy BIOS by default, so the layout
  # carries a 1MiB EF02 `bios_boot` partition for GRUB's core image and no ESP. Getting this
  # backwards is the classic nixos-anywhere failure on hcloud: the install completes, the reboot
  # never comes back, and the only way to see why is the Hetzner console.
  #
  # THE WHOLE 160GB DISK IS ONE ROOT FILESYSTEM, and unlike the other two hosts there is no attached
  # volume to keep off it. That is the point of this machine: container images and ephemeral pod
  # storage rebuild themselves, so there is nothing here that survives a re-conversion and nothing
  # that would be worth carving a separate partition for. A workload that needs a persistent disk
  # gets one through the hcloud CSI driver in the cluster, where it can be rescheduled, rather than
  # pinned to this node.
  disko.devices.disk.main = {
    device = "/dev/sda";
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        boot = {
          size = "1M";
          type = "EF02";
        };

        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
          };
        };
      };
    };
  };
}
