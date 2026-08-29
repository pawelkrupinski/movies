{ ... }:
{
  # BIOS/GPT, not UEFI. Hetzner Cloud x86 servers boot legacy BIOS by default, so the layout carries
  # a 1MiB EF02 `bios_boot` partition for GRUB's core image and no ESP at all. Getting this backwards
  # is the classic nixos-anywhere failure on hcloud: the install completes, the reboot never
  # comes back, and the only way to see why is the Hetzner console.
  #
  # /dev/sda is the ROOT disk. The database lives on an attached Hetzner Volume, which is a
  # different device entirely and is NOT declared here -- disko owns disks it may repartition, and
  # the volume is the one device on this machine that must never be repartitioned. Terraform formats
  # it once (`format = "ext4"` in modules/server/volumes.tf) and default.nix mounts it.
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
