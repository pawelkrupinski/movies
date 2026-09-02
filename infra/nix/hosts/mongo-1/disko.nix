{ ... }:
{
  # BIOS/GPT, not UEFI. Hetzner Cloud x86 servers boot legacy BIOS by default, so the layout carries
  # a 1MiB EF02 `bios_boot` partition for GRUB's core image and no ESP at all. Getting this backwards
  # is the classic nixos-anywhere failure on hcloud: the install completes, the reboot never
  # comes back, and the only way to see why is the Hetzner console.
  #
  # The device above is the ROOT disk. The database lives on an attached Hetzner Volume, which is a
  # different device entirely and is NOT declared here -- disko owns disks it may repartition, and
  # the volume is the one device on this machine that must never be repartitioned. Terraform formats
  # it once (`format = "ext4"` in modules/server/volumes.tf) and default.nix mounts it.
  # THE ROOT DISK BY STABLE ID, NOT /dev/sda, AND THIS IS THE MOST IMPORTANT LINE IN THE FILE.
  #
  # `/dev/sda` IS NOT STABLE ON A SERVER WITH A VOLUME ATTACHED. Kernel device names are assigned in
  # whatever order the SCSI devices enumerate, and an attached Hetzner Volume competes for `sda`
  # with the root disk. On mongo-1, 2026-08-29, it won: disko partitioned and formatted the 10GB
  # VOLUME -- the one meant to hold the database -- and left the 38GB root disk unformatted. The
  # machine then had nothing to boot, came up with no network, and had to be recovered through the
  # rescue system. Had that volume held data, `disko` would have destroyed it, because disko's whole
  # job is to make the disk it is pointed at match the declaration.
  #
  # THE OTHER TWO HOSTS SURVIVED THE SAME CONFIGURATION BY LUCK, which is the worst property this
  # could have had: monitoring-1 has a volume too and enumerated the other way round, so the bug was
  # invisible on two of three machines and appeared only on a re-run.
  #
  # `scsi-0QEMU_QEMU_HARDDISK_<serial>` is the root disk's own identity and cannot be taken by
  # anything else -- a Hetzner Volume always presents as `scsi-0HC_Volume_<id>`, so the two can
  # never be confused. The serial is per-machine, which is why this is a per-host literal rather
  # than a shared default; read it with `lsblk -dno NAME,SIZE,SERIAL`. A wrong one here does not
  # eat the wrong disk, it fails to find any, which is the failure direction to want.
  #
  # convert-host refuses to install if this path is missing on the target or resolves to a volume.
  disko.devices.disk.main = {
    device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_126555739";
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
