"""THE SEEDED TEST ORDER the nightly order-independence legs share (ios/scripts/swift_test_shuffled.py,
android/scripts/devtest_shuffled.py).

THE SAME SEED MUST GIVE THE SAME ORDER ON EVERY MACHINE, because the whole point of printing the seed is that a failure
found on a CI runner can be replayed on a laptop. The first version keyed each line with awk's `srand(seed); rand()`,
and awk's generator is the implementation's own: the Android leg ran under Ubuntu's mawk and was replayed under macOS's
BSD awk, which put the same seed's classes in a different order. The key is the POSIX CRC of "<seed>\\n<line>", the
checksum `cksum` prints (the standard fixes it), and lines of equal key fall back to their bytes (the C locale): the
order scripts/seeded-order.sh gave, so a seed printed before the move replays the same order after it."""
from __future__ import annotations

from typing import Iterable, List

_POLYNOMIAL = 0x04C11DB7


def _table() -> List[int]:
    table: List[int] = []
    for byte in range(256):
        crc = byte << 24
        for _ in range(8):
            crc = ((crc << 1) ^ _POLYNOMIAL if crc & 0x80000000 else crc << 1) & 0xFFFFFFFF
        table.append(crc)
    return table


_TABLE = _table()


def posix_cksum(data: bytes) -> int:
    """The CRC `cksum` prints for `data`: CRC-32 MSB first over the bytes, then their length (least significant byte
    first, as few bytes as it takes), complemented."""
    crc = 0
    length = len(data)
    trailer = bytearray()
    while length:
        trailer.append(length & 0xFF)
        length >>= 8
    for byte in bytes(data) + bytes(trailer):
        crc = ((crc << 8) & 0xFFFFFFFF) ^ _TABLE[(crc >> 24) ^ byte]
    return ~crc & 0xFFFFFFFF


def seeded_order(seed: str, lines: Iterable[str]) -> List[str]:
    """`lines` (blank ones dropped) in the order `seed` names."""
    kept = [line for line in lines if line]
    return sorted(kept, key=lambda line: (posix_cksum(f"{seed}\n{line}".encode()), line.encode()))
