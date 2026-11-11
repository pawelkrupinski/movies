#!/usr/bin/env python3
"""Production's poster hash, bit for bit: the 63-bit pHash `services.identity.PosterHash.of` files for a poster.

The worker cuts a poster to the card's 2:3 slot with `vips thumbnail <file> <out> 420 --height 630 --crop centre`
(`VipsPosterShrinker`), composites any transparency over the card's background (`OgCardRenderer.coverSlot`), and hashes
the slot: grey by luma, area-averaged to 32x32, the signs of the lowest 8x8 DCT terms (DC dropped) against their median.
This module runs the same vips command and the same arithmetic (numpy), so a hash here equals the one prod filed
(scripts/poster_eval_test.py checks it against stored hashes). A pure PIL resize matches only ~82% of them.

    hash_file(path) -> int | None      # None: vips could not read it (prod files such a poster unread)
"""
import os
import struct
import subprocess
import tempfile

import numpy as np

SlotWidth, SlotHeight = 420, 630
Side, Low = 32, 8
Background = (0x14, 0x17, 0x1F)  # OgCardRenderer.Bg: what a transparent poster is composited over


def _basis():
    k = np.arange(Side)[:, None]
    i = np.arange(Side)[None, :]
    c = np.cos(np.pi * k * (2 * i + 1) / (2.0 * Side)) * np.sqrt(2.0 / Side)
    c[0] = np.sqrt(1.0 / Side)
    return c


Basis = _basis()


def _spans(n):
    """The area-average matrix taking `n` samples to Side: each target cell the mean of the source span it covers."""
    weights = np.zeros((Side, n))
    for t in range(Side):
        start, end = t * n / Side, (t + 1) * n / Side
        for i in range(int(start), min(n, int(np.ceil(end)))):
            weights[t, i] = max(0.0, min(end, i + 1.0) - max(start, float(i)))
    return weights / weights.sum(1, keepdims=True)


def of_grey(grey):
    """The hash of a grey image (h x w floats): production's `PosterHash.of` after its luma step."""
    h, w = grey.shape
    if w > h:  # a landscape image centre-cut to a 2:3 portrait (a slot never is one; kept for parity)
        cut = max(1, h * 2 // 3)
        x0 = (w - cut) // 2
        grey, w = grey[:, x0:x0 + cut], cut
    small = _spans(h) @ grey @ _spans(w).T
    low = (Basis[:Low] @ small @ Basis[:Low].T).flatten()[1:]
    median = np.sort(low)[len(low) // 2]
    return int(sum(1 << i for i, v in enumerate(low) if v > median))


def read_vips(path):
    """The pixels of a vips native (.v) file as an h x w x bands uint8/uint16 array."""
    with open(path, "rb") as f:
        data = f.read()
    endian = "<" if data[:4] == b"\xb6\xa6\xf2\x08" else ">"
    width, height, bands, _, band_format = struct.unpack(endian + "5i", data[4:24])
    dtype = {0: np.uint8, 2: np.uint16}.get(band_format)
    if dtype is None:
        raise ValueError(f"unsupported vips band format {band_format}")
    count = width * height * bands
    pixels = np.frombuffer(data, dtype=np.dtype(dtype).newbyteorder(endian), count=count, offset=64)
    return pixels.reshape(height, width, bands)


def slot_rgb(pixels):
    """The slot as opaque 8-bit RGB floats, as the worker's `ImageIO.read` + `coverSlot` see it."""
    a = pixels.astype(np.float64)
    if pixels.dtype == np.uint16:
        a = np.round(a * 255.0 / 65535.0)
    bands = a.shape[2]
    if bands in (2, 4):  # transparency over the card's background
        alpha = a[..., -1:] / 255.0
        colour = a[..., :-1]
        if bands == 2:
            colour = np.repeat(colour, 3, axis=2)
        a = np.round(colour * alpha + np.array(Background, dtype=np.float64) * (1.0 - alpha))
    elif bands == 1:
        a = np.repeat(a, 3, axis=2)
    return a[..., :3]


def hash_slot(pixels):
    rgb = slot_rgb(pixels)
    return of_grey(0.299 * rgb[..., 0] + 0.587 * rgb[..., 1] + 0.114 * rgb[..., 2])


def hash_file(path, vips="vips"):
    """`path`'s hash as the worker files it, or None when vips cannot read it."""
    fd, out = tempfile.mkstemp(suffix=".v")
    os.close(fd)
    try:
        done = subprocess.run([vips, "thumbnail", path, out, str(SlotWidth), "--height", str(SlotHeight), "--crop", "centre"],
                              capture_output=True, env=dict(os.environ, VIPS_CONCURRENCY="1"), timeout=60)
        if done.returncode != 0:
            return None
        return hash_slot(read_vips(out))
    except (OSError, ValueError, subprocess.TimeoutExpired):
        return None
    finally:
        os.remove(out)


def distance(a, b):
    return bin(a ^ b).count("1")
