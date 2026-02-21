#!/usr/bin/env python3
"""Generate binary input files for the rotating_convection experiment.

Files created in experiments/rotating_convection/input/:
  bathy.bin     - parabolic depth, circular tank mask (land outside cylinder)
  init_T.bin    - linear temperature profile (20 degC surface, 0 degC at H_max)
  rbcs_T.bin    - RBCS target temperature (0 degC everywhere)
  rbcs_mask.bin - RBCS relaxation rate (1/tau at bottom wet cell of each column)

MITgcm binary convention:
  Big-endian float64 (readBinaryPrec=64).
  2D field (Nx, Ny): C-order shape (Ny, Nx), x varies fastest.
  3D field (Nx, Ny, Nz): C-order shape (Nz, Ny, Nx), level 0 at surface.
  Bathymetry: negative value = depth below surface; 0 = land.
"""
import math
import pathlib

import numpy as np

# ── Grid (must match SIZE.h and data) ─────────────────────────────────────────
NX = 60
NY = 60
NZ = 40
LX = 1.0       # domain width  [m]
LY = 1.0       # domain height [m]
DZ = 0.003     # vertical cell thickness [m]  (NZ * DZ = 0.12 m)

# ── Tank geometry ──────────────────────────────────────────────────────────────
R        = 0.5    # tank radius [m]
H_CENTRE = 0.01   # water depth at centre [m]
H_EDGE   = 0.12   # water depth at edge   [m]

# ── Temperature ────────────────────────────────────────────────────────────────
T_SURFACE    = 20.0   # degC at z = 0
T_BOTTOM     = 0.0    # degC at z = -H_EDGE (also the heat-bath temperature)
# RBCS: mask contains dimensionless 0-1 weight; tauRelaxT in data.rbcs is the timescale.
# mask = 1.0 at bottom cell, 0.0 elsewhere -> full restoring at bottom only.
RBCS_MASK_BOTTOM = 1.0    # mask weight at bottom cell (1 = full restoring at tauRelaxT)

OUTPUT_DIR = (
    pathlib.Path(__file__).resolve().parent.parent
    / "experiments" / "rotating_convection" / "input"
)


def cell_centres_xy():
    """Cell-centre coordinates (xx, yy) relative to tank centre, shape (NY, NX)."""
    dx = LX / NX
    dy = LY / NY
    x = np.arange(NX) * dx + dx / 2 - LX / 2   # centred on 0
    y = np.arange(NY) * dy + dy / 2 - LY / 2
    xx, yy = np.meshgrid(x, y)                  # shape (NY, NX)
    return xx, yy


def make_bathy(xx, yy):
    """Parabolic depth inside cylinder; 0 (land) outside.

    h(r) = H_CENTRE + (H_EDGE - H_CENTRE) * (r/R)^2
    MITgcm sign convention: negative value = depth.
    """
    r = np.sqrt(xx**2 + yy**2)
    h = H_CENTRE + (H_EDGE - H_CENTRE) * (r / R) ** 2
    bathy = np.where(r <= R, -h, 0.0)           # shape (NY, NX)
    return bathy.astype(">f8")


def make_init_T(bathy):
    """Linear temperature profile: T_SURFACE at z=0, T_BOTTOM at z=-H_EDGE.

    The same 1-D profile is applied everywhere; cells below the local
    bathymetry are masked by MITgcm and their value is irrelevant.
    """
    T = np.zeros((NZ, NY, NX), dtype=">f8")
    for k in range(NZ):
        z_centre = -(k + 0.5) * DZ            # negative metres
        frac = -z_centre / H_EDGE             # 0 at surface, 1 at H_EDGE
        T[k, :, :] = T_SURFACE + (T_BOTTOM - T_SURFACE) * frac
    return T


def make_rbcs_target():
    """Target temperature for RBCS restoring: 0 degC everywhere."""
    return np.zeros((NZ, NY, NX), dtype=">f8")


def make_rbcs_mask(bathy):
    """1/tau [s^-1] at the bottom wet cell of each column, 0 elsewhere.

    The bottom wet cell k at column (j, i) satisfies:
        top_of_cell  = k * DZ  < depth      (cell contains water)
        bottom_of_cell = (k+1)*DZ >= depth  (or k == NZ-1)
    i.e. k = floor(depth / DZ), clamped to [0, NZ-1].
    """
    mask = np.zeros((NZ, NY, NX), dtype=">f8")
    for j in range(NY):
        for i in range(NX):
            depth = -bathy[j, i]     # positive depth [m]; 0 = land
            if depth <= 0.0:
                continue
            k_bot = min(NZ - 1, int(depth / DZ))
            # Edge case: if the cell top is exactly at depth, step back one.
            if k_bot * DZ >= depth and k_bot > 0:
                k_bot -= 1
            mask[k_bot, j, i] = RBCS_MASK_BOTTOM
    return mask


def save(path, arr):
    arr.tofile(path)
    print(f"  {path.name:20s}  {arr.nbytes // 1024:5d} KB  shape {arr.shape}")


def main():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    print(f"Writing to {OUTPUT_DIR}\n")

    xx, yy = cell_centres_xy()

    print("bathy.bin")
    bathy = make_bathy(xx, yy)
    save(OUTPUT_DIR / "bathy.bin", bathy)

    print("init_T.bin")
    init_T = make_init_T(bathy)
    save(OUTPUT_DIR / "init_T.bin", init_T)

    print("rbcs_T.bin")
    rbcs_T = make_rbcs_target()
    save(OUTPUT_DIR / "rbcs_T.bin", rbcs_T)

    print("rbcs_mask.bin")
    rbcs_mask = make_rbcs_mask(bathy)
    save(OUTPUT_DIR / "rbcs_mask.bin", rbcs_mask)

    # ── Diagnostics ────────────────────────────────────────────────────────────
    print()
    wet_cols = (bathy < 0).sum()
    total_cols = NX * NY
    centre_depth = -bathy[NY // 2, NX // 2]
    edge_depth = -bathy[NY // 2, NX - 1]
    bottom_cells = int((rbcs_mask > 0).sum())

    print(f"Wet surface cells   : {wet_cols}/{total_cols} "
          f"({100 * wet_cols / total_cols:.0f}%)")
    print(f"Centre depth        : {centre_depth * 100:.2f} cm "
          f"(target {H_CENTRE * 100:.1f} cm)")
    print(f"Edge depth          : {edge_depth * 100:.2f} cm "
          f"(target {H_EDGE * 100:.1f} cm)")
    print(f"RBCS bottom cells   : {bottom_cells}")


if __name__ == "__main__":
    main()
