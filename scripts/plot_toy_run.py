"""Plot SST and SSH from FESOM2 toy experiment output.

Usage:
    pixi run python scripts/plot_toy_run.py

Reads neverworld2 and soufflet 5-day runs from /tmp/fesom2-*/,
reads mesh geometry from FESOM2/test/meshes/, and writes
data/fesom2_toy_run.png.
"""

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.tri as mtri
import netCDF4 as nc
from pathlib import Path


def read_mesh(mesh_dir):
    """Read nod2d.out and elem2d.out, return (lon, lat, triangles)."""
    mesh_dir = Path(mesh_dir)
    # nod2d.out: first line = node count, then "index lon lat boundary_flag"
    nod = np.loadtxt(mesh_dir / "nod2d.out", skiprows=1)
    lon, lat = nod[:, 1], nod[:, 2]
    # elem2d.out: first line = elem count, then "n1 n2 n3" (1-indexed)
    tri = np.loadtxt(mesh_dir / "elem2d.out", dtype=int, skiprows=1) - 1
    return lon, lat, tri


def read_var(nc_path, varname, tidx=-1):
    """Read a single time slice from a FESOM2 output file."""
    with nc.Dataset(nc_path) as ds:
        return ds[varname][tidx, :]


# ── locate output files ──────────────────────────────────────────────────────

nw2_dir   = Path("/tmp/fesom2-neverworld2")
souf_dir  = Path("/tmp/fesom2-soufflet")
mesh_nw2  = Path("FESOM2/test/meshes/neverworld2")
mesh_souf = Path("FESOM2/test/meshes/soufflet")

nw2_sst  = sorted(nw2_dir.glob("sst.fesom.*.nc"))[0]
nw2_ssh  = sorted(nw2_dir.glob("ssh.fesom.*.nc"))[0]
souf_sst = sorted(souf_dir.glob("sst.fesom.*.nc"))[0]
souf_ssh = sorted(souf_dir.glob("ssh.fesom.*.nc"))[0]

# ── read meshes and last time slice ─────────────────────────────────────────

lon_nw2,  lat_nw2,  tri_nw2  = read_mesh(mesh_nw2)
lon_souf, lat_souf, tri_souf = read_mesh(mesh_souf)

nw2_sst_data  = read_var(nw2_sst,  "sst")
nw2_ssh_data  = read_var(nw2_ssh,  "ssh")
souf_sst_data = read_var(souf_sst, "sst")
souf_ssh_data = read_var(souf_ssh, "ssh")

triang_nw2  = mtri.Triangulation(lon_nw2,  lat_nw2,  tri_nw2)
triang_souf = mtri.Triangulation(lon_souf, lat_souf, tri_souf)

# ── plot ─────────────────────────────────────────────────────────────────────

fig, axes = plt.subplots(2, 2, figsize=(12, 8))
fig.suptitle("FESOM2 toy experiments — day 5", fontsize=13)

panels = [
    (axes[0, 0], triang_nw2,  nw2_sst_data,  "neverworld2  SST (°C)",  "RdYlBu_r"),
    (axes[0, 1], triang_souf, souf_sst_data, "soufflet  SST (°C)",     "RdYlBu_r"),
    (axes[1, 0], triang_nw2,  nw2_ssh_data,  "neverworld2  SSH (m)",   "RdBu_r"),
    (axes[1, 1], triang_souf, souf_ssh_data, "soufflet  SSH (m)",      "RdBu_r"),
]

for ax, triang, data, title, cmap in panels:
    vmax = np.percentile(np.abs(data), 99)
    vmin = -vmax if "SSH" in title else (np.percentile(data, 1))
    vmax_plot = vmax if "SSH" in title else np.percentile(data, 99)
    tcf = ax.tripcolor(triang, data, cmap=cmap, vmin=vmin, vmax=vmax_plot,
                       shading="gouraud")
    plt.colorbar(tcf, ax=ax, shrink=0.85)
    ax.set_title(title, fontsize=10)
    ax.set_xlabel("longitude (°)")
    ax.set_ylabel("latitude (°)")

plt.tight_layout()
out = Path("data/fesom2_toy_run.png")
out.parent.mkdir(exist_ok=True)
plt.savefig(out, dpi=150)
print(f"Saved {out}")
