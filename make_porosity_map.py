# Build a global porosity (theta_sat) map from the ECMWF soil type grid,
# resampled to match soilwater_avg_1973_2025.tif.
#
# Porosity values from Balsamo et al. (2009), as tabulated in
# IFS Documentation Cy41r2, Part IV: Physical Processes, Table 8.9.
# Soil type 7 (tropical organic, added in a later IFS cycle) is merged
# into class 6 (organic) since it covers a small fraction of the globe.

import numpy as np
import h5py
import rasterio
from rasterio.warp import reproject, Resampling
from affine import Affine

SLT_PATH = "/Users/shlokavjanapaty/Downloads/e8603df7a57a3cf7ddd6b22891624e41.nc"
TARGET_PATH = "/Users/shlokavjanapaty/Downloads/soilwater_avg_1973_2025.tif"
OUT_PATH = "/Users/shlokavjanapaty/Downloads/porosity.tif"

POROSITY = {
    1: 0.403,  # Coarse
    2: 0.439,  # Medium
    3: 0.430,  # Medium Fine
    4: 0.520,  # Fine
    5: 0.614,  # Very Fine
    6: 0.766,  # Organic (includes Tropical Organic, code 7)
    7: 0.766,  # Tropical Organic -> merged into Organic
}

# --- Load soil type grid ---
with h5py.File(SLT_PATH, "r") as f:
    slt = np.round(f["slt"][0]).astype(np.uint8)  # (1801, 3600), lon 0..359.9, lat 90..-90
    lon = f["longitude"][:]
    lat = f["latitude"][:]

# --- Reorder longitude from 0/360 to -180/180 ---
lon_shifted = ((lon + 180) % 360) - 180
order = np.argsort(lon_shifted)
slt = slt[:, order]

src_transform = Affine(0.1, 0.0, -180.0, 0.0, -0.1, 90.0)
src_crs = "EPSG:4326"

# --- Load target grid definition ---
with rasterio.open(TARGET_PATH) as tgt:
    dst_transform = tgt.transform
    dst_crs = tgt.crs
    dst_shape = (tgt.height, tgt.width)

# --- Resample soil type (categorical) using mode, excluding ocean (0) ---
soil_resampled = np.zeros(dst_shape, dtype=np.uint8)
reproject(
    source=slt,
    destination=soil_resampled,
    src_transform=src_transform,
    src_crs=src_crs,
    src_nodata=0,
    dst_transform=dst_transform,
    dst_crs=dst_crs,
    dst_nodata=0,
    resampling=Resampling.mode,
)

# --- Map soil type codes to porosity ---
porosity_map = np.zeros(8, dtype=np.float32)
for code, val in POROSITY.items():
    porosity_map[code] = val

porosity = porosity_map[soil_resampled]
porosity[soil_resampled == 0] = np.nan

# --- Write output GeoTIFF, matching target grid exactly ---
profile = {
    "driver": "GTiff",
    "height": dst_shape[0],
    "width": dst_shape[1],
    "count": 1,
    "dtype": "float32",
    "crs": dst_crs,
    "transform": dst_transform,
    "nodata": np.nan,
    "compress": "lzw",
}

with rasterio.open(OUT_PATH, "w", **profile) as dst:
    dst.write(porosity.astype(np.float32), 1)

print(f"Wrote {OUT_PATH}")
print("Shape:", porosity.shape)
print("Valid (land) cells:", np.sum(~np.isnan(porosity)))
print("Porosity range:", np.nanmin(porosity), "-", np.nanmax(porosity))
