# Extract mean annual temperature (mat_avg_1973_2025.tif) at the lat/long
# pairs in NvsAZ_v42 and CvsAZ_v14, writing the temperature column to
# temp_N.xlsx and temp_C.xlsx respectively.

library(readxl)
library(writexl)
library(terra)

mat_raster <- rast("mat_avg_1973_2025.tif")

##############################################################################
# N (NvsAZ_v42): separate Lat / Long columns
##############################################################################
NvsAZ <- read_excel("~/Downloads/NvsAZ_v42.xlsx")

n_pts <- cbind(NvsAZ$Long, NvsAZ$Lat)
n_mat_k <- extract(mat_raster, n_pts)[, 1]
n_mat_c <- n_mat_k - 273.15

temp_N <- data.frame(Lat = NvsAZ$Lat, Long = NvsAZ$Long,
                      mat_k = n_mat_k, mat_c = n_mat_c)
write_xlsx(temp_N, "temp_N.xlsx")

##############################################################################
# C (CvsAZ_v14): combined "Lat-Long" column, "lat, long" format
##############################################################################
CvsAZ <- read_excel("~/Downloads/CvsAZ_v14.xlsx")

latlong_split <- strsplit(trimws(CvsAZ$`Lat-Long`), ",\\s*")
c_lat <- as.numeric(sapply(latlong_split, `[`, 1))
c_long <- as.numeric(sapply(latlong_split, `[`, 2))

c_pts <- cbind(c_long, c_lat)
c_mat_k <- extract(mat_raster, c_pts)[, 1]
c_mat_c <- c_mat_k - 273.15

temp_C <- data.frame(Lat = c_lat, Long = c_long,
                      mat_k = c_mat_k, mat_c = c_mat_c)
write_xlsx(temp_C, "temp_C.xlsx")

cat("NA count in temp_N mat_c:", sum(is.na(temp_N$mat_c)), "of", nrow(temp_N), "\n")
cat("NA count in temp_C mat_c:", sum(is.na(temp_C$mat_c)), "of", nrow(temp_C), "\n")
