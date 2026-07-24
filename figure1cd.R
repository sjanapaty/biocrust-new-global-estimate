# Single panel sharing a log precipitation x-axis.
# Background: global distribution of light available (1 - canopy cover, %)
# and soil water (m^3/m^3, actual units) scattered against precipitation.
# Both are rescaled onto the FIXE log axis for display (light has no
# dedicated axis, reported only via the legend; soil water gets the
# secondary axis in its true units). Soil water points are colored by
# whether their grid cell's soil type is Mineral or Organic (same two blues
# as the Panel B Mineral/Organic violins in figure1ab.R), using the
# mode-classified soiltype.tif raster.
# Foreground: FIXE (N:C ratio) bootstrapped by precipitation bins
# (<100, 100-300, 300-600, 600-1000, >1000 mm/yr), shown as violins +
# boxplots on top of the scatter. FIXE reads off the left (primary) y-axis;
# soil water off the right.

library(readxl)
library(dplyr)
library(ggplot2)
library(terra)
library(gridExtra)

##############################################################################
# Read N:C data
##############################################################################
NvsAZ <- read_excel("Downloads/NvsAZ_v43.xlsx")
CvsAZ <- read_excel("Downloads/CvsAZ_v15.xlsx")

# Remove NAs (need both the fixation value and MAP present)
NvsAZ_clean <- NvsAZ[!is.na(NvsAZ$No_Coverage) & !is.na(NvsAZ$MAP), ]
CvsAZ_clean <- CvsAZ[!is.na(CvsAZ$`Raw No Coverage`) & !is.na(CvsAZ$MAP), ]

##############################################################################
# Load and process raster data (same pipeline as Figure D) to get MAP,
# light available (%), and normalized soil water (%) per grid cell
##############################################################################
coverage_raster <- rast("~/Downloads/cover (1).tif")
coverage_df <- as.data.frame(coverage_raster, xy = TRUE, na.rm = FALSE)
colnames(coverage_df) <- c("x", "y", "coverage_value")
coverage_df$coverage_value <- coverage_df$coverage_value / 100

aridity_raster <- rast("~/Downloads/ai_v3_yr.tif")
aridity_regrid <- aggregate(aridity_raster, fact = 60, fun = "mean", na.rm = TRUE)
aridity_regrid <- aridity_regrid / 10000
aridity_df <- as.data.frame(aridity_regrid, xy = TRUE, na.rm = FALSE)
colnames(aridity_df) <- c("x", "y", "aridity_index")

et0_raster <- rast("~/Downloads/et0_v31_yr.tif")
et0_regrid <- aggregate(et0_raster, fact = 60, fun = "mean", na.rm = TRUE)
et0_df <- as.data.frame(et0_regrid, xy = TRUE, na.rm = FALSE)
colnames(et0_df) <- c("x", "y", "et0")

high_veg_raster <- rast("~/Downloads/high-vegetation-cover.nc")
high_veg_regrid <- resample(high_veg_raster, coverage_raster, method = "bilinear")
high_veg_df <- as.data.frame(high_veg_regrid, xy = TRUE, na.rm = FALSE)
colnames(high_veg_df) <- c("x", "y", "high_veg_cover")

# Soil water raster (already on the same grid as coverage_raster)
soilwater_raster <- rast("~/Downloads/soilwater_avg_1973_2025.tif")
soilwater_df <- as.data.frame(soilwater_raster, xy = TRUE, na.rm = FALSE)
colnames(soilwater_df) <- c("x", "y", "soilwater_raw")

# Soil type raster (mode-classified, same 0.5deg grid as coverage_raster;
# 0 = Sea/Water, 1-5 = Mineral, 6-7 = Organic), used only to color the soil
# water points by Mineral vs Organic
soiltype_raster <- rast("~/Documents/GitHub/biocrust-new-global-estimate/soiltype.tif")
soiltype_df <- as.data.frame(soiltype_raster, xy = TRUE, na.rm = FALSE)
colnames(soiltype_df) <- c("x", "y", "soil_type_class")
soiltype_df$soil_category <- ifelse(soiltype_df$soil_type_class %in% c(6, 7), "Organic",
                              ifelse(soiltype_df$soil_type_class %in% c(1, 2, 3, 4, 5), "Mineral", NA))

coverage_df$x <- round(coverage_df$x, 6);   coverage_df$y <- round(coverage_df$y, 6)
aridity_df$x <- round(aridity_df$x, 6);     aridity_df$y <- round(aridity_df$y, 6)
et0_df$x <- round(et0_df$x, 6);             et0_df$y <- round(et0_df$y, 6)
high_veg_df$x <- round(high_veg_df$x, 6);   high_veg_df$y <- round(high_veg_df$y, 6)
soilwater_df$x <- round(soilwater_df$x, 6); soilwater_df$y <- round(soilwater_df$y, 6)
soiltype_df$x <- round(soiltype_df$x, 6);   soiltype_df$y <- round(soiltype_df$y, 6)

combined_df <- merge(coverage_df, aridity_df, by = c("x", "y"))
combined_df <- merge(combined_df, et0_df, by = c("x", "y"))
combined_df <- merge(combined_df, high_veg_df, by = c("x", "y"))
combined_df <- merge(combined_df, soilwater_df, by = c("x", "y"))
combined_df <- merge(combined_df, soiltype_df, by = c("x", "y"))

combined_df$mean_annual_precip <- combined_df$aridity_index * combined_df$et0
combined_df$canopy_cover <- 1 - combined_df$high_veg_cover
combined_df$light_available_pct <- combined_df$canopy_cover * 100

plot_data_map <- combined_df[!is.na(combined_df$mean_annual_precip) &
                               combined_df$mean_annual_precip > 0 &
                               !is.na(combined_df$light_available_pct) &
                               !is.na(combined_df$soilwater_raw), ]

##############################################################################
# Mean annual temperature raster, for the second (bottom) panel. Already
# regridded to match soilwater_avg_1973_2025.tif exactly, so it merges onto
# the same x/y grid as everything else above.
##############################################################################
mat_raster <- rast("~/Documents/GitHub/biocrust-new-global-estimate/mat_avg_1973_2025.tif")
mat_df <- as.data.frame(mat_raster, xy = TRUE, na.rm = FALSE)
colnames(mat_df) <- c("x", "y", "mat_k")
mat_df$mat_c <- mat_df$mat_k - 273.15
mat_df$x <- round(mat_df$x, 6); mat_df$y <- round(mat_df$y, 6)

combined_df_temp <- merge(combined_df, mat_df, by = c("x", "y"))

plot_data_map_temp <- combined_df_temp[!is.na(combined_df_temp$mat_c) &
                                         !is.na(combined_df_temp$light_available_pct) &
                                         !is.na(combined_df_temp$soilwater_raw), ]

##############################################################################
# Bootstrap function for FIXE (N:C ratio)
##############################################################################
paired_bootstrap_n_c_ratios <- function(n_values, c_values, n_iterations = 10000) {
  ratios <- numeric(n_iterations)
  for (i in 1:n_iterations) {
    n_sample <- sample(n_values, length(n_values), replace = TRUE)
    c_sample <- sample(c_values, length(c_values), replace = TRUE)
    ratios[i] <- mean(n_sample) / mean(c_sample)
  }
  ratios
}

##############################################################################
# Bin by precipitation instead of aridity zone (FIXE bootstrap only)
##############################################################################
precip_breaks <- c(0, 100, 300, 600, 1000, Inf)
precip_labels <- c("<100", "100-300", "300-600", "600-1000", ">1000")

NvsAZ_clean$precip_bin <- cut(NvsAZ_clean$MAP, breaks = precip_breaks, labels = precip_labels, right = FALSE)
CvsAZ_clean$precip_bin <- cut(CvsAZ_clean$MAP, breaks = precip_breaks, labels = precip_labels, right = FALSE)

# Diagnostic: sample size feeding each bin
cat("Sample size per precipitation bin (NvsAZ_clean):\n")
print(table(NvsAZ_clean$precip_bin))

results_fixe_precip <- list()

for (bin in precip_labels) {
  n_values <- NvsAZ_clean$No_Coverage[NvsAZ_clean$precip_bin == bin & !is.na(NvsAZ_clean$precip_bin)]
  c_values <- CvsAZ_clean$`Raw No Coverage`[CvsAZ_clean$precip_bin == bin & !is.na(CvsAZ_clean$precip_bin)]

  if (length(n_values) > 0 & length(c_values) > 0) {
    results_fixe_precip[[bin]] <- paired_bootstrap_n_c_ratios(n_values, c_values, n_iterations = 10000)
  }
}

# x-position for each violin = midpoint of its interval.
# The last bin (">1000") has no upper bound, so it's placed at the mean of
# the observed MAP values that actually fall in that bin (pooled across both
# datasets). Override manually if you'd rather use a fixed anchor.
bin_x_pos <- c(
  "<100"      = mean(c(0, 100)),
  "100-300"   = mean(c(100, 300)),
  "300-600"   = mean(c(300, 600)),
  "600-1000"  = mean(c(600, 1000)),
  ">1000"     = mean(c(NvsAZ_clean$MAP[NvsAZ_clean$precip_bin == ">1000"],
                        CvsAZ_clean$MAP[CvsAZ_clean$precip_bin == ">1000"]), na.rm = TRUE)
)

violin_data_fixe <- data.frame()
for (bin in names(results_fixe_precip)) {
  values <- results_fixe_precip[[bin]]
  violin_data_fixe <- rbind(violin_data_fixe,
                            data.frame(precip_bin = bin,
                                       FIXE = values,
                                       x_pos = bin_x_pos[[bin]]))
}

cat("FIXE bootstrap range per bin (check against nc_log_limits below):\n")
print(tapply(violin_data_fixe$FIXE, violin_data_fixe$precip_bin, range))

##############################################################################
# Bin by mean annual temperature (deg C), same bootstrap approach as the
# precipitation bins above.
##############################################################################
temp_breaks <- c(-20, -10, 0, 10, 20, 30)
temp_labels <- c("-20 to -10", "-10 to 0", "0 to 10", "10 to 20", "20 to 30")

NvsAZ_clean_temp <- NvsAZ[!is.na(NvsAZ$No_Coverage) & !is.na(NvsAZ$`MAT (C)`), ]
CvsAZ_clean_temp <- CvsAZ[!is.na(CvsAZ$`Raw No Coverage`) & !is.na(CvsAZ$`MAT (C)`), ]

NvsAZ_clean_temp$temp_bin <- cut(NvsAZ_clean_temp$`MAT (C)`, breaks = temp_breaks, labels = temp_labels, right = FALSE)
CvsAZ_clean_temp$temp_bin <- cut(CvsAZ_clean_temp$`MAT (C)`, breaks = temp_breaks, labels = temp_labels, right = FALSE)

# Diagnostic: sample size feeding each bin
cat("Sample size per temperature bin (NvsAZ_clean_temp):\n")
print(table(NvsAZ_clean_temp$temp_bin))

results_fixe_temp <- list()

for (bin in temp_labels) {
  n_values <- NvsAZ_clean_temp$No_Coverage[NvsAZ_clean_temp$temp_bin == bin & !is.na(NvsAZ_clean_temp$temp_bin)]
  c_values <- CvsAZ_clean_temp$`Raw No Coverage`[CvsAZ_clean_temp$temp_bin == bin & !is.na(CvsAZ_clean_temp$temp_bin)]

  if (length(n_values) > 0 & length(c_values) > 0) {
    results_fixe_temp[[bin]] <- paired_bootstrap_n_c_ratios(n_values, c_values, n_iterations = 10000)
  }
}

# x-position for each violin = midpoint of its interval. All bins are
# closed on both ends, so midpoints are fixed (no open-ended bin to anchor
# on the observed data like the ">1000" precipitation bin).
bin_x_pos_temp <- c(
  "-20 to -10" = mean(c(-20, -10)),
  "-10 to 0"   = mean(c(-10, 0)),
  "0 to 10"    = mean(c(0, 10)),
  "10 to 20"   = mean(c(10, 20)),
  "20 to 30"   = mean(c(20, 30))
)

violin_data_fixe_temp <- data.frame()
for (bin in names(results_fixe_temp)) {
  values <- results_fixe_temp[[bin]]
  violin_data_fixe_temp <- rbind(violin_data_fixe_temp,
                            data.frame(temp_bin = bin,
                                       FIXE = values,
                                       x_pos = bin_x_pos_temp[[bin]]))
}

cat("FIXE bootstrap range per temperature bin (check against nc_log_limits below):\n")
print(tapply(violin_data_fixe_temp$FIXE, violin_data_fixe_temp$temp_bin, range))

##############################################################################
# Map light available (%) and soil water (m^3/m^3, actual units) onto the
# FIXE log axis so both can sit in the background behind the FIXE violins.
##############################################################################
nc_log_limits <- c(0.00001, 1)

# Light available (0-100%) has no dedicated axis (reported via legend only),
# so it's just linearly interpolated onto the log FIXE range.
scale_light_to_fixe <- function(pct) {
  exp(log(nc_log_limits[1]) + (pct / 100) * (log(nc_log_limits[2]) - log(nc_log_limits[1])))
}

# Soil water gets the secondary axis in its true units. Soil water itself is
# NOT log-distributed, so (like light) it's linearly interpolated over its
# own range and only the *position* on the log FIXE axis is exponential.
soilwater_range <- range(plot_data_map$soilwater_raw, na.rm = TRUE)

scale_soilwater_to_fixe <- function(x) {
  frac <- (x - soilwater_range[1]) / (soilwater_range[2] - soilwater_range[1])
  exp(log(nc_log_limits[1]) + frac * (log(nc_log_limits[2]) - log(nc_log_limits[1])))
}
scale_fixe_to_soilwater <- function(y) {
  frac <- (log(y) - log(nc_log_limits[1])) / (log(nc_log_limits[2]) - log(nc_log_limits[1]))
  soilwater_range[1] + frac * (soilwater_range[2] - soilwater_range[1])
}

plot_data_map$light_scaled_fixe <- scale_light_to_fixe(plot_data_map$light_available_pct)
plot_data_map$soilwater_scaled_fixe <- scale_soilwater_to_fixe(plot_data_map$soilwater_raw)

# Secondary axis breaks/labels spanning the observed soil water range
soilwater_breaks <- pretty(soilwater_range, n = 5)
soilwater_breaks <- soilwater_breaks[soilwater_breaks > 0 &
                                        soilwater_breaks >= soilwater_range[1] &
                                        soilwater_breaks <= soilwater_range[2]]

##############################################################################
# Plot
##############################################################################
common_theme <- theme_classic(base_size = 8) +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(10, 10, 10, 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    aspect.ratio = 0.5
  )

# Single panel: light + soil water scatter in the background, FIXE
# violins/boxplots on top. Soil water points are split into two layers so
# they can be colored by their grid cell's soil type. -----------------------
soilwater_mineral <- plot_data_map[plot_data_map$soil_category == "Mineral" & !is.na(plot_data_map$soil_category), ]
soilwater_organic <- plot_data_map[plot_data_map$soil_category == "Organic" & !is.na(plot_data_map$soil_category), ]

figure_fixe_precip3 <- ggplot() +
  geom_point(data = plot_data_map, aes(x = mean_annual_precip, y = light_scaled_fixe, color = "Light available (%)"),
             alpha = 0.2, size = 0.3) +
  geom_point(data = soilwater_mineral, aes(x = mean_annual_precip, y = soilwater_scaled_fixe, color = "Soil water (Mineral)"),
             alpha = 0.2, size = 0.3) +
  geom_point(data = soilwater_organic, aes(x = mean_annual_precip, y = soilwater_scaled_fixe, color = "Soil water (Organic)"),
             alpha = 0.2, size = 0.3) +
  geom_violin(data = violin_data_fixe, aes(x = x_pos, y = FIXE, group = precip_bin),
              fill = "grey85", color = "black", trim = TRUE, width = 0.3) +
  geom_boxplot(data = violin_data_fixe, aes(x = x_pos, y = FIXE, group = precip_bin),
               width = 0.08, fill = "white", outlier.size = 1.5) +
  scale_color_manual(
    name = "",
    values = c("Light available (%)" = "#8c510a",
               "Soil water (Mineral)" = "#deebf7",
               "Soil water (Organic)" = "#3182bd")
  ) +
  scale_x_log10(
    name = expression("Precipitation (mm yr"^-1*")"),
    breaks = c(10, 100, 1000, 10000),
    labels = expression(10^1, 10^2, 10^3, 10^4)
  ) +
  scale_y_log10(
    name = "Fixation efficiency (FIXE)",
    breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1),
    labels = expression(10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0),
    sec.axis = sec_axis(
      trans = ~ scale_fixe_to_soilwater(.),
      name = expression("Soil water (" * m^3 ~ "water" ~ m^-3 ~ "soil)"),
      breaks = soilwater_breaks,
      labels = soilwater_breaks
    )
  ) +
  # Zoom the *view* to this window without dropping data from the stats
  # (unlike passing limits= to the scales above, which discards points
  # outside the window before geom_violin computes its density estimate).
  coord_cartesian(xlim = c(10, 10000), ylim = nc_log_limits) +
  common_theme +
  theme(legend.position = "bottom")

##############################################################################
# BOTTOM PANEL: same light + soil water background scatter as the top panel,
# rescaled onto the same FIXE log axis, but against Mean Annual Temperature
# (deg C) instead of precipitation. Temperature can go negative, so the
# x-axis stays linear (unlike the log precipitation axis above); the FIXE
# violins/boxplots from the temperature-bin bootstrap sit on top.
##############################################################################
plot_data_map_temp$light_scaled_fixe <- scale_light_to_fixe(plot_data_map_temp$light_available_pct)
plot_data_map_temp$soilwater_scaled_fixe <- scale_soilwater_to_fixe(plot_data_map_temp$soilwater_raw)

soilwater_mineral_temp <- plot_data_map_temp[plot_data_map_temp$soil_category == "Mineral" & !is.na(plot_data_map_temp$soil_category), ]
soilwater_organic_temp <- plot_data_map_temp[plot_data_map_temp$soil_category == "Organic" & !is.na(plot_data_map_temp$soil_category), ]

figure_temp_scatter <- ggplot() +
  geom_point(data = plot_data_map_temp, aes(x = mat_c, y = light_scaled_fixe, color = "Light available (%)"),
             alpha = 0.2, size = 0.3) +
  geom_point(data = soilwater_mineral_temp, aes(x = mat_c, y = soilwater_scaled_fixe, color = "Soil water (Mineral)"),
             alpha = 0.2, size = 0.3) +
  geom_point(data = soilwater_organic_temp, aes(x = mat_c, y = soilwater_scaled_fixe, color = "Soil water (Organic)"),
             alpha = 0.2, size = 0.3) +
  geom_violin(data = violin_data_fixe_temp, aes(x = x_pos, y = FIXE, group = temp_bin),
              fill = "grey85", color = "black", trim = TRUE, width = 3) +
  geom_boxplot(data = violin_data_fixe_temp, aes(x = x_pos, y = FIXE, group = temp_bin),
               width = 0.8, fill = "white", outlier.size = 1.5) +
  scale_color_manual(
    name = "",
    values = c("Light available (%)" = "#8c510a",
               "Soil water (Mineral)" = "#deebf7",
               "Soil water (Organic)" = "#3182bd")
  ) +
  scale_x_continuous(
    name = expression("Mean annual temperature ("*degree*"C)"),
    breaks = seq(-20, 30, by = 10)
  ) +
  scale_y_log10(
    name = "Fixation efficiency (FIXE)",
    breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1),
    labels = expression(10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0),
    sec.axis = sec_axis(
      trans = ~ scale_fixe_to_soilwater(.),
      name = expression("Soil water (" * m^3 ~ "water" ~ m^-3 ~ "soil)"),
      breaks = soilwater_breaks,
      labels = soilwater_breaks
    )
  ) +
  coord_cartesian(xlim = c(-20, 30), ylim = nc_log_limits) +
  common_theme +
  theme(legend.position = "bottom")

##############################################################################
# Combine precipitation (top) and temperature (bottom) panels
##############################################################################
combined_fixe_plot <- grid.arrange(figure_fixe_precip3, figure_temp_scatter, ncol = 1)

ggsave("figure_fixe_precip3.png", combined_fixe_plot, width = 180, height = 220, units = "mm", dpi = 300)
ggsave("figure_fixe_precip3.pdf", combined_fixe_plot, width = 180, height = 220, units = "mm", dpi = 300)
