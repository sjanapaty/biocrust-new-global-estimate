# Combined spinoff: two stacked panels sharing a log precipitation x-axis.
# Panel 1 (top): FIXE (N:C ratio) bootstrapped by precipitation bins
# (<100, 100-300, 300-600, 600-1000, >1000 mm/yr), shown as violins, plus the
# raw (non-bootstrapped) N fixation values scattered on their own log-mapped
# y-axis. FIXE reads off the left y-axis; raw N fixation off the right.
# Panel 2 (bottom): global distribution of light available (1 - canopy
# cover, %) and soil water (m^3/m^3, actual units) scattered against
# precipitation. Light available reads off the left y-axis; soil water off
# the right (soil water is linearly mapped onto the 0-100 range for display,
# then mapped back to its true units for the secondary axis).

library(readxl)
library(dplyr)
library(ggplot2)
library(terra)
library(patchwork)

##############################################################################
# Read N:C data
##############################################################################
NvsAZ <- read_excel("Downloads/NvsAZ_v37.xlsx")
CvsAZ <- read_excel("Downloads/CvsAZ_v9.xlsx")

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

coverage_df$x <- round(coverage_df$x, 6);   coverage_df$y <- round(coverage_df$y, 6)
aridity_df$x <- round(aridity_df$x, 6);     aridity_df$y <- round(aridity_df$y, 6)
et0_df$x <- round(et0_df$x, 6);             et0_df$y <- round(et0_df$y, 6)
high_veg_df$x <- round(high_veg_df$x, 6);   high_veg_df$y <- round(high_veg_df$y, 6)
soilwater_df$x <- round(soilwater_df$x, 6); soilwater_df$y <- round(soilwater_df$y, 6)

combined_df <- merge(coverage_df, aridity_df, by = c("x", "y"))
combined_df <- merge(combined_df, et0_df, by = c("x", "y"))
combined_df <- merge(combined_df, high_veg_df, by = c("x", "y"))
combined_df <- merge(combined_df, soilwater_df, by = c("x", "y"))

combined_df$mean_annual_precip <- combined_df$aridity_index * combined_df$et0
combined_df$canopy_cover <- 1 - combined_df$high_veg_cover
combined_df$light_available_pct <- combined_df$canopy_cover * 100

plot_data_map <- combined_df[!is.na(combined_df$mean_annual_precip) &
                               combined_df$mean_annual_precip > 0 &
                               !is.na(combined_df$light_available_pct) &
                               !is.na(combined_df$soilwater_raw), ]

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
# Map raw N fixation onto the FIXE log scale so it can share panel 1
##############################################################################
nc_log_limits <- c(0.00001, 1)
rawN_range <- range(NvsAZ_clean$No_Coverage, na.rm = TRUE)

scale_rawN_to_log <- function(x) {
  exp(log(nc_log_limits[1]) +
        (log(x) - log(rawN_range[1])) / (log(rawN_range[2]) - log(rawN_range[1])) *
        (log(nc_log_limits[2]) - log(nc_log_limits[1])))
}
scale_log_to_rawN <- function(y) {
  exp(log(rawN_range[1]) +
        (log(y) - log(nc_log_limits[1])) / (log(nc_log_limits[2]) - log(nc_log_limits[1])) *
        (log(rawN_range[2]) - log(rawN_range[1])))
}

# Raw (non-bootstrapped) N fixation observations, scattered against their
# own precipitation values
rawN_points <- NvsAZ_clean[, c("MAP", "No_Coverage")]
rawN_points$raw_N_scaled <- scale_rawN_to_log(rawN_points$No_Coverage)

# Secondary axis breaks in 10^x format, spanning the observed raw-N range
raw_exponents <- floor(log10(rawN_range[1])):ceiling(log10(rawN_range[2]))
raw_breaks <- 10^raw_exponents
raw_labels <- parse(text = paste0("10^", raw_exponents))

##############################################################################
# Map soil water (m^3/m^3, actual units) onto the 0-100 (%) scale so it can
# share panel 2 with light available (%)
##############################################################################
soilwater_range <- range(plot_data_map$soilwater_raw, na.rm = TRUE)

scale_soilwater_to_pct <- function(x) {
  (x - soilwater_range[1]) / (soilwater_range[2] - soilwater_range[1]) * 100
}
scale_pct_to_soilwater <- function(pct) {
  soilwater_range[1] + pct / 100 * (soilwater_range[2] - soilwater_range[1])
}

plot_data_map$soilwater_scaled_pct <- scale_soilwater_to_pct(plot_data_map$soilwater_raw)

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

# Panel 1: FIXE violins + raw N fixation scatter -----------------------------
figure_fixe_rawN <- ggplot() +
  geom_point(data = rawN_points, aes(x = MAP, y = raw_N_scaled),
             shape = 24, color = "black", fill = "red", alpha = 0.6, size = 2.5) +
  geom_violin(data = violin_data_fixe, aes(x = x_pos, y = FIXE, group = precip_bin),
              fill = "grey85", color = "black", trim = TRUE, width = 0.3) +
  geom_boxplot(data = violin_data_fixe, aes(x = x_pos, y = FIXE, group = precip_bin),
               width = 0.08, fill = "white", outlier.size = 1.5) +
  scale_x_log10(
    breaks = c(10, 100, 1000, 10000),
    labels = expression(10^1, 10^2, 10^3, 10^4)
  ) +
  scale_y_log10(
    name = "Fixation efficiency (FIXE)",
    breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1),
    labels = expression(10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0),
    sec.axis = sec_axis(
      trans = ~ scale_log_to_rawN(.),
      name = expression("N Fixation (g"*~m^-2*~y^-1*")"),
      breaks = raw_breaks,
      labels = raw_labels
    )
  ) +
  # Zoom the *view* to this window without dropping data from the stats
  # (unlike passing limits= to the scales above, which discards points
  # outside the window before geom_violin computes its density estimate).
  coord_cartesian(xlim = c(10, 10000), ylim = nc_log_limits) +
  common_theme +
  theme(axis.title.x = element_blank())

# Panel 2: light available (1 - canopy cover) + soil water scatter ----------
figure_light_soilwater <- ggplot() +
  geom_point(data = plot_data_map, aes(x = mean_annual_precip, y = light_available_pct, color = "Light available (%)"),
             alpha = 0.1, size = 0.3) +
  geom_point(data = plot_data_map, aes(x = mean_annual_precip, y = soilwater_scaled_pct, color = "Soil water (m³ water / m³ soil)"),
             alpha = 0.1, size = 0.3) +
  scale_color_manual(
    name = "",
    values = c("Light available (%)" = "#8c510a",
               "Soil water (m³ water / m³ soil)" = "#3182bd"),
    guide = "none"
  ) +
  scale_x_log10(
    name = expression("Precipitation (mm yr"^-1*")"),
    breaks = c(10, 100, 1000, 10000),
    labels = expression(10^1, 10^2, 10^3, 10^4)
  ) +
  scale_y_continuous(
    name = "Light available (%)",
    sec.axis = sec_axis(
      trans = ~ scale_pct_to_soilwater(.),
      name = expression("Soil water (" * m^3 ~ "water" ~ m^-3 ~ "soil)")
    )
  ) +
  # Same non-clipping approach as panel 1: crop the view, not the data.
  coord_cartesian(xlim = c(10, 10000), ylim = c(0, 100)) +
  common_theme +
  theme(legend.position = "none")

figure_fixe_rawN_precip3 <- figure_fixe_rawN / figure_light_soilwater

print(figure_fixe_rawN_precip3)

ggsave("figure_fixe_rawN_precip3.png", figure_fixe_rawN_precip3, width = 180, height = 180, units = "mm", dpi = 300)
ggsave("figure_fixe_rawN_precip3.pdf", figure_fixe_rawN_precip3, width = 180, height = 180, units = "mm", dpi = 300)
