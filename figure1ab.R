# Figure AB: N:C Ratio by Light Availability and Soil Moisture (Side by Side)
# Formatted for Nature submission
 
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
 
# Read the data
NvsAZ <- read_excel("Downloads/NvsAZ_v43.xlsx")
CvsAZ <- read_excel("Downloads/CvsAZ_v15.xlsx")

# Remove NAs
NvsAZ_clean <- NvsAZ[!is.na(NvsAZ$No_Coverage), ]
CvsAZ_clean <- CvsAZ[!is.na(CvsAZ$`Raw No Coverage`), ]
 
# Paired bootstrap function for N:C ratios
paired_bootstrap_n_c_ratios <- function(n_values, c_values, n_iterations = 10000) {
  ratios <- numeric(n_iterations)
  
  for (i in 1:n_iterations) {
    n_sample <- sample(n_values, length(n_values), replace = TRUE)
    c_sample <- sample(c_values, length(c_values), replace = TRUE)
    mean_n <- mean(n_sample)
    mean_c <- mean(c_sample)
    ratios[i] <- mean_n / mean_c
  }
  
  return(ratios)
}
 
##############################################################################
# PANEL A: Bootstrap N:C by Light Availability
##############################################################################
# Create canopy categories
NvsAZ_clean$Canopy_Cat <- cut(NvsAZ_clean$High,
                               breaks = c(0, 0.25, 0.5, 1),
                               labels = c("0-0.25", "0.25-0.5", "0.5+"),
                               right = FALSE, include.lowest = TRUE)

CvsAZ_clean$Canopy_Cat <- cut(CvsAZ_clean$High,
                               breaks = c(0, 0.25, 0.5, 1),
                               labels = c("0-0.25", "0.25-0.5", "0.5+"),
                               right = FALSE, include.lowest = TRUE)
 
canopy_cats <- c("0.5+", "0.25-0.5", "0-0.25")
results_nc_canopy <- list()
 
for (cat in canopy_cats) {
  n_values <- NvsAZ_clean$No_Coverage[NvsAZ_clean$Canopy_Cat == cat & !is.na(NvsAZ_clean$Canopy_Cat)]
  c_values <- CvsAZ_clean$`Raw No Coverage`[CvsAZ_clean$Canopy_Cat == cat & !is.na(CvsAZ_clean$Canopy_Cat)]
  
  if (length(n_values) > 0 & length(c_values) > 0) {
    ratios <- paired_bootstrap_n_c_ratios(n_values, c_values, n_iterations = 10000)
    results_nc_canopy[[cat]] <- ratios
  }
}
 
# Prepare data for Panel A
plot_data_light <- data.frame()
for (i in 1:length(results_nc_canopy)) {
  cat_name <- names(results_nc_canopy)[i]
  if (cat_name == "0.5+") {
    light_cat <- "0-0.5"
  } else if (cat_name == "0.25-0.5") {
    light_cat <- "0.5-0.75"
  } else if (cat_name == "0-0.25") {
    light_cat <- "0.75-1"
  }
  
  values <- results_nc_canopy[[i]]
  plot_data_light <- rbind(plot_data_light, 
                           data.frame(Light = light_cat, 
                                     NC_ratio = values))
}
 
plot_data_light$Light <- factor(plot_data_light$Light, 
                                levels = c("0-0.5", "0.5-0.75", "0.75-1"))
 
light_colors <- c("#8c510a", "#d8b365", "#f6e8c3")
 
##############################################################################
# PANEL B: Bootstrap N:C by Soil Organic/Mineral Type
##############################################################################
soilwater_cats <- c("Mineral", "Organic")

results_nc_moisture <- list()

for (cat in soilwater_cats) {
  n_values <- NvsAZ_clean$No_Coverage[NvsAZ_clean$Organic == cat &
                                       !is.na(NvsAZ_clean$Organic)]
  c_values <- CvsAZ_clean$`Raw No Coverage`[CvsAZ_clean$Organic == cat &
                                             !is.na(CvsAZ_clean$Organic)]

  if (length(n_values) > 0 & length(c_values) > 0) {
    ratios <- paired_bootstrap_n_c_ratios(n_values, c_values, n_iterations = 10000)
    results_nc_moisture[[cat]] <- ratios
  }
}

# Only create plot data for categories that have results
plot_data_moisture <- data.frame()
for (cat_name in names(results_nc_moisture)) {
  values <- results_nc_moisture[[cat_name]]
  plot_data_moisture <- rbind(plot_data_moisture,
                              data.frame(Moisture = cat_name,
                                        NC_ratio = values))
}

# Factor with only the categories we have
present_soilwater_cats <- soilwater_cats[soilwater_cats %in% names(results_nc_moisture)]
plot_data_moisture$Moisture <- factor(plot_data_moisture$Moisture,
                                      levels = present_soilwater_cats)

moisture_color_lookup <- setNames(c("#deebf7", "#3182bd"), soilwater_cats)
moisture_colors <- moisture_color_lookup[present_soilwater_cats]
 
##############################################################################
# CREATE PANELS WITH CONSISTENT FORMATTING
##############################################################################
 
# Common theme settings for all panels
common_theme <- theme_classic(base_size = 8) +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(10, 10, 15, 10),  # Keep the increased bottom margin
    legend.position = "none",
    axis.text = element_text(size = 8),   # Restored to 8
    axis.title = element_text(size = 8)   # Restored to 8
  )
 
# Panel A
panel_a <- ggplot(plot_data_light, aes(x = Light, y = NC_ratio, fill = Light)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.07, fill = "white", outlier.size = 1.5) +
  scale_fill_manual(
    values = light_colors, 
    name = "Light (%)",
    labels = c("<50", "50-75", ">75")
  ) +
  scale_x_discrete(labels = c("<50", "50-75", ">75")) +
  scale_y_log10(
    breaks = c(0.001, 0.01, 0.1),
    labels = expression(10^-3, 10^-2, 10^-1),
    limits = c(0.001, 0.1)
  ) +
  labs(x = "Light availability (%)", 
       y = expression("Fixation efficiency (g N"*~g^-1*~"C)")) +
  common_theme +
  theme(aspect.ratio = 1)
 
# Panel B
panel_b <- ggplot(plot_data_moisture, aes(x = Moisture, y = NC_ratio, fill = Moisture)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.07, fill = "white", outlier.size = 1.5) +
  scale_fill_manual(
    values = moisture_colors,
    name = "Soil type",
    labels = present_soilwater_cats
  ) +
  scale_x_discrete(labels = present_soilwater_cats) +
  scale_y_log10(
    breaks = c(0.001, 0.01, 0.1, 1),
    labels = expression(10^-3, 10^-2, 10^-1, 10^0),
    limits = c(0.001, 1)
  ) +
  labs(x = "Soil type",
       y = expression("Fixation efficiency (g N"*~g^-1*~"C)")) +
  common_theme +
  theme(aspect.ratio = 1)
 
##############################################################################
# COMBINE PANELS SIDE BY SIDE
##############################################################################
combined_plot <- grid.arrange(panel_a, panel_b, ncol = 2)
 
