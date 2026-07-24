# Figure: Variance in fixation efficiency (FIXE = N:C ratio) explained by
# each of four predictors, taken one at a time (Biome, Light availability,
# Soil water, Soil type). For each predictor, categories are bootstrapped
# into FIXE distributions using the same paired-bootstrap approach as
# figure1ab.R/figure1cd.R, then a one-way ANOVA (FIXE ~ category) gives the
# sum of squares explained by that predictor as a percent of total SS.
# Formatted to match the style of figure1ab.R.

library(readxl)
library(dplyr)
library(ggplot2)

# Read the data
NvsAZ <- read_excel("Downloads/NvsAZ_v43.xlsx")
CvsAZ <- read_excel("Downloads/CvsAZ_v15.xlsx")

# Remove NAs
NvsAZ_clean <- NvsAZ[!is.na(NvsAZ$No_Coverage), ]
CvsAZ_clean <- CvsAZ[!is.na(CvsAZ$`Raw No Coverage`), ]

# Paired bootstrap function for N:C ratios (identical to figure1ab.R)
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
# Build matching category labels on both datasets, one factor at a time
##############################################################################

# Biome: used as-is
NvsAZ_clean$Biome_Cat <- NvsAZ_clean$Biome
CvsAZ_clean$Biome_Cat <- CvsAZ_clean$Biome

# Light availability: same canopy -> light binning/relabeling as figure1ab.R
canopy_to_light <- function(high_values) {
  canopy_cat <- cut(high_values, breaks = c(0, 0.25, 0.5, 1),
                     labels = c("0-0.25", "0.25-0.5", "0.5+"),
                     right = FALSE, include.lowest = TRUE)
  light_cat <- factor(rep(NA, length(high_values)),
                       levels = c("0-0.5", "0.5-0.75", "0.75-1"))
  light_cat[canopy_cat == "0.5+"] <- "0-0.5"
  light_cat[canopy_cat == "0.25-0.5"] <- "0.5-0.75"
  light_cat[canopy_cat == "0-0.25"] <- "0.75-1"
  light_cat
}
NvsAZ_clean$Light_Cat <- canopy_to_light(NvsAZ_clean$High)
CvsAZ_clean$Light_Cat <- canopy_to_light(CvsAZ_clean$High)

# Soil water: tertiles, breaks shared across both datasets
sw_breaks <- quantile(c(NvsAZ_clean$`Soil Water`, CvsAZ_clean$`Soil Water`),
                       probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
sw_labels <- c("Low", "Medium", "High")
NvsAZ_clean$SoilWater_Cat <- cut(NvsAZ_clean$`Soil Water`, breaks = sw_breaks,
                                  labels = sw_labels, include.lowest = TRUE)
CvsAZ_clean$SoilWater_Cat <- cut(CvsAZ_clean$`Soil Water`, breaks = sw_breaks,
                                  labels = sw_labels, include.lowest = TRUE)

# Soil type: Mineral vs Organic
NvsAZ_clean$SoilType_Cat <- NvsAZ_clean$Organic
CvsAZ_clean$SoilType_Cat <- CvsAZ_clean$Organic

##############################################################################
# For a given category column, bootstrap FIXE per category, then run a
# one-way ANOVA and return % of total sum of squares explained by category
##############################################################################
compute_pct_ss <- function(cat_col, n_iterations = 10000) {
  cats <- na.omit(unique(c(NvsAZ_clean[[cat_col]], CvsAZ_clean[[cat_col]])))

  boot_df <- data.frame()
  for (cat in cats) {
    n_values <- NvsAZ_clean$No_Coverage[NvsAZ_clean[[cat_col]] == cat & !is.na(NvsAZ_clean[[cat_col]])]
    c_values <- CvsAZ_clean$`Raw No Coverage`[CvsAZ_clean[[cat_col]] == cat & !is.na(CvsAZ_clean[[cat_col]])]

    if (length(n_values) > 0 & length(c_values) > 0) {
      ratios <- paired_bootstrap_n_c_ratios(n_values, c_values, n_iterations)
      boot_df <- rbind(boot_df, data.frame(Category = cat, FIXE = ratios))
    }
  }
  boot_df$Category <- factor(boot_df$Category)

  fit <- aov(FIXE ~ Category, data = boot_df)
  ss <- summary(fit)[[1]][["Sum Sq"]]
  100 * ss[1] / sum(ss)
}

pct_biome <- compute_pct_ss("Biome_Cat")
pct_light <- compute_pct_ss("Light_Cat")
pct_soilwater <- compute_pct_ss("SoilWater_Cat")
pct_soiltype <- compute_pct_ss("SoilType_Cat")

ss_data <- data.frame(
  Factor = factor(c("Biome", "Light", "Soil water", "Soil type"),
                   levels = c("Biome", "Light", "Soil water", "Soil type")),
  PctSS = c(pct_biome, pct_light, pct_soilwater, pct_soiltype)
)

##############################################################################
# Plot (same formatting as figure1ab.R panels)
##############################################################################
common_theme <- theme_classic(base_size = 8) +
  theme(
    axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(10, 10, 15, 10),
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8)
  )

ss_barplot <- ggplot(ss_data, aes(x = Factor, y = PctSS)) +
  geom_col(fill = "grey70", color = "black", width = 0.6) +
  labs(x = NULL, y = "Variance in fixation efficiency\nexplained (% sum of squares)") +
  common_theme +
  theme(aspect.ratio = 1)

ss_barplot

ggsave("figure_ss_barplot.png", ss_barplot, width = 90, height = 90, units = "mm", dpi = 300)
ggsave("figure_ss_barplot.pdf", ss_barplot, width = 90, height = 90, units = "mm", dpi = 300)
