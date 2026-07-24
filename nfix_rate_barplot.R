# Bar graph: among high-light data points (High < 0.25), count of N fixation
# rates (No_Coverage) above vs. below 1, split by water saturation bin
# (<0.5 vs >0.5).

library(readxl)
library(ggplot2)

NvsAZ <- read_excel("Downloads/NvsAZ_v39.xlsx")

# Exclude studies whose satellite-derived light/cover data is known to be
# unreliable for this analysis:
#   - Zheng et al. (2020): forest planted in degraded grassland
#   - Rousk and Michelsen (2016): birch forest
#   - Goth et al. (2019): railroad site
#   - Permin et al. (2021): closed canopy
#   - Kubota et al. (2023): closed canopy
excluded_studies <- c("Zheng et al. (2020)", "Rousk and Michelsen (2016)",
                       "Goth et al. (2019)", "Permin et al. (2021)",
                       "Kubota et al. (2023)")

NvsAZ_clean <- NvsAZ[!is.na(NvsAZ$No_Coverage) &
                        !is.na(NvsAZ$`Water Saturation`) &
                        !is.na(NvsAZ$High) &
                        !(NvsAZ$Study %in% excluded_studies), ]

high_light <- NvsAZ_clean[NvsAZ_clean$High < 0.25, ]

high_light$WaterSat_Cat <- ifelse(high_light$`Water Saturation` < 0.5, "<0.5", ">0.5")
high_light$Rate_Cat <- ifelse(high_light$No_Coverage > 1, "Above 1", "Below 1")

counts <- as.data.frame(table(WaterSat_Cat = high_light$WaterSat_Cat,
                               Rate_Cat = high_light$Rate_Cat))

cat("Counts:\n")
print(counts)

bar_plot <- ggplot(counts, aes(x = WaterSat_Cat, y = Freq, fill = Rate_Cat)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("Above 1" = "#3182bd", "Below 1" = "#deebf7"), name = "N fixation rate") +
  labs(x = "Water saturation", y = "Count of data points",
       title = "N fixation rate (No_Coverage) above/below 1\nHigh light points (High < 0.25)") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

print(bar_plot)

ggsave("nfix_rate_barplot.png", bar_plot, width = 120, height = 100, units = "mm", dpi = 300)
