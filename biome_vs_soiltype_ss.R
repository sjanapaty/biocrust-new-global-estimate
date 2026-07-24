# =============================================================
# How much variance (sum of squares) in log(N fixation) is
# explained by Biome vs. Soil Type?
# Response: log(No_Coverage)
# =============================================================

library(readxl)

df <- read_excel("~/Downloads/NvsAZ_v43.xlsx")

# Keep rows with a valid, positive response and non-missing predictors
df <- df[!is.na(df$No_Coverage) & df$No_Coverage > 0 &
           !is.na(df$Biome) & !is.na(df$`Soil Type`), ]

df$log_N <- log(df$No_Coverage)
df$Biome <- factor(df$Biome)
df$SoilType <- factor(df$`Soil Type`)

cat("Observations used:", nrow(df), "\n\n")

# ---- Fit each predictor alone ----
m_biome <- lm(log_N ~ Biome, data = df)
m_soil  <- lm(log_N ~ SoilType, data = df)

ss_from_lm <- function(model) {
  a <- anova(model)
  ss_total  <- sum(a[["Sum Sq"]])
  ss_resid  <- a["Residuals", "Sum Sq"]
  ss_explained <- ss_total - ss_resid
  list(ss_explained = ss_explained, ss_total = ss_total,
       pct = 100 * ss_explained / ss_total)
}

biome_ss <- ss_from_lm(m_biome)
soil_ss  <- ss_from_lm(m_soil)

# ---- Combined model to see joint / overlapping contribution ----
m_both <- lm(log_N ~ Biome + SoilType, data = df)
both_ss <- ss_from_lm(m_both)

# Sequential (Type I) SS from the combined model shows how much
# each term adds given the order it's entered
a_both <- anova(m_both)
cat("===== Sequential SS (Biome entered first, then Soil Type) =====\n")
print(a_both)

results <- data.frame(
  Model = c("Biome alone", "Soil Type alone", "Biome + Soil Type"),
  SS_explained = c(biome_ss$ss_explained, soil_ss$ss_explained, both_ss$ss_explained),
  SS_total = c(biome_ss$ss_total, soil_ss$ss_total, both_ss$ss_total),
  Pct_SS_explained = c(biome_ss$pct, soil_ss$pct, both_ss$pct)
)

cat("\n===== Summary: % of total SS explained =====\n")
print(results, row.names = FALSE, digits = 4)

cat("\nBiome alone explains", round(biome_ss$pct, 1), "% of SS in log(N fixation).\n")
cat("Soil Type alone explains", round(soil_ss$pct, 1), "% of SS in log(N fixation).\n")
cat("Together they explain", round(both_ss$pct, 1), "%.\n")

# ---- Bar plot ----
barplot(results$Pct_SS_explained,
        names.arg = results$Model,
        col = "grey80", border = "black",
        ylab = "% of total SS explained",
        main = "Biome vs. Soil Type: log(N fixation)",
        ylim = c(0, max(results$Pct_SS_explained) * 1.2))
text(seq_along(results$Pct_SS_explained), results$Pct_SS_explained,
     labels = paste0(round(results$Pct_SS_explained, 1), "%"),
     pos = 3, cex = 0.9)
