# =============================================================
# Model comparison for log(N fixation)
# Response: log(No_Coverage)
# Models: Biome | Canopy (binned) | Organic | Canopy * Organic
# No forest exclusion -- all models use the full dataset
# =============================================================

library(readxl)
library(dplyr)

# ---- Read data ----
NvsAZ <- read_excel("Downloads/NvsAZ_v43.xlsx")

# ---- Clean ----
NvsAZ_clean <- NvsAZ[!is.na(NvsAZ$No_Coverage) &
                       NvsAZ$No_Coverage > 0 &
                       !is.na(NvsAZ$Biome) &
                       !is.na(NvsAZ$Organic) &
                       !is.na(NvsAZ$FG_host) &
                       !is.na(NvsAZ$High), ]

# Log-transform the response
NvsAZ_clean$log_N <- log(NvsAZ_clean$No_Coverage)

# Bin canopy cover into three categories
NvsAZ_clean$Canopy_Cat <- cut(NvsAZ_clean$High,
                              breaks = c(0, 0.25, 0.5, 1),
                              labels = c("0-0.25", "0.25-0.5", "0.5+"),
                              include.lowest = TRUE)

cat("Total observations used:", nrow(NvsAZ_clean), "\n\n")

# ---- Helper: pull SS explained, R2, and omega-squared out of a fitted lm ----
# Omega-squared corrects for the number of parameters used (unlike R2, which
# is biased upward as predictors are added), so it's the fairer metric when
# comparing models with different df.
get_model_stats <- function(model, label) {
  a <- anova(model)
  ss_total   <- sum(a[["Sum Sq"]])
  ss_resid   <- a["Residuals", "Sum Sq"]
  df_resid   <- a["Residuals", "Df"]
  ss_explained <- ss_total - ss_resid
  df_explained <- sum(a[["Df"]]) - df_resid
  ms_error   <- ss_resid / df_resid
  omega2     <- (ss_explained - df_explained * ms_error) / (ss_total + ms_error)
  s <- summary(model)
  data.frame(
    Model      = label,
    SS_explained = ss_explained,
    SS_total   = ss_total,
    R2         = s$r.squared,
    Adj_R2     = s$adj.r.squared,
    Omega2     = omega2,
    n          = length(model$residuals),
    stringsAsFactors = FALSE
  )
}

# ---- Model 1: Biome alone ----
m1 <- lm(log_N ~ Biome, data = NvsAZ_clean)

# ---- Model 2: Canopy alone ----
m2 <- lm(log_N ~ Canopy_Cat, data = NvsAZ_clean)

# ---- Model 3: Organic alone ----
m3 <- lm(log_N ~ Organic, data = NvsAZ_clean)

# ---- Model 5: Taxonomy (FG_host) alone ----
m5 <- lm(log_N ~ FG_host, data = NvsAZ_clean)

# ---- Model 4: Canopy * Organic, with interaction safeguard ----
m4_int <- lm(log_N ~ Canopy_Cat * Organic, data = NvsAZ_clean)
int_row <- grep(":", rownames(anova(m4_int)), value = TRUE)
int_p <- anova(m4_int)[int_row, "Pr(>F)"]
int_p <- if (length(int_p) == 1) int_p else NA_real_

cat("Canopy x Organic interaction p-value:", signif(int_p, 4), "\n")
if (is.na(int_p) || int_p > 0.05) {
  cat("Interaction not significant -- using additive model (Canopy + Organic).\n\n")
  m4 <- lm(log_N ~ Canopy_Cat + Organic, data = NvsAZ_clean)
  m4_label <- "Light + SOM"
} else {
  cat("Interaction IS significant -- using interaction model (Canopy * Organic).\n\n")
  m4 <- m4_int
  m4_label <- "Light × SOM"
}

# ---- Collect results ----
results <- rbind(
  get_model_stats(m5, "Taxonomy"),
  get_model_stats(m4, m4_label),
  get_model_stats(m1, "Biome"),
  get_model_stats(m3, "SOM"),
  get_model_stats(m2, "Light")
)

cat("===== Model comparison: log(N fixation) =====\n")
print(results, row.names = FALSE, digits = 4)

cat("\n\n===== Full model summaries =====\n")
model_list <- list(Taxonomy = m5, m4, Biome = m1, SOM = m3, Light = m2)
names(model_list)[2] <- m4_label
for (nm in names(model_list)) {
  cat("\n---", nm, "---\n")
  print(summary(model_list[[nm]]))
}

# ---- Plot: Omega-squared ----
omega_pct <- results$Omega2 * 100
ylim_lo <- min(0, min(omega_pct) * 1.15)
ylim_hi <- max(0, max(omega_pct) * 1.15)

par(pty = "s")

bp <- barplot(omega_pct,
              names.arg = results$Model,
              col = "grey80",
              border = "black",
              ylab = expression(omega^2 ~ "of log(N fixation) (%)"),
              las = 1,
              ylim = c(ylim_lo, ylim_hi))
text(bp, omega_pct,
     labels = paste0(round(omega_pct, 1), "%"),
     pos = ifelse(omega_pct >= 0, 3, 1), cex = 0.8)
