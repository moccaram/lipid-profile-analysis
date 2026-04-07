# visualize.R — ggplot2 charts for lipid profile analysis

if (!require("tidyverse"))  install.packages("tidyverse",  repos = "https://cran.r-project.org")
if (!require("patchwork"))  install.packages("patchwork",  repos = "https://cran.r-project.org")
if (!require("corrplot"))   install.packages("corrplot",   repos = "https://cran.r-project.org")
if (!require("RColorBrewer")) install.packages("RColorBrewer", repos = "https://cran.r-project.org")
library(tidyverse)
library(patchwork)
library(corrplot)
library(RColorBrewer)

df <- read_csv("data/synthetic_lipid_data.csv", show_col_types = FALSE) %>%
  mutate(bmi_group = factor(bmi_group, levels = c("Normal", "Overweight", "Obese")))

# Consistent palette
pal <- c("Normal" = "#2ca02c", "Overweight" = "#ff7f0e", "Obese" = "#d62728")
theme_set(theme_minimal(base_size = 13))

# ============================================================
# 1. Boxplots of lipid values by BMI group
# ============================================================
lipid_vars <- c("total_cholesterol", "hdl", "ldl", "triglycerides")
lipid_labels <- c("Total Cholesterol (mg/dL)", "HDL (mg/dL)",
                   "LDL (mg/dL)", "Triglycerides (mg/dL)")

plots <- map2(lipid_vars, lipid_labels, function(var, lab) {
  ggplot(df, aes(x = bmi_group, y = .data[[var]], fill = bmi_group)) +
    geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.size = 2) +
    scale_fill_manual(values = pal) +
    labs(x = NULL, y = lab) +
    theme(legend.position = "none")
})

p_lipid <- (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]]) +
  plot_annotation(
    title = "Lipid Profile by BMI Category",
    subtitle = "Asian WHO cutoffs: Normal <23, Overweight 23–27.5, Obese ≥27.5",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )

ggsave("outputs/figures/lipid_boxplots.png", p_lipid,
       width = 10, height = 8, dpi = 300, bg = "white")
cat("Saved: outputs/figures/lipid_boxplots.png\n")

# ============================================================
# 2. Correlation heatmap
# ============================================================
numeric_cols <- df %>%
  select(bmi, total_cholesterol, hdl, ldl, triglycerides,
         fasting_glucose, physical_activity_level, dietary_score)

cor_mat <- cor(numeric_cols, use = "complete.obs")

png("outputs/figures/correlation_heatmap.png", width = 2400, height = 2000, res = 300)
corrplot(cor_mat,
         method   = "color",
         type     = "lower",
         addCoef.col = "black",
         number.cex  = 0.75,
         tl.col   = "black",
         tl.cex   = 0.85,
         col      = colorRampPalette(c("#2166ac", "white", "#b2182b"))(200),
         title    = "Correlation Matrix — Lipid & Lifestyle Variables",
         mar      = c(0, 0, 2, 0))
dev.off()
cat("Saved: outputs/figures/correlation_heatmap.png\n")

# ============================================================
# 3. BMI distribution by group
# ============================================================
p_bmi <- ggplot(df, aes(x = bmi, fill = bmi_group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  geom_vline(xintercept = c(23, 27.5), linetype = "dashed", color = "grey30") +
  annotate("text", x = 20.5, y = Inf, label = "Normal", vjust = 1.5, size = 3.5) +
  annotate("text", x = 25.2, y = Inf, label = "Overweight", vjust = 1.5, size = 3.5) +
  annotate("text", x = 32,   y = Inf, label = "Obese", vjust = 1.5, size = 3.5) +
  scale_fill_manual(values = pal) +
  labs(
    title = "BMI Distribution of Study Participants",
    subtitle = "Dashed lines = Asian WHO cutoffs (23, 27.5 kg/m²)",
    x = "BMI (kg/m²)", y = "Count", fill = "BMI Group"
  ) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/figures/bmi_distribution.png", p_bmi,
       width = 9, height = 5, dpi = 300, bg = "white")
cat("Saved: outputs/figures/bmi_distribution.png\n")

# ============================================================
# 4. Fasting glucose boxplot (bonus)
# ============================================================
p_glucose <- ggplot(df, aes(x = bmi_group, y = fasting_glucose, fill = bmi_group)) +
  geom_boxplot(alpha = 0.75) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.2) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Fasting Glucose by BMI Category",
    x = NULL, y = "Fasting Glucose (mg/dL)"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave("outputs/figures/fasting_glucose_boxplot.png", p_glucose,
       width = 6, height = 5, dpi = 300, bg = "white")
cat("Saved: outputs/figures/fasting_glucose_boxplot.png\n")

cat("\n--- All figures saved ---\n")
