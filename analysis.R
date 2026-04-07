# analysis.R — Descriptive stats, chi-square, ANOVA, correlation matrix

if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cran.r-project.org")
if (!require("broom"))     install.packages("broom",     repos = "https://cran.r-project.org")
if (!require("effectsize")) install.packages("effectsize", repos = "https://cran.r-project.org")
library(tidyverse)
library(broom)
library(effectsize)

df <- read_csv("data/synthetic_lipid_data.csv", show_col_types = FALSE) %>%
  mutate(bmi_group = factor(bmi_group, levels = c("Normal", "Overweight", "Obese")))

# ============================================================
# 1. Descriptive statistics by BMI group (continuous variables)
# ============================================================
continuous_vars <- c("age", "bmi", "total_cholesterol", "hdl", "ldl",
                     "triglycerides", "fasting_glucose", "dietary_score")

desc_stats <- df %>%
  pivot_longer(cols = all_of(continuous_vars), names_to = "variable") %>%
  group_by(bmi_group, variable) %>%
  summarise(
    n    = n(),
    mean = round(mean(value), 2),
    sd   = round(sd(value), 2),
    median = round(median(value), 2),
    q1   = round(quantile(value, 0.25), 2),
    q3   = round(quantile(value, 0.75), 2),
    .groups = "drop"
  )

write_csv(desc_stats, "outputs/descriptive_statistics.csv")
cat("Saved: outputs/descriptive_statistics.csv\n")

# ============================================================
# 2. Sex distribution — Chi-square test
# ============================================================
sex_table <- table(df$bmi_group, df$sex)
chi_result <- chisq.test(sex_table)

chi_df <- tibble(
  test       = "Chi-square (sex × BMI group)",
  statistic  = round(chi_result$statistic, 3),
  df         = chi_result$parameter,
  p_value    = round(chi_result$p.value, 4),
  cramers_v  = round(cramers_v(sex_table)$Cramers_v, 3)
)

write_csv(chi_df, "outputs/chi_square_sex.csv")
cat("Saved: outputs/chi_square_sex.csv\n")

# Physical activity level (ordinal, treated as categorical here)
pa_table <- table(df$bmi_group, df$physical_activity_level)
chi_pa <- chisq.test(pa_table)

chi_pa_df <- tibble(
  test       = "Chi-square (physical activity × BMI group)",
  statistic  = round(chi_pa$statistic, 3),
  df         = chi_pa$parameter,
  p_value    = round(chi_pa$p.value, 4),
  cramers_v  = round(cramers_v(pa_table)$Cramers_v, 3)
)

write_csv(chi_pa_df, "outputs/chi_square_physical_activity.csv")
cat("Saved: outputs/chi_square_physical_activity.csv\n")

# ============================================================
# 3. One-way ANOVA for continuous variables across BMI groups
# ============================================================
anova_results <- map_dfr(continuous_vars, function(v) {
  formula <- as.formula(paste(v, "~ bmi_group"))
  model <- aov(formula, data = df)
  tidy_aov <- tidy(model) %>% filter(term == "bmi_group")
  es <- eta_squared(model)

  tibble(
    variable   = v,
    f_statistic = round(tidy_aov$statistic, 3),
    df1        = tidy_aov$df,
    df2        = (tidy(model) %>% filter(term == "Residuals"))$df,
    p_value    = signif(tidy_aov$p.value, 4),
    eta_squared = round(es$Eta2, 4),
    significance = case_when(
      tidy_aov$p.value < 0.001 ~ "***",
      tidy_aov$p.value < 0.01  ~ "**",
      tidy_aov$p.value < 0.05  ~ "*",
      TRUE                     ~ "ns"
    )
  )
})

write_csv(anova_results, "outputs/anova_results.csv")
cat("Saved: outputs/anova_results.csv\n")

# ============================================================
# 4. Correlation matrix (numeric variables)
# ============================================================
numeric_cols <- df %>%
  select(age, bmi, total_cholesterol, hdl, ldl, triglycerides,
         fasting_glucose, physical_activity_level, dietary_score)

cor_matrix <- round(cor(numeric_cols, use = "complete.obs"), 3)
write_csv(as_tibble(cor_matrix, rownames = "variable"), "outputs/correlation_matrix.csv")
cat("Saved: outputs/correlation_matrix.csv\n")

cat("\n--- Analysis complete ---\n")
