# simulate_data.R — Generate synthetic lipid profile dataset
# 203 medical students across 3 BMI groups (Asian WHO cutoffs)

if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cran.r-project.org")
library(tidyverse)

set.seed(42)

n <- 203

# Asian WHO BMI cutoffs: Normal <23, Overweight 23-27.5, Obese >=27.5
# Allocate roughly: 80 normal, 70 overweight, 53 obese
group_sizes <- c(80, 70, 53)
bmi_group <- rep(c("Normal", "Overweight", "Obese"), times = group_sizes)

# --- Demographics ---
age <- round(rnorm(n, mean = 22, sd = 2.5))
age <- pmax(age, 18)  # floor at 18
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.45, 0.55))

# --- BMI (continuous, consistent with group) ---
bmi <- numeric(n)
bmi[bmi_group == "Normal"]     <- runif(group_sizes[1], min = 18.5, max = 22.9)
bmi[bmi_group == "Overweight"] <- runif(group_sizes[2], min = 23.0, max = 27.4)
bmi[bmi_group == "Obese"]      <- runif(group_sizes[3], min = 27.5, max = 38.0)
bmi <- round(bmi, 1)

# --- Lipid Panel (mg/dL) — shifted by group ---
# Total cholesterol
tc_base <- c(Normal = 180, Overweight = 200, Obese = 220)
total_cholesterol <- round(rnorm(n, mean = tc_base[bmi_group], sd = 25))

# HDL (inversely related to BMI)
hdl_base <- c(Normal = 55, Overweight = 47, Obese = 40)
hdl <- round(rnorm(n, mean = hdl_base[bmi_group], sd = 8))
hdl <- pmax(hdl, 20)

# LDL
ldl_base <- c(Normal = 100, Overweight = 120, Obese = 140)
ldl <- round(rnorm(n, mean = ldl_base[bmi_group], sd = 22))

# Triglycerides (log-normal-ish, right-skewed)
tg_base <- c(Normal = 100, Overweight = 140, Obese = 180)
triglycerides <- round(rlnorm(n,
  meanlog = log(tg_base[bmi_group]),
  sdlog = 0.35
))

# Fasting glucose
fg_base <- c(Normal = 88, Overweight = 95, Obese = 104)
fasting_glucose <- round(rnorm(n, mean = fg_base[bmi_group], sd = 10))

# --- Lifestyle variables ---
# Physical activity: 1 (sedentary) to 5 (very active), inversely related to BMI
pa_probs <- list(
  Normal     = c(0.05, 0.10, 0.25, 0.35, 0.25),
  Overweight = c(0.10, 0.25, 0.35, 0.20, 0.10),
  Obese      = c(0.25, 0.30, 0.25, 0.15, 0.05)
)
physical_activity_level <- integer(n)
for (g in c("Normal", "Overweight", "Obese")) {
  idx <- which(bmi_group == g)
  physical_activity_level[idx] <- sample(1:5, length(idx), replace = TRUE,
                                         prob = pa_probs[[g]])
}

# Dietary score: 1 (poor) to 10 (excellent)
ds_mean <- c(Normal = 6.5, Overweight = 5.5, Obese = 4.5)
dietary_score <- round(rnorm(n, mean = ds_mean[bmi_group], sd = 1.5), 1)
dietary_score <- pmin(pmax(dietary_score, 1), 10)

# --- Assemble and save ---
df <- tibble(
  id                     = 1:n,
  age                    = age,
  sex                    = sex,
  bmi_group              = factor(bmi_group, levels = c("Normal", "Overweight", "Obese")),
  bmi                    = bmi,
  total_cholesterol      = total_cholesterol,
  hdl                    = hdl,
  ldl                    = ldl,
  triglycerides          = triglycerides,
  fasting_glucose        = fasting_glucose,
  physical_activity_level = physical_activity_level,
  dietary_score          = dietary_score
)

write_csv(df, "data/synthetic_lipid_data.csv")
cat("Data saved: data/synthetic_lipid_data.csv\n")
cat("Rows:", nrow(df), "| Groups:", paste(table(df$bmi_group), collapse = ", "), "\n")
