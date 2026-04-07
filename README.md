# Lipid Profile Patterns Across BMI Categories in Medical Students

A cross-sectional analysis examining lipid profile differences across Asian WHO BMI categories among 203 medical students, demonstrating statistical analysis, data visualization, and academic reporting skills.

## Skills Demonstrated

- **R Programming** — data simulation, wrangling, and statistical analysis
- **ggplot2** — publication-quality boxplots, histograms, and correlation heatmaps
- **Statistical Analysis** — ANOVA, chi-square tests, effect sizes (eta-squared, Cramer's V), correlation matrices
- **Survey/Clinical Data** — working with health survey data and clinical biomarkers
- **Academic Reporting** — APA-style tables, inline statistics, structured scientific report
- **Quarto** — reproducible report generation (HTML and PDF)

## How to Reproduce

1. **Clone the repository** and ensure R (>= 4.4) and Quarto are installed
2. **Run the scripts** in order:
   ```r
   source("simulate_data.R")   # generates data/synthetic_lipid_data.csv
   source("analysis.R")        # outputs statistical tables to outputs/
   source("visualize.R")       # outputs figures to outputs/figures/
   ```
3. **Render the report:**
   ```bash
   quarto render portfolio_report.qmd
   ```

## Project Structure

```
lipid-profile-portfolio/
├── simulate_data.R          # Data generation (n=203, set.seed(42))
├── analysis.R               # Descriptive stats, ANOVA, chi-square, correlations
├── visualize.R              # ggplot2 figures
├── portfolio_report.qmd     # Quarto report document
├── data/
│   └── synthetic_lipid_data.csv
├── outputs/
│   ├── descriptive_statistics.csv
│   ├── anova_results.csv
│   ├── correlation_matrix.csv
│   ├── chi_square_sex.csv
│   ├── chi_square_physical_activity.csv
│   └── figures/
│       ├── lipid_boxplots.png
│       ├── correlation_heatmap.png
│       ├── bmi_distribution.png
│       └── fasting_glucose_boxplot.png
└── portfolio_report.html    # Rendered report
```

## Rendered Report

[View the HTML report](portfolio_report.html) <!-- Replace with GitHub Pages link after deployment -->

## License

MIT
