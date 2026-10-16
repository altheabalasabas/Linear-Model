# APM1205 - Linear Models | Summative Assessment 1
# Reproducible analysis of Seoul Bike Sharing Demand


# ---------- 0. Packages and folders ----------
required_packages <- c("ggplot2", "dplyr", "readr", "broom", "car",
                       "lmtest", "knitr", "rmarkdown", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

root <- getwd()
data_dir <- file.path(root, "data")
fig_dir <- file.path(root, "output", "figures")
tab_dir <- file.path(root, "output", "tables")
report_dir <- file.path(root, "report")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

data_path <- "C:/Users/RuthMarienelle/Documents/Linear Model files/SeoulBikeData.csv"
setwd("C:/Users/RuthMarienelle/Documents/Linear Model files")

# ---------- 1. Read and standardize data ----------
bike <- readr::read_csv(data_path, show_col_types = FALSE, name_repair = "minimal")
names(bike) <- iconv(names(bike), from = "", to = "UTF-8", sub = "")
names(bike) <- trimws(names(bike))
names(bike)[names(bike) == "Temperature(C)"] <- "Temperature"
# Handle the original UCI column names and common spelling variants.
name_map <- c(
  "Rented Bike Count" = "Rented Bike Count",
  "Temperature(C)" = "Temperature",
  "Temperature (C)" = "Temperature",
  "Temperature" = "Temperature",
  "Humidity(%)" = "Humidity",
  "Humidity (%)" = "Humidity",
  "Humidity" = "Humidity",
  "Wind speed (m/s)" = "Wind speed",
  "Wind Speed (m/s)" = "Wind speed",
  "Wind speed" = "Wind speed",
  "Visibility (10m)" = "Visibility",
  "Dew point temperature(C)" = "Dew point temperature",
  "Solar Radiation (MJ/m2)" = "Solar Radiation",
  "Rainfall(mm)" = "Rainfall",
  "Snowfall (cm)" = "Snowfall",
  "Seasons" = "Seasons",
  "Holiday" = "Holiday",
  "Functioning Day" = "Functioning Day",
  "Date" = "Date",
  "Hour" = "Hour"
)
for (old_name in names(name_map)) {
  if (old_name %in% names(bike)) names(bike)[names(bike) == old_name] <- name_map[[old_name]]
}

needed <- c("Rented Bike Count", "Temperature", "Humidity", "Wind speed", "Seasons")
missing_cols <- setdiff(needed, names(bike))
if (length(missing_cols) > 0) {
  stop(paste("Required columns missing:", paste(missing_cols, collapse = ", "),
             "\nActual columns:", paste(names(bike), collapse = ", ")))
}

numeric_cols <- intersect(c("Rented Bike Count", "Temperature", "Humidity",
                            "Wind speed", "Visibility", "Dew point temperature",
                            "Solar Radiation", "Rainfall", "Snowfall", "Hour"),
                          names(bike))
bike[numeric_cols] <- lapply(bike[numeric_cols], function(x) suppressWarnings(as.numeric(x)))
if ("Date" %in% names(bike)) {
  bike$Date <- as.Date(bike$Date, format = "%d/%m/%Y")
}
bike$Seasons <- factor(bike$Seasons)
if ("Holiday" %in% names(bike)) bike$Holiday <- factor(bike$Holiday)
if ("Functioning Day" %in% names(bike)) bike$`Functioning Day` <- factor(bike$`Functioning Day`)

# ---------- 2. Data checks and descriptive statistics ----------
quality <- data.frame(
  variable = names(bike),
  class = vapply(bike, function(x) paste(class(x), collapse = "/"), character(1)),
  missing_count = vapply(bike, function(x) sum(is.na(x)), integer(1)),
  missing_percent = round(vapply(bike, function(x) mean(is.na(x)) * 100, numeric(1)), 3)
)
write.csv(quality, file.path(tab_dir, "missing_values.csv"), row.names = FALSE)
quality_summary <- data.frame(
  metric = c("rows", "columns", "duplicate_rows"),
  value = c(nrow(bike), ncol(bike), sum(duplicated(bike)))
)
write.csv(quality_summary, file.path(tab_dir, "data_quality_summary.csv"), row.names = FALSE)
write.csv(as.data.frame(summary(bike)), file.path(tab_dir, "descriptive_summary.csv"))
numeric_data <- bike[vapply(bike, is.numeric, logical(1))]
desc <- data.frame(
  variable = names(numeric_data),
  n = vapply(numeric_data, function(x) sum(!is.na(x)), integer(1)),
  mean = vapply(numeric_data, function(x) mean(x, na.rm = TRUE), numeric(1)),
  sd = vapply(numeric_data, function(x) sd(x, na.rm = TRUE), numeric(1)),
  min = vapply(numeric_data, function(x) min(x, na.rm = TRUE), numeric(1)),
  median = vapply(numeric_data, function(x) median(x, na.rm = TRUE), numeric(1)),
  max = vapply(numeric_data, function(x) max(x, na.rm = TRUE), numeric(1))
)
write.csv(desc, file.path(tab_dir, "descriptive_statistics_numeric.csv"), row.names = FALSE)

season_summary <- bike |>
  dplyr::group_by(Seasons) |>
  dplyr::summarise(n = sum(!is.na(`Rented Bike Count`)),
                   mean_rentals = mean(`Rented Bike Count`, na.rm = TRUE),
                   sd_rentals = sd(`Rented Bike Count`, na.rm = TRUE),
                   median_rentals = median(`Rented Bike Count`, na.rm = TRUE),
                   .groups = "drop")
write.csv(season_summary, file.path(tab_dir, "demand_by_season.csv"), row.names = FALSE)

# ---------- 3. Exploratory plots ----------
p_temp <- ggplot(bike, aes(x = Temperature, y = `Rented Bike Count`)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(title = "Rented Bike Count vs Temperature",
       x = "Temperature (\u00B0C)", y = "Rented Bike Count") +
  theme_minimal()
ggsave(file.path(fig_dir, "eda_temperature_scatter.png"), p_temp, width = 8, height = 5, dpi = 160)

p_season <- ggplot(bike, aes(x = Seasons, y = `Rented Bike Count`)) +
  geom_boxplot() +
  labs(title = "Bike Rentals by Season", x = "Season", y = "Rented Bike Count") +
  theme_minimal()
ggsave(file.path(fig_dir, "eda_demand_by_season.png"), p_season, width = 8, height = 5, dpi = 160)

if ("Hour" %in% names(bike)) {
  hourly <- bike |>
    group_by(Hour) |>
    summarise(mean_rentals = mean(`Rented Bike Count`, na.rm = TRUE), .groups = "drop")
  write.csv(hourly, file.path(tab_dir, "demand_by_hour.csv"), row.names = FALSE)
  p_hour <- ggplot(hourly, aes(Hour, mean_rentals)) +
    geom_line() + geom_point() +
    labs(title = "Mean Bike Rentals by Hour", x = "Hour of Day", y = "Mean Rented Bike Count") +
    theme_minimal()
  ggsave(file.path(fig_dir, "eda_demand_by_hour.png"), p_hour, width = 8, height = 5, dpi = 160)
}

# ---------- 4. Simple linear regression ----------
simple_data <- bike |>
  dplyr::select(`Rented Bike Count`, Temperature) |>
  tidyr::drop_na()
simple_model <- lm(`Rented Bike Count` ~ Temperature, data = simple_data)
write.csv(broom::tidy(simple_model, conf.int = TRUE),
          file.path(tab_dir, "simple_regression_coefficients.csv"), row.names = FALSE)
write.csv(broom::glance(simple_model),
          file.path(tab_dir, "simple_regression_model_fit.csv"), row.names = FALSE)

p_simple <- ggplot(simple_data, aes(Temperature, `Rented Bike Count`)) +
  geom_point(alpha = 0.22) +
  geom_smooth(method = "lm", se = TRUE, colour = "firebrick") +
  labs(title = "Simple Linear Regression: Temperature and Demand",
       x = "Temperature (\u00B0C)", y = "Rented Bike Count") +
  theme_minimal()
ggsave(file.path(fig_dir, "simple_regression_temperature.png"), p_simple, width = 8, height = 5, dpi = 160)

# ---------- 5. Multiple linear regression ----------
main_data <- bike |>
  dplyr::select(`Rented Bike Count`, Temperature, Humidity, `Wind speed`) |>
  tidyr::drop_na()
multiple_model <- lm(`Rented Bike Count` ~ Temperature + Humidity + `Wind speed`, data = main_data)
write.csv(broom::tidy(multiple_model, conf.int = TRUE),
          file.path(tab_dir, "multiple_regression_coefficients.csv"), row.names = FALSE)
write.csv(broom::glance(multiple_model),
          file.path(tab_dir, "multiple_regression_model_fit.csv"), row.names = FALSE)
capture.output(summary(multiple_model), file = file.path(tab_dir, "multiple_regression_summary.txt"))

# ---------- 6. Hypothesis tests, confidence intervals, dummy variables, ANOVA ----------
season_data <- bike |>
  dplyr::select(`Rented Bike Count`, Seasons) |>
  tidyr::drop_na()
season_model <- lm(`Rented Bike Count` ~ Seasons, data = season_data)
write.csv(broom::tidy(season_model, conf.int = TRUE),
          file.path(tab_dir, "season_dummy_coefficients.csv"), row.names = FALSE)
write.csv(as.data.frame(anova(season_model)),
          file.path(tab_dir, "anova_season.csv"), row.names = TRUE)

cat_data <- bike |>
  dplyr::select(`Rented Bike Count`, Temperature, Humidity, `Wind speed`, Seasons) |>
  tidyr::drop_na()
adjusted_season_model <- lm(`Rented Bike Count` ~ Temperature + Humidity + `Wind speed` + Seasons,
                            data = cat_data)
write.csv(as.data.frame(car::Anova(adjusted_season_model, type = 2)),
          file.path(tab_dir, "anova_adjusted_model.csv"), row.names = TRUE)

# R's default treatment coding uses the first factor level as the reference.
reference_season <- levels(droplevels(cat_data$Seasons))[1]
writeLines(c(
  paste("Season reference category:", reference_season),
  paste("Multiple model R-squared:", summary(multiple_model)$r.squared),
  paste("Multiple model adjusted R-squared:", summary(multiple_model)$adj.r.squared),
  paste("Multiple model overall F-test p-value:",
        pf(summary(multiple_model)$fstatistic[1],
           summary(multiple_model)$fstatistic[2],
           summary(multiple_model)$fstatistic[3], lower.tail = FALSE))
), file.path(tab_dir, "model_metadata.txt"))

# ---------- 7. Interaction analysis ----------
interaction_data <- bike |>
  dplyr::select(`Rented Bike Count`, Temperature, Seasons) |>
  tidyr::drop_na()
interaction_model <- lm(`Rented Bike Count` ~ Temperature * Seasons, data = interaction_data)
write.csv(broom::tidy(interaction_model, conf.int = TRUE),
          file.path(tab_dir, "interaction_coefficients.csv"), row.names = FALSE)
write.csv(as.data.frame(anova(interaction_model)),
          file.path(tab_dir, "anova_interaction_model.csv"), row.names = TRUE)
p_interaction <- ggplot(interaction_data,
                        aes(x = Temperature, y = `Rented Bike Count`, colour = Seasons)) +
  geom_point(alpha = 0.10) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature-Demand Relationship by Season",
       x = "Temperature (\u00B0C)", y = "Rented Bike Count", colour = "Season") +
  theme_minimal()
ggsave(file.path(fig_dir, "interaction_temperature_season.png"),
       p_interaction, width = 8, height = 5, dpi = 160)

# ---------- 8. Matrix least-squares demonstration ----------
# Deterministic sample of up to 100 complete observations for manageable demonstration.
take <- unique(round(seq(1, nrow(main_data), length.out = min(100, nrow(main_data)))))
matrix_data <- main_data[take, , drop = FALSE]
y <- matrix_data$`Rented Bike Count`
X <- model.matrix(~ Temperature + Humidity + `Wind speed`, data = matrix_data)
beta_hat <- solve(crossprod(X), crossprod(X, y))
beta_lm <- coef(lm(`Rented Bike Count` ~ Temperature + Humidity + `Wind speed`,
                  data = matrix_data))
matrix_table <- data.frame(
  term = names(beta_lm),
  beta_hat_matrix_formula = as.numeric(beta_hat),
  beta_hat_lm = as.numeric(beta_lm),
  difference = as.numeric(beta_hat) - as.numeric(beta_lm)
)
write.csv(matrix_table, file.path(tab_dir, "matrix_least_squares_demo.csv"), row.names = FALSE)

# ---------- 9. Diagnostics and influential observations ----------
# Full diagnostic model includes continuous predictors, season, and interaction.
final_data <- bike |>
  dplyr::select(`Rented Bike Count`, Temperature, Humidity, `Wind speed`, Seasons) |>
  tidyr::drop_na()
final_model <- lm(`Rented Bike Count` ~ Temperature + Humidity + `Wind speed` +
                    Seasons + Temperature:Seasons, data = final_data)
write.csv(broom::tidy(final_model, conf.int = TRUE),
          file.path(tab_dir, "final_model_inference.csv"), row.names = FALSE)
write.csv(broom::glance(final_model),
          file.path(tab_dir, "final_model_fit.csv"), row.names = FALSE)
capture.output(summary(final_model), file = file.path(tab_dir, "final_model_summary.txt"))

png(file.path(fig_dir, "diagnostic_residuals_fitted.png"), width = 1200, height = 800, res = 150)
plot(final_model, which = 1)
dev.off()
png(file.path(fig_dir, "diagnostic_qq.png"), width = 1200, height = 800, res = 150)
plot(final_model, which = 2)
dev.off()
png(file.path(fig_dir, "diagnostic_cooks_distance.png"), width = 1200, height = 800, res = 150)
plot(final_model, which = 4)
dev.off()

bp <- lmtest::bptest(final_model)
dw <- lmtest::dwtest(final_model)
reset <- lmtest::resettest(final_model, power = 2, type = "fitted")
diag_table <- data.frame(
  test = c("Breusch-Pagan test (heteroskedasticity)", "Durbin-Watson test (serial correlation)", "RESET test (functional form)"),
  statistic = c(unname(bp$statistic), unname(dw$statistic), unname(reset$statistic)),
  p_value = c(bp$p.value, dw$p.value, reset$p.value)
)
write.csv(diag_table, file.path(tab_dir, "diagnostic_tests.csv"), row.names = FALSE)
cooks <- cooks.distance(final_model)
influential <- data.frame(row_number = seq_along(cooks), cooks_distance = cooks)
influential <- influential[order(influential$cooks_distance, decreasing = TRUE), ]
write.csv(head(influential, 20), file.path(tab_dir, "largest_cooks_distance.csv"), row.names = FALSE)

# ---------- 10. Console answers for Parts A-J ----------
# This script prints the answers in the Console, displays plots in RStudio's
# Plots pane, and saves the tables/figures to output/. It does NOT create a PDF
# or Markdown report.

fmt_p <- function(p) {
  if (is.na(p)) return("not available")
  if (p < 0.001) "< 0.001" else paste0("= ", signif(p, 4))
}
print_section <- function(title) {
  cat("\n\n", paste(rep("=", 76), collapse = ""), "\n", title, "\n",
      paste(rep("=", 76), collapse = ""), "\n", sep = "")
}
print_table <- function(x, digits = 4) {
  print(as.data.frame(lapply(x, function(col) {
    if (is.numeric(col)) round(col, digits) else col
  })), row.names = FALSE)
}

# Useful inferential objects
simple_sum <- summary(simple_model)
multi_sum <- summary(multiple_model)
final_sum <- summary(final_model)
simple_coef_table <- broom::tidy(simple_model, conf.int = TRUE)
multi_coef_table <- broom::tidy(multiple_model, conf.int = TRUE)
final_coef_table <- broom::tidy(final_model, conf.int = TRUE)
season_coef_table <- broom::tidy(season_model, conf.int = TRUE)
interaction_coef_table <- broom::tidy(interaction_model, conf.int = TRUE)
overall_f <- multi_sum$fstatistic
overall_p <- pf(overall_f[1], overall_f[2], overall_f[3], lower.tail = FALSE)
temp_multi <- multi_coef_table[multi_coef_table$term == "Temperature", , drop = FALSE]
season_anova <- anova(season_model)
season_p <- season_anova$`Pr(>F)`[1]
interaction_joint_test <- anova(
  lm(`Rented Bike Count` ~ Temperature + Seasons, data = interaction_data),
  interaction_model
)
interaction_joint_p <- interaction_joint_test$`Pr(>F)`[2]

# PART A
print_section("PART A - RESEARCH QUESTION AND DATA PREPARATION")
cat("Research question: How are hourly rented-bike counts in Seoul associated with\n",
    "temperature, humidity, wind speed, and season?\n\n", sep = "")
cat("Response variable: Rented Bike Count (quantitative count).\n")
cat("Predictors: Temperature, Humidity, Wind speed (quantitative); Seasons (categorical).\n")
cat("Dataset size:", nrow(bike), "rows and", ncol(bike), "columns.\n")
cat("Duplicate rows:", sum(duplicated(bike)), "\n")
cat("Missing values and data types by variable:\n")
print_table(quality)
cat("\nData quality summary:\n")
print_table(quality_summary)
cat("\nPreparation performed: column names standardized; numeric variables converted\n",
    "to numeric; Date converted to Date where available; categorical variables\n",
    "converted to factors. Models use complete cases for their required variables.\n", sep = "")

# PART B
print_section("PART B - EXPLORATORY DATA ANALYSIS")
cat("Descriptive statistics for numeric variables:\n")
print_table(desc)
cat("\nMean rental count by season:\n")
print_table(season_summary)
if (exists("hourly")) {
  cat("\nMean rental count by hour:\n")
  print_table(hourly)
}
cat("\nInterpretation guide: use the printed summaries and plots to identify the\n",
    "typical rental count, spread, possible extreme values, and whether demand\n",
    "patterns differ by temperature, hour, or season. The plots are displayed below.\n", sep = "")
print(p_temp)
print(p_season)
if (exists("p_hour")) print(p_hour)

# PART C
print_section("PART C - SIMPLE LINEAR REGRESSION")
cat("Model: Rented Bike Count = beta0 + beta1(Temperature) + error.\n\n")
print(simple_sum)
cat("\nCoefficient estimates with 95% confidence intervals:\n")
print_table(simple_coef_table)
cat("\nR-squared:", round(simple_sum$r.squared, 4),
    "| Adjusted R-squared:", round(simple_sum$adj.r.squared, 4), "\n")
cat("Temperature slope interpretation: for each 1\u00B0C increase in temperature,\n",
    "the model estimates an average change of ",
    round(unname(coef(simple_model)["Temperature"]), 3),
    " rented bikes per hour (unadjusted association).\n", sep = "")
print(p_simple)

# PART D
print_section("PART D - MULTIPLE LINEAR REGRESSION")
cat("Model: Rented Bike Count ~ Temperature + Humidity + Wind speed.\n",
    "Each coefficient describes its association with rental count while holding\n",
    "the other predictors in this model constant.\n\n", sep = "")
print(multi_sum)
cat("\nCoefficients and 95% confidence intervals:\n")
print_table(multi_coef_table)
cat("\nR-squared:", round(multi_sum$r.squared, 4),
    "| Adjusted R-squared:", round(multi_sum$adj.r.squared, 4),
    "| Overall F-test p-value ", fmt_p(overall_p), "\n", sep = "")
cat("Overall model test H0: all predictor slopes are zero; H1: at least one slope\n",
    "is nonzero. Use the F-test p-value above to assess evidence against H0.\n", sep = "")

# PART E
print_section("PART E - HYPOTHESIS TESTS AND CONFIDENCE INTERVALS")
cat("For each regression coefficient, H0: beta = 0 versus H1: beta != 0.\n")
cat("Multiple-regression coefficient tests and 95% confidence intervals:\n")
print_table(multi_coef_table)
if (nrow(temp_multi) == 1) {
  cat("\nTemperature in the adjusted model: estimate = ", round(temp_multi$estimate, 3),
      "; 95% CI [", round(temp_multi$conf.low, 3), ", ",
      round(temp_multi$conf.high, 3), "]; p-value ", fmt_p(temp_multi$p.value), ".\n",
      sep = "")
  cat(if (temp_multi$p.value < 0.05)
    "At the 5% significance level, the temperature coefficient is statistically significant.\n"
    else
    "At the 5% significance level, there is not sufficient evidence that the adjusted temperature coefficient differs from zero.\n")
}
cat("Statistical significance should be considered alongside effect size and real-world\n",
    "importance; a small p-value alone does not show that an effect is practically large.\n", sep = "")

# PART F
print_section("PART F - MATRIX FORM AND LEAST-SQUARES ESTIMATION")
cat("Linear model: y = X beta + epsilon.\n",
    "y = vector of observed rented-bike counts; X = design matrix containing an\n",
    "intercept and predictor columns; beta = unknown coefficients; epsilon = errors.\n",
    "Least-squares estimate: beta_hat = (X'X)^(-1)X'y, provided X'X is invertible.\n\n",
    "Comparison of the matrix calculation with lm() on the same subset:\n", sep = "")
print_table(matrix_table)
cat("The differences should be zero or extremely close to zero because both methods\n",
    "calculate the least-squares coefficients on the same observations.\n", sep = "")

# PART G
print_section("PART G - DUMMY VARIABLES AND ANOVA")
cat("Season-only model: Rented Bike Count ~ Seasons.\n")
cat("Reference season under R's treatment coding:", reference_season, "\n")
cat("Dummy-variable coefficients (each season coefficient is compared with the\n",
    "reference season):\n", sep = "")
print_table(season_coef_table)
cat("\nOne-way ANOVA table:\n")
print(season_anova)
cat("ANOVA test H0: mean rental count is equal across all seasons.\n",
    "ANOVA p-value ", fmt_p(season_p), ".\n", sep = "")
cat("\nAdjusted ANOVA (season after accounting for Temperature, Humidity, and Wind speed):\n")
print(car::Anova(adjusted_season_model, type = 2))
cat("\nSeason boxplot:\n")
print(p_season)

# PART H
print_section("PART H - INTERACTION ANALYSIS")
cat("Model: Rented Bike Count ~ Temperature * Seasons.\n",
    "This model allows the temperature slope to vary by season.\n\n",
    "Interaction model coefficients and 95% confidence intervals:\n", sep = "")
print_table(interaction_coef_table)
cat("\nSequential ANOVA table for interaction model:\n")
print(anova(interaction_model))
cat("\nJoint test comparing the model with Temperature + Seasons against the model\n",
    "with Temperature * Seasons (tests whether the interaction terms improve fit):\n", sep = "")
print(interaction_joint_test)
cat("Joint interaction-test p-value ", fmt_p(interaction_joint_p), ".\n", sep = "")
cat("Interaction plot:\n")
print(p_interaction)

# PART I
print_section("PART I - MODEL DIAGNOSTICS")
cat("Diagnostic tests:\n")
print_table(diag_table)
cat("\nBreusch-Pagan test: H0 is constant error variance. p-value ", fmt_p(bp$p.value), ".\n",
    "Durbin-Watson test: assesses serial correlation in residuals; statistic = ",
    round(unname(dw$statistic), 4), ", p-value ", fmt_p(dw$p.value), ".\n",
    "RESET test: checks for possible functional-form problems; p-value ",
    fmt_p(reset$p.value), ".\n", sep = "")
cat("These tests are indicators, not proof that assumptions are satisfied. Review the\n",
    "plots for patterns, nonconstant spread, departures from normality, and influential\n",
    "observations. A small p-value may indicate a potential model issue.\n", sep = "")
cat("\nTop 10 observations by Cook's distance:\n")
print_table(head(influential, 10))
cat("\nResiduals vs fitted plot:\n")
plot(final_model, which = 1)
cat("\nNormal Q-Q plot:\n")
plot(final_model, which = 2)
cat("\nCook's distance plot:\n")
plot(final_model, which = 4)

# PART J
print_section("PART J - FINAL MODEL AND CONCLUSION")
cat("Final model: Rented Bike Count ~ Temperature + Humidity + Wind speed + Seasons\n",
    "+ Temperature:Seasons.\n\n", sep = "")
print(final_sum)
cat("\nFinal-model coefficient estimates and 95% confidence intervals:\n")
print_table(final_coef_table)
cat("\nFinal model R-squared:", round(final_sum$r.squared, 4),
    "| Adjusted R-squared:", round(final_sum$adj.r.squared, 4), "\n")
cat("\nConclusion based on the fitted models:\n")
cat("- The simple model estimates the unadjusted relationship between temperature and demand.\n")
cat("- The multiple model estimates temperature, humidity, and wind-speed associations\n",
    "  while holding the other included predictors constant.\n", sep = "")
cat("- The season ANOVA assesses whether mean rentals differ among seasons.\n")
cat("- The interaction test assesses whether the temperature-demand association differs by season.\n")
cat("- The final model combines weather, season, and their temperature-by-season interaction.\n")
cat("- These are observational associations and should not be interpreted as proof of causation.\n")
cat("Use the coefficient estimates, confidence intervals, p-values, R-squared values, and\n",
    "diagnostic plots printed above to state which predictors show evidence of association\n",
    "and whether the model appears adequate for the assignment's research question.\n", sep = "")

