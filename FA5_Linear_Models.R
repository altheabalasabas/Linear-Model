# ============================================================
# APM1205 - FORMATIVE ASSESSMENT 5
# PALMER PENGUINS
# ============================================================

# PART A: DATA PREPARATION -------------------------------------
dir.create("data", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/main/inst/extdata/penguins.csv"

if (!file.exists("data/penguins.csv")) {
  download.file(url, "data/penguins.csv", mode = "wb")
}

penguins <- read.csv("data/penguins.csv", na.strings = c("NA", ""))
cat("\n--- Dataset structure ---\n")
str(penguins)
cat("\n--- Dataset summary ---\n")
print(summary(penguins))
cat("\n--- Missing values by variable ---\n")
print(colSums(is.na(penguins)))

penguins_clean <- subset(
  penguins,
  !is.na(body_mass_g) & !is.na(species) & !is.na(sex)
)
penguins_clean$species <- factor(
  penguins_clean$species,
  levels = c("Adelie", "Chinstrap", "Gentoo")
)
penguins_clean$sex <- factor(
  penguins_clean$sex,
  levels = c("female", "male")
)
cat("\nFinal sample size =", nrow(penguins_clean), "\n")

# PART B: DESCRIPTIVE STATISTICS -------------------------------
cat("\n--- Overall body mass statistics ---\n")
overall_stats <- data.frame(
  n = length(penguins_clean$body_mass_g),
  mean = mean(penguins_clean$body_mass_g),
  sd = sd(penguins_clean$body_mass_g),
  minimum = min(penguins_clean$body_mass_g),
  maximum = max(penguins_clean$body_mass_g)
)
print(round(overall_stats, 2))

summarize_mass <- function(x) {
  c(n = length(x), mean = mean(x), sd = sd(x))
}
cat("\n--- By species ---\n")
species_stats <- aggregate(
  body_mass_g ~ species, data = penguins_clean, FUN = summarize_mass
)
species_stats <- data.frame(
  species = species_stats$species,
  n = species_stats$body_mass_g[, "n"],
  mean = species_stats$body_mass_g[, "mean"],
  sd = species_stats$body_mass_g[, "sd"]
)
print(species_stats, row.names = FALSE)

cat("\n--- By sex ---\n")
sex_stats <- aggregate(
  body_mass_g ~ sex, data = penguins_clean, FUN = summarize_mass
)
sex_stats <- data.frame(
  sex = sex_stats$sex,
  n = sex_stats$body_mass_g[, "n"],
  mean = sex_stats$body_mass_g[, "mean"],
  sd = sex_stats$body_mass_g[, "sd"]
)
print(sex_stats, row.names = FALSE)

cat("\n--- By species and sex ---\n")
cell_stats <- aggregate(
  body_mass_g ~ species + sex, data = penguins_clean, FUN = summarize_mass
)
cell_stats <- data.frame(
  species = cell_stats$species,
  sex = cell_stats$sex,
  n = cell_stats$body_mass_g[, "n"],
  mean = cell_stats$body_mass_g[, "mean"],
  sd = cell_stats$body_mass_g[, "sd"]
)
print(cell_stats, row.names = FALSE)

# Make sure the figures folder exists
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

# Figure 1: Body mass by species
png("figures/body_mass_by_species.png",
    width = 1000, height = 700)

boxplot(
  body_mass_g ~ species,
  data = penguins_clean,
  xlab = "Penguin Species",
  ylab = "Body Mass (g)",
  main = "Penguin Body Mass by Species"
)

dev.off()


# Figure 2: Body mass by sex
png("figures/body_mass_by_sex.png",
    width = 1000, height = 700)

boxplot(
  body_mass_g ~ sex,
  data = penguins_clean,
  xlab = "Sex",
  ylab = "Body Mass (g)",
  main = "Penguin Body Mass by Sex"
)

dev.off()

# Figure 3: Species and sex interaction plot
png("figures/species_sex_interaction.png",
    width = 1000, height = 700)

interaction.plot(
  x.factor = penguins_clean$species,
  trace.factor = penguins_clean$sex,
  response = penguins_clean$body_mass_g,
  fun = mean,
  type = "b",
  pch = c(16, 17),
  xlab = "Penguin Species",
  ylab = "Mean Body Mass (g)",
  trace.label = "Sex",
  main = "Interaction Plot: Species and Sex"
)

dev.off()


# Check whether the images were created
list.files("figures", pattern = "\\.png$")

# PART C: ONE-WAY ANOVA ----------------------------------------
cat("\n--- Part C: One-Way ANOVA ---\n")
oneway_model <- aov(body_mass_g ~ species, data = penguins_clean)
print(summary(oneway_model))

# PART D: DUMMY-VARIABLE REGRESSION ----------------------------
cat("\n--- Part D: Dummy-variable regression ---\n")
# Adelie is the reference category because it is the first factor level.
dummy_model <- lm(body_mass_g ~ species, data = penguins_clean)
print(summary(dummy_model))
cat("Reference category: Adelie\n")

# PART E: TWO-WAY ANOVA WITH INTERACTION -----------------------
cat("\n--- Part E: Two-Way ANOVA ---\n")
twoway_model <- aov(body_mass_g ~ species * sex, data = penguins_clean)
print(summary(twoway_model))

# PART F: INTERACTION ANALYSIS ---------------------------------
cat("\n--- Part F: Cell means ---\n")
print(with(penguins_clean, tapply(body_mass_g, list(species, sex), mean)))

png("figures/species_sex_interaction.png", width = 1000, height = 700)
interaction.plot(
  x.factor = penguins_clean$species,
  trace.factor = penguins_clean$sex,
  response = penguins_clean$body_mass_g,
  fun = mean, type = "b", pch = c(16, 17),
  xlab = "Penguin Species", ylab = "Mean Body Mass (g)",
  trace.label = "Sex",
  main = "Interaction Plot: Species and Sex"
)
dev.off()

# PART G: FULL REGRESSION MODEL -------------------------------
cat("\n--- Part G: Full regression model ---\n")
full_model <- lm(body_mass_g ~ species * sex, data = penguins_clean)
print(summary(full_model))
cat("\nSequential ANOVA for the full model:\n")
print(anova(full_model))

cat("\nAnalysis completed. Check the figures/ folder for saved plots.\n")
