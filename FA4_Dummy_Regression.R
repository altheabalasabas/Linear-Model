# ============================================================
# APM1205 - FORMATIVE ASSESSMENT 4
# Dummy-Variable Regression
# ============================================================

# Load package
library(ggplot2)

# Load diamonds dataset
data(diamonds)

# Convert cut from ordered factor to regular factor
diamonds$cut <- factor(diamonds$cut)

# Set Ideal as the reference category
diamonds$cut <- relevel(diamonds$cut, ref = "Ideal")


# ============================================================
# PART A - EXPLORING AND PREPARING THE DATA
# ============================================================

# View the data
head(diamonds)

# Structure of the data
str(diamonds)

# Summary statistics
summary(diamonds)

# Number of observations
nrow(diamonds)

# Number of variables
ncol(diamonds)

# Response variable
summary(diamonds$price)

# Quantitative explanatory variable
summary(diamonds$carat)

# Categorical explanatory variable
levels(diamonds$cut)

# Number of observations in each cut category
table(diamonds$cut)


# ============================================================
# PART B - DUMMY VARIABLES
# ============================================================

# Create dummy variables
diamonds$Fair <- ifelse(diamonds$cut == "Fair", 1, 0)

diamonds$Good <- ifelse(diamonds$cut == "Good", 1, 0)

diamonds$VeryGood <- ifelse(diamonds$cut == "Very Good", 1, 0)

diamonds$Premium <- ifelse(diamonds$cut == "Premium", 1, 0)

# Display dummy-variable coding
head(
  diamonds[, c(
    "cut",
    "Fair",
    "Good",
    "VeryGood",
    "Premium"
  )]
)


# ============================================================
# PART C - ADDITIVE DUMMY-VARIABLE REGRESSION
# ============================================================

# Fit additive regression model
model1 <- lm(
  price ~ carat + cut,
  data = diamonds
)

# Regression results
summary(model1)

# Regression coefficients
coef(model1)

# R-squared
summary(model1)$r.squared


# ============================================================
# PART D - INTERACTION BETWEEN CARAT AND CUT
# ============================================================

# Fit interaction model
model2 <- lm(
  price ~ carat * cut,
  data = diamonds
)

# Regression results
summary(model2)

# Interaction coefficients
summary(model2)$coefficients

# Compare additive and interaction models
anova(model1, model2)


# ============================================================
# PART E - VISUALIZATION
# ============================================================

# Create figures folder if it does not exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Create graph
p <- ggplot(
  diamonds,
  aes(
    x = carat,
    y = price,
    color = cut
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Diamond Price versus Carat by Cut",
    x = "Carat",
    y = "Price (US Dollars)"
  ) +
  theme_minimal()

# Display graph
p

# Save graph
ggsave(
  filename = "figures/diamond_price_vs_carat_by_cut.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)

