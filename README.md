# APM1205 Formative Assessment 5
## Palmer Penguins
Althea Fiona Balasabas


This repository contains my completed Formative Assessment 5 on linear models using the palmerpenguins dataset in R/RStudio

### Research question
Does penguin body mass differ according to species and sex, and does the effect of species on body mass depend on sex?

### Dataset
The Palmer Penguins dataset contains measurements of Adelie, Chinstrap, and Gentoo penguins from the Palmer Archipelago, Antarctica. The response variable is `body_mass_g`; the categorical explanatory variables are `species` and `sex`.

### Dataset source
https://github.com/allisonhorst/palmerpenguins  
CSV: https://raw.githubusercontent.com/allisonhorst/palmerpenguins/main/inst/extdata/penguins.csv

### Variables used
- `body_mass_g`: quantitative response variable, in grams
- `species`: categorical predictor (Adelie, Chinstrap, Gentoo)
- `sex`: categorical predictor (female, male)

### Statistical methods
- Data cleaning and descriptive statistics
- One-Way ANOVA
- Dummy-variable linear regression, with Adelie as the reference category
- Two-Way ANOVA with species, sex, and their interaction
- Full linear regression model with interaction
- Interaction plot

### Brief summary of findings
The analysis examines the differences in penguin body mass according to species and sex using descriptive statistics, One-Way ANOVA, Two-Way ANOVA, and regression models. The descriptive statistics and visualizations are used to compare the average body mass of Adelie, Chinstrap, and Gentoo penguins, as well as male and female penguins. The Two-Way ANOVA and interaction analysis determine whether the effects of species and sex on body mass are statistically significant and whether the difference in body mass between males and females varies across species. The final conclusions are based on the calculated statistical results and the observed patterns in the dataset.

### Reproducing the analysis
1. Open the project folder in RStudio.
2. Open `R/FA5_Linear_Models.R`.
3. Set the working directory to the project root folder (the folder containing `data`, `R`, and `figures`).
4. Run the script from beginning to end. If `data/penguins.csv` is absent, the script downloads it from the official dataset source.
5. The script saves the required figures in `figures/`.

All analysis was completed using R/RStudio.
