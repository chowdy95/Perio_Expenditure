# This file aims to predict the proportion of dental expenditure spent on periodontitis. This is reported for
# only 26 countries and unreported for the other 172 countries
# This proportion above is saved as results in the csv

# renv related commands for my convenience
# renv::init()
# renv::install("cowplot")
# renv::status()
# renv::snapshot(type = "all")
# renv::restore()

#' 1. Importing data into R
GBD <- readr::read_csv("./data/GBD.csv",
  col_names = TRUE,
  show_col_types = TRUE,
  col_types = list(
    "direct" = readr::col_factor(),
    "adv_coverage" = readr::col_factor()
  )
)

# Setting up data distribution to facilitate prediction after regression
dd <- rms::datadist(GBD)
options(datadist = "dd")


#' 2. Descriptive statistics for data

d <- GBD %>%
  select(
    rowname, results, perio_prop_oral, perio_prev, decid_caries, perm_caries,
    edent, diab_prev, diab_death, smoking_agest, dentists, dent_personnel,
    dent_pers_excl_dentists, gdp_usd, dent_exp,
    dent_exppc, # selecting only mean predictor values, excluding upper/lower CIs
    adv_coverage, coverage
  ) %>%
  Hmisc::describe()

d
plot(d, bvspace = 0.2)

# All predictors have 0 missing values, except for dent_exp and dent_exppc with 2 missing values,
# adv_coverage with 85 missing values, and coverage with 92 missing values
# dent_exp is very right skewed with a huge spike on the left therefore may be less informative


#' 3. Dimension reduction

# There are 16 candidate predictors, but only 26 observations for perio expenditure, which
# suggests there is only sufficient data to support the use of 3-4 predictors

# Therefore we must eliminate at least 12 candidate predictors

# We start by excluding predictors based on distribution, missingness and subject matter knowledge

# There were 3 latent factors to be modeled by the predictors: treatment need (as modeled by
# perio_prop_oral, perio_prev, decid_caries, perm_caries, edent), dental utilisation (as modeled
# by dent_exppc and dent_personnel) and cost effectiveness of treatment (as modeled by diab_death,
# diab_prev and smoking_agest)

# However, prior knowledge suggests that the limiting factors are likely overall dental utilisation
# for periodontitis patients and treatment need, rather than cost effectiveness of treatment, given
# the prevalence of untreated, partially treated, or undiagnosed periodontitis
# Therefore, we exclude the 3 predictors associated with cost effectiveness (diab_death, diab_prev
# and smoking_agest)

# adv_coverage and coverage have significant amounts of missing data and therefore were excluded

# gdp and dent_exp had very right skewed distributions which might be expected to be less
# informative, and this would not improve with data transformation as it is absolute dental
# expenditure, not relative differences, that drive differences in perio expenditure

# perio-prop_oral is the prevalence of periodontitis as a proportion of the sum
# of prevalence for all oral conditions. As such, we can exclude the prevalence
# of other oral conditions

# However, an exploratory analysis demonstrated that periodontitis prevalence, which was expected to
# be the single strongest predictor, had almost no predictive effect on periodontitis expenditure

# Model f2 includes all predictors except periodontitis prevalence
f2 <- rms::ols(
  results ~ decid_caries + perm_caries + edent + diab_prev + diab_death +
    smoking_agest + dentists + dent_personnel + gdp_usd + dent_exp + dent_exppc,
  data = GBD,
  x = TRUE, y = TRUE
)

# Model f includes all predictors, including periodontitis prevalence
f <- rms::ols(
  results ~ perio_prev + decid_caries + perm_caries + edent + diab_prev + diab_death +
    smoking_agest + dentists + dent_personnel + gdp_usd + dent_exp + dent_exppc,
  data = GBD,
  x = TRUE, y = TRUE
)

# Comparing the chi2 likelihood ratio test for f2 (18.83) vs the chi2 likelihood ratio test for f (19.68),
# the effect of adding periodontitis prevalence as a predictor is only 0.85, which suggests
# that periodontitis prevalence does not predict proportion spent on perio well
f
f2

# This leaves us with the final set of 3 predictors: dentists, dent_personnel and dent_exppc

# Variable clustering was then done to assess correlations between predictors
vc <- varclus(~ dent_personnel + dentists + dent_exppc, data = GBD)
plot(vc)
vc

#' 4. No transformation or imputation necessary

#' 5. Linear regression

f6 <- ols(results ~ dentists + dent_personnel + dent_exppc, data = GBD_trans, x = TRUE, y = TRUE)

# These 2 charts graphically show the effect size of the various predictors across the interquartile range
ggplot(Predict(f6))
plot(summary(f6))

# This chart graphically shows the strength of the various predictors, and number of dentists, dental expenditure,
# prevalence of permanent caries, and number of dental personnel have a stronger effect, after which there is a
# significant dropoff
plot(anova(f6))

#' 6. Diagnostics

# Assessment of collinearity
vif(f6)
# Collinearity appears to be acceptable, VIF is below 5 for all predictors

# This diagnostic test shows residuals that are somewhat normally distributed around the fitted values
plot(f6, which = 1)
# Residuals do not appear to be normally distributed

# This Q-Q plot does not demonstrate any clear abnormalities
r5 <- residuals(f6, type = "student")
qqnorm(r5)
qqline(r5)

# Now we look at influential observations
temp <- data.frame(rowname = 1:195)
temp$dffits <- residuals(f6, type = "dffits")

ggplot(temp, aes(x = rowname, y = dffits)) +
  geom_point() +
  geom_text(aes(label = GBD$rowname, vjust = -1.0))
# While Norway is the most influential datapoint, it does not appear to be a huge outlier

#' 7. Validation
val <- validate(f6, B = 1000)
val
# Focusing on the MSE, the performance of the model appears poor. The RMSE would be ~0.07, which is almost
# 58% of the data range, which is approximately between 0 to 0.12

plot(calibrate(f6, B = 1000))
# the calibration plot is s shaped rather than linear, which suggests that there may be nonlinear effects or
# influential outliers
# However, we lack sufficient observations to investigate nonlinear effects

#' 8. Prediction
new_data <- GBD %>% select(dentists, dent_personnel, dent_exppc, )
GBD_pred <- predict(f6, new_data, conf.int = 0.95, se.fit = FALSE) %>%
  as.tibble()

GBD_pred <- GBD_pred %>%
  mutate(results = GBD$results, rowname = GBD$rowname) %>%
  relocate(rowname, results, .before = everything())
# To combine transformed data with the response variable and country names
