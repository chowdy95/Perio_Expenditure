#This file aims to predict the proportion of dental expenditure spent on periodontitis. This is reported for 
#only 25 countries and unreported for the other 173 countries
#This proportion above is saved as results in the csv

#renv related commands for my convenience
#renv::init()
#renv::install("rms","tidyverse")
#renv::status()
renv::snapshot(type = "all")
#renv::restore()


# to restore()# to read csv into R
GBD <- readr::read_csv("GBD.csv",
  col_names = TRUE,
  show_col_types = TRUE,
  col_types = list(
    "direct" = readr::col_factor(),
    "adv_coverage" = readr::col_factor()
  )
) 

dd <- rms::datadist(GBD)
options(datadist = "dd")

#f_ignore <- ols(
#  results ~ direct + perio_prev + decid_caries + perm_caries + edent + 
#    diab_prev + diab_death + smoking_agest + dentists + dent_personnel + 
#    gdp_usd + dent_exp + dent_exppc + adv_coverage + coverage, 
#  data = GBD)

#This part involves exploratory analysis of the importance of periodontitis prevalence to the proportion of dental
#expenditure spent on periodontitis
#Periodontitis prevalence was expected to be the strongest predictor. However, this is not the case as seen below

#Model f2 includes all predictors except periodontitis prevalence
f2 <- rms::ols(
  results ~ decid_caries + perm_caries + edent + diab_prev + diab_death + 
    smoking_agest + dentists + dent_personnel + gdp_usd + dent_exp + dent_exppc, 
  data = GBD, 
  x = TRUE, y = TRUE)

#Model f includes all predictors, including periodontitis prevalence
f <- rms::ols(
  results ~ perio_prev + decid_caries + perm_caries + edent + diab_prev + diab_death + 
    smoking_agest + dentists + dent_personnel + gdp_usd + dent_exp + dent_exppc, 
  data = GBD, 
  x = TRUE, y = TRUE)

#Comparing the chi2 likelihood ratio test for f2 (18.83) vs the chi2 likelihood ratio test for f (19.68),
#the effect of adding periodontitis prevalence as a predictor is only 0.85, which suggests
#that periodontitis prevalence does not predict proportion spent on perio well
f
f2

#This chart graphically shows the strength of the various predictors, and it appears from the slopes
#that most predictors do not have a strong effect. However I have excluded it as the following chart
#is able to demonstrate this more clearly
#ggplot(Predict(f))

#This chart graphically shows the strength of the various predictors, and number of dentists, dental expenditure,
#prevalence of permanent caries, and number of dental personnel have a stronger effect, after which there is a
#significant dropoff
plot(anova(f))

#Testing variance inflation factors to detect collinearity
rms::vif(f)

#This chart demonstrates the direction and magnitude of the effect of the various predictors as well.
#The increased uncertainty and opposite direction of dentists and dental personnel, as well as a priori
#the fact that dentists contribute to the number of dental personnel, suggests significant collinearity
#and the need for dimensional reduction
plot(summary(f))

#These diagnostics investigate any misfit of the regression above but since we are not planning to use this as
#the final regression, I have left them in comment form
#plot(f, which = 1)
#r <- residuals(f, type = "student")
#qqnorm(r)
#qqline(r)


#This regression narrows down the predictors to the four predictors with the strongest effect, as selected
#from the previous regression with a complete model
f3 <- ols(results ~ perm_caries + dentists + dent_personnel + dent_exppc, data = GBD, x = TRUE)
f3

#These 2 charts graphically show the effect size of the various predictors across the interquartile range
ggplot(Predict(f3))
plot(summary(f3))

#This diagnostic test shows normally distributed residuals around the fitted values
plot(f3, which = 1)

#This Q-Q plot does not demonstrate any clear abnormalities
r2 <- residuals(f3, type = "student")
qqnorm(r2)
qqline(r2)


##A more systematic approach

#1. Descriptive statistics for data

#2. Dimension reduction

#There are 15 candidate predictors, but only 26 observations for perio expenditure, which 
#suggests there is only sufficient data to support the use of 3 predictors

#Therefore we must eliminate 12 candidate predictors
#This dimension reduction may be done through the use of variable clustering or PCA
#Variable clustering was selected for better iinterpretabiliity, and also because it was
#felt that the dataset was too small to support the complexity of PCA

#We start by excluding predictors based on missingness and subject matter knowledge
#perio-prop_oral is the prevalence of periodontitis as a proportion of the sum
#of prevalence for all oral conditions. As such, we can exclude the prevalence
#of other oral conditions
#adv_coverage and coverage also have significant amounts of missing data and
#therefore were excluded
#dent_exp is also redundant as dent_exppc likely encodes all relevant information

#This leaves us with 9 predictors, of which we must reduce to 3
#We start with hierarchical clustering to assess highly correlated variables

vc <- varclus(~perio_prop_oral + perio_prev + diab_prev + diab_death + 
          smoking_agest + dentists + dent_personnel + gdp_usd +
          dent_exppc, data = GBD)
plot(vc)

#Looking at dendrogram, we could select one predictor from each pair:
#perio_prev/perio_prev_oral, dentists/dent_personnel, gdp_usd/dent_exppc
#since they are strongly correlated with each other
#Based on subject matter knowledge, we would choose perio_prev_oral, dent_personnel
#and dent_exppc
#This leaves us with 6 variables (perio_prev_oral, dent_personnel, dent_exppc, 
#diab_death, diab_prev and smoking_agest)

#The final selection of predictors is determined by subject matter knowledge.
#There were 4 latent factors to be modeled by the predictors: treatment need (as modeled by
#perio_prop_oral), dental utilisation (as modeled by dent_exppc and dent_personnel) and
#cost effectiveness of treatment (as modeled bydiab_death, diab_prev and smoking_agest) 

#However, prior knowledge suggests that the limiting factors are likely overall dental utilisation 
#for periodontitis patients and treatment need, rather than cost effectiveness of treatment, given
#the prevalence of untreated, partially treated, or undiagnosed periodontitis

#Therefore, the 3 predictors relating to cost effectiveness of treatment were eliminated from the
#model, leaving the final 3 predictors:: dent_exppc, dent_personnel and perio_prop_oral


#3. Optimal transformation of 3 selected predictors to minimise collinearity, account for skewed 
#data and for single imputation

#Single imputation selected as for the predictors used, the number of missing values 
#is less than 4% of data points, so multiple imputation is unlikely to offer significant benefits

#These transformations only consider relationships between the variables, and do not consider the
#relationship to the outcome variable, hence does not increase the risk of overfitting

trans_model <- transcan(~perio_prop_oral + perio_prev + diab_prev + diab_death + 
                          smoking_agest + dentists + dent_personnel + gdp_usd +
                          dent_exppc, imputed = TRUE, 
                      transformed = TRUE, pl= FALSE,
                      trantab = TRUE, data = GBD)

#This plot would show the transformations for the
#ggplot(trans_model, scale=TRUE) +
#  theme(axis.text.x=element_text(size=6))

GBD_trans <- predict(trans_model, GBD, type  = "transformed") %>%  #To extract transformed data from the trans_model
  as_tibble()                 

GBD_trans <- GBD_trans %>% mutate(results = GBD$results,rowname = GBD$rowname) %>%
  relocate(rowname, results, .before = everything())      #To combine transformed data with the response variable and country names


#4. Linear regression

f4 <- ols(results ~ perio_prop_oral + dent_personnel + dent_exppc, data = GBD_trans, x = TRUE)
f4

#These 2 charts graphically show the effect size of the various predictors across the interquartile range
ggplot(Predict(f4))
plot(summary(f4))
vif(f4)

#This diagnostic test shows normally distributed residuals around the fitted values
plot(f4, which = 1)

#This Q-Q plot does not demonstrate any clear abnormalities
r3 <- residuals(f4, type = "student")
qqnorm(r3)
qqline(r3)
