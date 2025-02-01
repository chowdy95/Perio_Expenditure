install.packages('rms')
library(rms)

install.packages('tidyverse')
library(ggplot2)
library(tidyverse)
read_csv("GBD.csv")

GBD <- read_csv("GBD.csv",show_col_types = TRUE,
                col_types = list(
                  'direct'=col_factor(),
                  'adv_coverage'=col_factor()
                ))
view(GBD)

GBD_sim <- GBD %>% 
  select(-starts_with(c("Upper","Lower")))

dd <- datadist(GBD_sim);options(datadist='dd')

f <- ols(results ~ direct + perio_prev+decid_caries+perm_caries+edent+diab_prev+diab_death+smoking_agest+dentists+dent_personnel+gdp_usd+dent_exp+dent_exppc+adv_coverage+coverage, data=GBD_sim)

f2 <- ols(results ~ perio_prev, data=GBD_sim, x= TRUE)

f <- ols(results ~ perio_prev+decid_caries+perm_caries+edent+diab_prev+diab_death+smoking_agest+dentists+dent_personnel+gdp_usd+dent_exp+dent_exppc, data=GBD_sim, x=TRUE)
anova(f)
ggplot(Predict(f))
anova(f2)
summary(f)

f
f2
