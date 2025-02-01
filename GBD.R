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

GBD_sim <- column_to_rownames(GBD) %>% 
  select(-starts_with(c("Upper","Lower")))

view(GBD_sim)


dd <- datadist(GBD_sim);options(datadist='dd')

f_ignore <- ols(results ~ direct + perio_prev+decid_caries+perm_caries+edent+diab_prev+diab_death+smoking_agest+dentists+dent_personnel+gdp_usd+dent_exp+dent_exppc+adv_coverage+coverage, data=GBD_sim)

f2 <- ols(results ~ decid_caries+perm_caries+edent+diab_prev+diab_death+smoking_agest+dentists+dent_personnel+gdp_usd+dent_exp+dent_exppc, data=GBD_sim, x=TRUE)

f <- ols(results ~ perio_prev+decid_caries+perm_caries+edent+diab_prev+diab_death+smoking_agest+dentists+dent_personnel+gdp_usd+dent_exp+dent_exppc, data=GBD_sim, x=TRUE)
anova(f)
ggplot(Predict(f))
anova(f2)
summary(f)

f
f2
plot(anova(f))
plot(summary(f))
plot(f, which=1)

r <- residuals(f, type = "student")
qqnorm(r)
qqline(r)

f3 <- ols(results ~ perm_caries+dentists+dent_personnel+dent_exppc, data=GBD_sim, x=TRUE)
ggplot(Predict(f3))
plot(summary(f3))
plot(f3, which=1)

f3

r2 <- residuals(f3, type = "student")
qqnorm(r2)
qqline(r2)
