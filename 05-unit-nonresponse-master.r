### -----------------------------------------------------
### SEMINAR: introduction to survey methodology
### TEACHER: simon munzert
### SESSION: unit nonresponse
### -----------------------------------------------------


### preparations ----------------------------------------

# clear workspace
rm(list=ls(all=TRUE))

# load packages
library(survey)
library(samplingbook)
library(plyr)
library(dplyr)
library(foreign)
library(arm)



### Initial thoughts on nonresponse -------

# Q: What are types of nonresponse in population surveys?
# Q: What are potential reasons for these types of nonresponse?
# Q: What are means to cope with these nonresponse issues?
# Q: MCAR, MAR, and NMAR are different assumptions about missingness. Discuss which of the assumptions is most difficult to deal with!



### Method of bounds ----------------------

# idea: assessing the logical bounds of a sample statistic of interest
# assume extreme values for unobserved fraction of the population and estimate the bounds

# Build a function for the case of proportions that operates on a given response rate and an observed fraction among respondents:

boundsFun <- function(resp_rate, p_obs) {
  p_min <- resp_rate * p_obs
  p_max <- resp_rate * p_obs + (1 - resp_rate)
  cat(paste0("Min. possible proportion: ", p_min, "\n", "Max. possible proportion: ", p_max))
}

boundsFun(resp_rate = .10, p_obs = .5)

# Q: Why is the method of bounds often of limited use to assess a sample statistic of interest?
# Q: Can you think of circumstances under which even the method of bounds misses the true population parameter?


### Poststratification ---------------------------

# Q: What is the idea behind poststratification?
# Q: What assumption of missingness applies to the standard poststratification approach?

# recall stratified sampling:
  # divide population into exhaustive groups
  # draw SRS within each group
  # estimate of population totals is improved by removing variability between groups, as the contribution of each group is fixed by design
# poststratification
  # now imagine that the group sizes are known but the sample was not stratified
  # post-stratification adjusts sampling weights so that the estimated population group sizes are correct (equal to known group sizes)
  # sampling weights 1/pi_i are replaced by weights g_i/pi_i , where g_i = N_h/hat(N_h)
  # note, however, that if we sample no one from a group h, it is not possible to perform re-weighting
  # possible solution: merge two post-strata

# API example
data(api)
clus2_design <- svydesign(id = ~dnum + snum, fpc = ~fpc1 + fpc2, data = apiclus2)
clus2_design
table(apipop$stype)
table(apiclus2$stype)
prop.table(table(apipop$stype))
prop.table(table(apiclus2$stype))
pop_types <- data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018)) # provide population size information
ps_design <- postStratify(clus2_design, strata = ~stype, population = pop_types)  
ps_design

svymean(~enroll, clus2_design, na.rm = TRUE)
svymean(~enroll, ps_design, na.rm = TRUE)
# substantive difference in mean and s.e. estimate because of undersampling of elementary schools (and high intra-class correlation). 
?api
svymean(~grad.sch, clus2_design)
svymean(~grad.sch, ps_design)

# on the other hand, hardly any between-variance regarding percent parents with postgraduate education between school types -> post-stratification does not help too much here



### Raking (Iterative proportional fitting) ----------

# good example how a raking algorithm works in practice: 
browseURL("http://en.wikipedia.org/wiki/Iterative_proportional_fitting#Example")

clus1_design <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
summary(clus1_design)   
pop_types <- data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018))
# sch.wide variable: met school-wide growth target?
pop_schwide <- data.frame(sch.wide = c("No", "Yes"), Freq=c(1072, 5122))
# raking
clus1_design_rake <- rake(clus1_design, list(~stype, ~sch.wide), list(pop_types, pop_schwide))
summary(clus1_design_rake)
svymean(~grad.sch, clus1_design)
svymean(~grad.sch, clus1_design_rake)

svymean(~enroll, clus1_design)
svymean(~enroll, clus1_design_rake)




### Imputation -----------------------------------------

# overview of R implementations of survey methodology tools:
browseURL("http://cran.r-project.org/web/views/OfficialStatistics.html")

# example dataset
soep <- read.dta("data/soep.dta")
nrow(soep)
summary(soep$income)
summary(soep$edu)
summary(soep$yedu)
display(lm(income ~ edu, data = soep)) # display command from the arm package

# usually, R performs listwise deletion/complete case analysis
# we can perform mean imputation or best guess/logical imputation manually
# further, there are functions for single (deterministic and stochastic) regression as well as multiple imputation

# helper function: create a complete data vector using imputed values
impute <- function (a, a.impute){
  ifelse (is.na(a), a.impute, a)
}


# logical imputation
# ------------------
summary(soep$emp)
soep$age <- 1997 - soep$ybirth
young <- soep$age < 18

soep$emp_imputed <- soep$emp
soep$emp_imputed[young & is.na(soep$emp)] <- "not employed"

table(soep$emp, soep$emp_imputed)
table(soep$emp, soep$emp_imputed, useNA = "always")


# deterministic regression imputation
# ------------------

summary(soep$income)
model_out <- lm(income ~ age + sex + emp + rooms + edu, data = soep)
display (model_out)
summary(model_out)
pred <- predict(model_out, soep)
summary(pred)
soep$income_imp <- impute(soep$income, pred)
summary(soep$income_imp)
summary(soep$income)

# Q: What do you think - what are the advantages and disadvantages of regression imputation?



# stochastic regression imputation
# ------------------

display(model_out)
sigma.hat(model_out)
pred_stochastic <- pred + rnorm(length(pred), 0, sigma.hat(model_out)/20)

plot(pred, pred_stochastic)
abline(lm(pred_stochastic~pred), col ="red", lwd = 2)
# Q: How would you proceed next?



# Hot-decking / Matching 
# ------------------

# Q: What is the idea behind hot-decking?
  # expression "hot deck" dates back to data storage on punched cards (Wikipedia)
  # "last observation carried forward"

library(VIM)
?hotdeck

soep_hotdeck <- hotdeck(soep, variable = "income", ord_var = c("emp", "sex", "edu"), imp_var = TRUE, imp_suffix = "i")
summary(soep_hotdeck)
table(soep_hotdeck$income_i)
summary(soep$income)
summary(soep_hotdeck$income)

soep_hotdeck <- arrange(soep_hotdeck, income)
soep_hotdeck$id <- order(soep_hotdeck$income)
plot(soep_hotdeck$id, soep_hotdeck$income)
points(soep_hotdeck$id[soep_hotdeck$income_i], soep_hotdeck$income[soep_hotdeck$income_i], col = rgb(1,0,0,.5), cex = .7)

