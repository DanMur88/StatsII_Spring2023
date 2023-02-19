#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.
# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

# Inpect dataset
head(graduation)

# Run OLS model with hsgrad regressed on all other variables
ols_base <- lm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + 
                 nsibs + intact, data = graduation)

# Check regression coefficients
print(ols_base$coefficients)

# Run logit model with hsgrad regressed on all other variables
logit_base <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + 
                    nsibs + intact, data = graduation, 
                  family = binomial(link="logit")) 
# Can also use "." in place of all input variables as omnibus selector

summary(logit_base)

# Carry out likelihood ratio test

# Create null model
nullMod <- glm(hsgrad ~ 1, # 1 = fit an intercept only (i.e. a sort of mean)
               data = graduation,
               family = "binomial")

# Use anova function to do likelihood ratio test
anova(nullMod, logit_base, test="LRT")

# Extract confidence intervals of coefficients
exp(confint(logit_base)) # need to transform to odds ratio by exponentiation

# Make data.frame of confidence intervals and coefficients
confMod <- data.frame(cbind(lower = exp(confint(logit_base)[,1]),
                            coeffs = exp(coef(logit_base)),
                            upper = exp(confint(logit_base)[,2])))

# Use dataframe to make a plot


# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?

# Convert "nsibs" to factor
graduation$nsibs <- as.factor(graduation$nsibs)

# Carry out likelihood ratio test
logit_2 <- glm(hsgrad ~ ., data = graduation, 
                  family = binomial(link="logit")) 

summary(logit_2)
summary(logit_base)

anova(nullMod, logit_2, test="LRT")
