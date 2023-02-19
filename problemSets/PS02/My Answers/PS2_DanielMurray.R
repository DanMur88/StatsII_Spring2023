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

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

# Inspect data
head(climateSupport)
summary(climateSupport)
str(climateSupport)

# Change sanctions variable to unordered factor and re-order to set 5% as reference
climateSupport$sanctions <- factor(climateSupport$sanctions, 
                                   ordered=FALSE)
climateSupport$sanctions <- relevel(climateSupport$sanctions, 
                                    ref="5%")

# Run logit model with choice regressed on all other variables
logit_mod <- glm(choice ~ .,
                 data=climateSupport,
                 family=binomial(link="logit"))

# Check summary output of model
summary(logit_mod)
stargazer(logit_mod, type="latex")

# Global null hypothesis: coefficients for all predictor variables are equal to zero. 
# No values for either number of countries or level of sanctions for non-compliance are statistically differentiable from zero.

# Carry out likelihood ratio test

# Create null model
null_mod <- glm(choice ~ 1,
                data=climateSupport, 
                family=binomial(link="logit"))

# Run anova test comparing our model to the null model
anova(null_mod, logit_mod, test = "LRT")

# Calculate p-value manually, with lower.tail=FALSE to get P[X>x]  
# Initially did this without lower.tail=FALSE, instead with 1-pchisq() but got value of 0 (because p-value is too small?)...
p_val <- pchisq(logit_mod$null.deviance - logit_mod$deviance,
                logit_mod$df.null - logit_mod$df.residual,
                lower.tail=FALSE)

print(p_val)

# As the p-value is below our threshold of alpha=0.05, we can reject 
# the null hypothesis that no coefficients are statistically
# differentiable from zero. Our model provides a better explanation of 
# the variation in our outcome variable than the null model.

#####################
# Problem 2
#####################

# A)

# Convert log odds to odds
exp(-0.3251)

# Increasing the level of sanctions, on average, leads to a 0.325
# reduction in the log odds that an individual will support the policy.
# This equates to a reduction in the odds by a multiplicative factor 
# of 0.722. This would hold true for all levels of country participation 
# in the policy, as our model is additive.

# B) 

# Convert coefficients of interest from log odds to odds
odds <- exp(0.247+0.458-0.192)
odds

# Calculate probability from odds
prob <- odds/(1+odds)
round(prob,2)

# C)

# The answers might change as there would be different lines
# with potentially differing intercepts and slopes for each combination 
# of sanction level and number of participating countries. 

# Run logit model with interaction terms
logit_mod_inter <- glm(choice ~ countries*sanctions,
                 data=climateSupport,
                 family=binomial(link="logit"))

# Run anova test comparing first model to the interactive model
anova(logit_mod, logit_mod_inter, test = "LRT")
  
