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

lapply(c('stargazer', 'ggplot2', 'pscl', 'AER'),  pkgTest)

## Poisson

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

# Read in data
long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

long_data <- within(long_data, {
  fem <- as.logical(fem)
  mar <- as.logical(mar)
})

# Exploratory data analysis

str(long_data)
summary(long_data)

with(long_data,
     list(mean(art), var(art)))

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?

hist(long_data$art)
plot(jitter(long_data$art))

ggplot(long_data, aes(ment, art, color = fem)) +
  geom_jitter(alpha = 0.5)

ggplot(long_data, aes(phd, art, color = fem)) +
  geom_jitter(alpha = 0.5)

ggplot(long_data, aes(kid5, art, color = fem)) +
  geom_jitter(alpha = 0.5)

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# What conclusions would you draw from this analysis?

# Run OLS regression
mod.lm <- lm(art ~ ., data = long_data)
mod2.lm <- lm(art ~ fem*., data = long_data)

# Plot residuals and standard residuals
plot(predict(mod2.lm), abs(resid(mod2.lm)), xlab="Predicted", ylab="Absolute")

sresid <- rstandard(mod2.lm)
hist(sresid, main = "")

par(mfrow = c(2,2))
plot(mod2.lm)

# Run poisson regression
art_poisson <- glm(art ~ ., data=long_data, family=poisson)
summary(art_poisson)

## NB: ***If median residuals deviate (far) from zero, assumptions of poisson are violated***
##        I.e. - deviance does not equal mean

# Plot coefficients
stargazer(art_poisson, type="text")

# Interpreting outputs
cfs <- coef(art_poisson)

# Predicted no. of articles for a married male PhD researcher with 1 child at 2-
exp(cfs[1] + cfs[2]*0 + cfs[3]*5 + cfs[4]*2 + cfs[5]*1 + cfs[6]*1)

## *** Exponentiation of all coefficient terms times their x value give 
##     predicted value of count for that combination***
## *** NEED TO HAVE COMPLETE SET OF TERMS ACCOUNTED FOR FOR IT TO MAKE SENSE - 
##     otherwise it's just conditional mean of that terms contribution to the 
##     overall count (meaningless by itself)***
pred <- data.frame(fem = FALSE,
                 ment = 5,
                 phd = 2,
                 mar = TRUE,
                 kid5 = 1)

# Plot prediction vs. count
ggplot(data = NULL, aes(x = art_poisson$fitted.values, y = long_data$art)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = "blue")

# Calculate pseudo R squared
1 - (art_poisson$deviance/art_poisson$null.deviance)

# Calculate RMSE
sqrt(mean((art_poisson$model$art - art_poisson$fitted.values)^2))

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account

# Run zero inflated model
mod.zeroinfl <- zeroinfl(formula = art ~ ., data = long_data, dist = "poisson")
summary(mod.zeroinfl)

# Check for over-dispersion
dispersiontest(art_poisson)
